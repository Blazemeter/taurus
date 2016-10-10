"""
Module for reporting into http://www.blazemeter.com/ service

Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import copy
import json
import logging
import os
import platform
import sys
import time
import traceback
import zipfile
from abc import abstractmethod
from collections import defaultdict, OrderedDict
from functools import wraps
from ssl import SSLError

import yaml
from urwid import Pile, Text

from bzt import ManualShutdown
from bzt.engine import Reporter, Provisioning, ScenarioExecutor, Configuration, Service
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator, ResultsProvider, AggregatorListener
from bzt.modules.chrome import ChromeProfiler
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.monitoring import Monitoring, MonitoringListener
from bzt.modules.services import Unpacker
from bzt.six import BytesIO, text_type, iteritems, HTTPError, urlencode, Request, urlopen, r_input, URLError
from bzt.utils import open_browser, get_full_path, get_files_recursive, replace_in_config
from bzt.utils import to_json, dehumanize_time, MultiPartForm, BetterDict, ensure_is_dict


def send_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        if not isinstance(self, BlazeMeterUploader):
            raise ValueError("send_with_retry should only be applied to BlazeMeterUploader methods")

        try:
            method(self, *args, **kwargs)
        except IOError:
            self.log.debug("Error sending data: %s", traceback.format_exc())
            self.log.warning("Failed to send data, will retry in %s sec...", self.client.timeout)
            try:
                time.sleep(self.client.timeout)
                method(self, *args, **kwargs)
                self.log.info("Succeeded with retry")
            except IOError:
                self.log.error("Fatal error sending data: %s", traceback.format_exc())
                self.log.warning("Will skip failed data and continue running")

    return _impl


class BlazeMeterUploader(Reporter, AggregatorListener, MonitoringListener):
    """
    Reporter class

    :type client: BlazeMeterClient
    """

    def __init__(self):
        super(BlazeMeterUploader, self).__init__()
        self.browser_open = 'start'
        self.client = BlazeMeterClient(self.log)
        self.test_id = ""
        self.kpi_buffer = []
        self.send_interval = 30
        self.sess_name = None
        self._last_status_check = time.time()
        self.send_monitoring = True
        self.monitoring_buffer = None
        self.send_custom_metrics = False
        self.send_custom_tables = False

    def prepare(self):
        """
        Read options for uploading, check that they're sane
        """
        super(BlazeMeterUploader, self).prepare()
        self.client.logger_limit = self.settings.get("request-logging-limit", self.client.logger_limit)
        self.client.address = self.settings.get("address", self.client.address)
        self.client.data_address = self.settings.get("data-address", self.client.data_address)
        self.client.timeout = dehumanize_time(self.settings.get("timeout", self.client.timeout))
        self.send_interval = dehumanize_time(self.settings.get("send-interval", self.send_interval))
        self.send_monitoring = self.settings.get("send-monitoring", self.send_monitoring)
        self.send_custom_metrics = self.settings.get("send-custom-metrics", self.send_custom_metrics)
        self.send_custom_tables = self.settings.get("send-custom-tables", self.send_custom_tables)
        monitoring_buffer_limit = self.settings.get("monitoring-buffer-limit", 500)
        self.monitoring_buffer = MonitoringBuffer(monitoring_buffer_limit)
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        token = self.settings.get("token", "")
        if not token:
            self.log.warning("No BlazeMeter API key provided, will upload anonymously")
        self.client.token = token

        self.client.session_id = self.parameters.get("session-id", None)
        self.client.test_id = self.parameters.get("test-id", None)
        self.client.user_id = self.parameters.get("user-id", None)
        self.client.data_signature = self.parameters.get("signature", None)
        self.client.kpi_target = self.parameters.get("kpi-target", self.client.kpi_target)

        if not self.client.test_id:
            try:
                self.client.ping()  # to check connectivity and auth
            except HTTPError:
                self.log.error("Cannot reach online results storage, maybe the address/token is wrong")
                raise

            if token:
                finder = ProjectFinder(self.parameters, self.settings, self.client, self.log)
                self.test_id = finder.resolve_external_test()

        self.sess_name = self.parameters.get("report-name", self.settings.get("report-name", self.sess_name))
        if self.sess_name == 'ask' and sys.stdin.isatty():
            self.sess_name = r_input("Please enter report-name: ")

        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)

        for service in self.engine.services:
            if isinstance(service, Monitoring):
                service.add_listener(self)

    def startup(self):
        """
        Initiate online test
        """
        super(BlazeMeterUploader, self).startup()
        self.client.log = self.log.getChild(self.__class__.__name__)

        if not self.client.session_id:
            try:
                url = self.client.start_online(self.test_id, self.sess_name)
                self.log.info("Started data feeding: %s", url)
                if self.browser_open in ('start', 'both'):
                    open_browser(url)
            except KeyboardInterrupt:
                raise
            except BaseException as exc:
                self.log.debug("Exception: %s", traceback.format_exc())
                self.log.warning("Failed to start feeding: %s", exc)
                raise

    def __get_jtls_and_more(self):
        """
        Compress all files in artifacts dir to single zipfile
        :return: BytesIO
        """
        mfile = BytesIO()

        logs = set()
        for handler in self.engine.log.parent.handlers:
            if isinstance(handler, logging.FileHandler):
                logs.add(handler.baseFilename)

        max_file_size = self.settings.get('artifact-upload-size-limit', 10) * 1024 * 1024  # 10MB
        with zipfile.ZipFile(mfile, mode='w', compression=zipfile.ZIP_DEFLATED, allowZip64=True) as zfh:
            for root, _, files in os.walk(self.engine.artifacts_dir):
                for filename in files:
                    full_path = os.path.join(root, filename)
                    if full_path in logs:
                        logs.remove(full_path)

                    if os.path.getsize(full_path) <= max_file_size:
                        zfh.write(full_path, os.path.join(os.path.relpath(root, self.engine.artifacts_dir), filename))
                    else:
                        msg = "File %s exceeds maximum size quota of %s and won't be included into upload"
                        self.log.warning(msg, filename, max_file_size)

            for filename in logs:  # upload logs unconditionally
                zfh.write(filename, os.path.basename(filename))
        return mfile

    def __upload_artifacts(self):
        """
        If token provided, upload artifacts folder contents and bzt.log

        :return:
        """
        if self.client.token:
            worker_index = self.engine.config.get('modules').get('shellexec').get('env').get('TAURUS_INDEX_ALL', '')
            if worker_index:
                suffix = '-%s' % worker_index
            else:
                suffix = ''
            artifacts_zip = "artifacts%s.zip" % suffix
            mfile = self.__get_jtls_and_more()
            self.log.info("Uploading all artifacts as %s ...", artifacts_zip)
            self.client.upload_file(artifacts_zip, mfile.getvalue())

            for handler in self.engine.log.parent.handlers:
                if isinstance(handler, logging.FileHandler):
                    fname = handler.baseFilename
                    self.log.info("Uploading %s", fname)
                    fhead, ftail = os.path.splitext(os.path.split(fname)[-1])
                    modified_name = fhead + suffix + ftail
                    with open(fname) as _file:
                        self.client.upload_file(modified_name, _file.read())

    def post_process(self):
        """
        Upload results if possible
        """
        if not self.client.session_id:
            self.log.debug("No feeding session obtained, nothing to finalize")
            return

        self.log.debug("KPI bulk buffer len in post-proc: %s", len(self.kpi_buffer))
        try:
            self.__send_data(self.kpi_buffer, False, True)
            self.kpi_buffer = []
            if self.send_monitoring:
                self.__send_monitoring()
            if self.send_custom_metrics:
                self.__send_custom_metrics()
            if self.send_custom_tables:
                self.__send_custom_tables()
        finally:
            self._postproc_phase2()

        if self.client.results_url:
            if self.browser_open in ('end', 'both'):
                open_browser(self.client.results_url)
            self.log.info("Online report link: %s", self.client.results_url)

    def _postproc_phase2(self):
        try:
            self.__upload_artifacts()
        except IOError:
            self.log.warning("Failed artifact upload: %s", traceback.format_exc())
        finally:
            self.set_last_status_check(self.parameters.get('forced-last-check', self._last_status_check))
            tries = self.send_interval  # NOTE: you dirty one...
            while not self._last_status_check and tries > 0:
                self.log.info("Waiting for ping...")
                time.sleep(self.send_interval)
                tries -= 1

            self._postproc_phase3()

    def __append_note(self, obj, note):
        data = self.client.get(obj)
        if 'note' in data:
            note = data['note'] + '\n' + note
        note = note.strip()
        if note:
            self.client.update(obj, {'note': note})

    def _postproc_phase3(self):
        try:
            self.client.end_online()
            if self.engine.stopping_reason:
                exc_class = self.engine.stopping_reason.__class__.__name__
                note = "%s: %s" % (exc_class, str(self.engine.stopping_reason))
                self.client.append_note_to_session(note)
                self.client.append_note_to_master(note)

        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            self.log.warning("Failed to finish online: %s", exc)

    def check(self):
        """
        Send data if any in buffer

        :return:
        """
        self.log.debug("KPI bulk buffer len: %s", len(self.kpi_buffer))
        if len(self.kpi_buffer):
            if self.client.last_ts < (time.time() - self.send_interval):
                self.__send_data(self.kpi_buffer)
                if self.send_monitoring:
                    self.__send_monitoring()
                if self.send_custom_metrics:
                    self.__send_custom_metrics()
                self.kpi_buffer = []
        return super(BlazeMeterUploader, self).check()

    def __send_data(self, data, do_check=True, is_final=False):
        """
        :param data: list[bzt.modules.aggregator.DataPoint]
        :return:
        """
        if not self.client.session_id:
            return

        try:
            self.client.send_kpi_data(data, do_check, is_final)
        except IOError:
            self.log.debug("Error sending data: %s", traceback.format_exc())
            self.log.warning("Failed to send data, will retry in %s sec...", self.client.timeout)
            try:
                time.sleep(self.client.timeout)
                self.client.send_kpi_data(data, do_check, is_final)
                self.log.info("Succeeded with retry")
            except IOError:
                self.log.error("Fatal error sending data: %s", traceback.format_exc())
                self.log.warning("Will skip failed data and continue running")

    def aggregated_second(self, data):
        """
        Send online data
        :param data: DataPoint
        :return:
        """
        self.kpi_buffer.append(data)

    def set_last_status_check(self, value):
        self._last_status_check = value
        self.log.debug("Set last check time to: %s", self._last_status_check)

    def monitoring_data(self, data):
        if self.send_monitoring:
            self.monitoring_buffer.record_data(data)

    @send_with_retry
    def __send_monitoring(self):
        src_name = platform.node()
        data = self.monitoring_buffer.get_monitoring_json(self.client.session_id,
                                                          self.client.user_id,
                                                          self.client.test_id)
        self.client.send_monitoring_data(src_name, data)

    @send_with_retry
    def __send_custom_metrics(self):
        data = self.get_custom_metrics_json()
        self.client.send_custom_metrics(data)

    @send_with_retry
    def __send_custom_tables(self):
        data = self.get_custom_tables_json()
        if not data:
            return
        self.client.send_custom_tables(data)

    def get_custom_metrics_json(self):
        datapoints = {}

        for source, buff in iteritems(self.monitoring_buffer.data):
            for timestamp, item in iteritems(buff):
                if source == 'local':
                    source = platform.node()

                if timestamp not in datapoints:
                    datapoints[timestamp] = {}

                for field, value in iteritems(item):
                    if field in ('ts', 'interval'):
                        continue
                    if source == 'chrome':
                        if field.startswith("time"):
                            prefix = "Time"
                        elif field.startswith("network"):
                            prefix = "Network"
                        elif field.startswith("dom"):
                            prefix = "DOM"
                        elif field.startswith("js"):
                            prefix = "JS"
                        elif field.startswith("memory"):
                            prefix = "Memory"
                        else:
                            prefix = "Metrics"
                        field = self.get_chrome_metric_kpi_label(field)
                    else:
                        if field.lower().startswith('cpu'):
                            prefix = 'System'
                            field = 'CPU'
                        elif field.lower().startswith('mem'):
                            prefix = 'System'
                            field = 'Memory'
                            value *= 100
                        elif field.lower().startswith('disk'):
                            prefix = 'Disk'
                        elif field.lower().startswith('bytes-') or field.lower().startswith('net'):
                            prefix = 'Network'
                        else:
                            prefix = 'Monitoring'

                    label = "/".join([source, prefix, field])
                    datapoints[timestamp][label] = value

        results = []
        for timestamp in sorted(datapoints):
            datapoint = OrderedDict([(metric, datapoints[timestamp][metric])
                                     for metric in sorted(datapoints[timestamp])])
            datapoint["ts"] = timestamp
            results.append(datapoint)
        return {"datapoints": results}

    def get_chrome_metric_kpi_label(self, metric):
        for module in self.engine.services:
            if isinstance(module, ChromeProfiler):
                return module.get_metric_label(metric)
        return metric

    def get_custom_tables_json(self):
        for module in self.engine.services:
            if isinstance(module, ChromeProfiler):
                return module.get_custom_tables_json()


class MonitoringBuffer(object):
    def __init__(self, size_limit):
        self.size_limit = size_limit
        self.data = defaultdict(OrderedDict)
        # data :: dict(datasource -> dict(interval -> datapoint))
        # datapoint :: dict(metric -> value)

    def record_data(self, data):
        for item in data:
            source = item.pop('source')
            timestamp = int(item['ts'])
            item['interval'] = 1
            buff = self.data[source]
            if timestamp in buff:
                buff[timestamp].update(item)
            else:
                buff[timestamp] = item

        sources = list(self.data)
        for source in sources:
            if len(self.data[source]) > self.size_limit:
                self._downsample(self.data[source])

    def _downsample(self, buff):
        size = 1
        while len(buff) > self.size_limit:
            self._merge_small_intervals(buff, size)
            size += 1

    def _merge_small_intervals(self, buff, size):
        timestamps = list(buff)
        merged_already = set()
        for left, right in zip(timestamps, timestamps[1:]):
            if left in merged_already:
                continue
            if buff[left]['interval'] <= size:
                self._merge_datapoints(buff[left], buff[right])
                buff.pop(right)
                merged_already.add(left)
                merged_already.add(right)

    @staticmethod
    def _merge_datapoints(left, right):
        sum_size = float(left['interval'] + right['interval'])
        for metric in set(right):
            if metric in ('ts', 'interval'):
                continue
            if metric in left:
                left[metric] = (left[metric] * left['interval'] + right[metric] * right['interval']) / sum_size
            else:
                left[metric] = right[metric]
        left['interval'] = sum_size

    def get_monitoring_json(self, session_id, user_id, test_id):
        results = {}
        hosts = []
        kpis = {}

        for source, buff in iteritems(self.data):
            for timestamp, item in iteritems(buff):
                if source == 'local':
                    source = platform.node()

                if source not in results:
                    results[source] = {
                        "name": source,
                        "intervals": OrderedDict()
                    }

                if source not in hosts:
                    hosts.append(source)

                src = results[source]
                tstmp = timestamp * 1000
                tstmp_key = '%d' % tstmp

                if tstmp_key not in src['intervals']:
                    src['intervals'][tstmp_key] = {
                        "start": tstmp,
                        "duration": item['interval'] * 1000,
                        "indicators": {}
                    }

                for field, value in iteritems(item):
                    if field in ('ts', 'interval'):
                        continue
                    if field.lower().startswith('cpu'):
                        field = 'CPU'
                    elif field.lower().startswith('mem'):
                        field = 'Memory'
                        value *= 100
                    elif field == 'bytes-recv' or field.lower().startswith('net'):
                        field = 'Network I/O'
                    else:
                        continue  # maybe one day BZA will accept all other metrics...

                    if field not in kpis:
                        kpis[field] = field

                    src['intervals'][tstmp_key]['indicators'][field] = {
                        "value": value,
                        "name": field,
                        "std": 0,
                        "mean": 0,
                        "sum": 0,
                        "min": 0,
                        "max": 0,
                        "sumOfSquares": 0,
                        "n": 1
                    }

        kpis = {"Network I/O": "Network I/O", "Memory": "Memory", "CPU": "CPU"}
        return {
            "reportInfo": {
                "sessionId": session_id,
                "timestamp": time.time(),
                "userId": user_id,
                "testId": test_id,
                "type": "MONITOR",
                "testName": ""
            },
            "kpis": kpis,
            "hosts": hosts,
            "results": results
        }


class ProjectFinder(object):
    """
    :type client: BlazeMeterClient
    """

    TEST_TYPE_CLOUD = 'cloud-test'
    TEST_TYPE_COLLECTION = 'cloud-collection'

    def __init__(self, parameters, settings, client, parent_log):
        super(ProjectFinder, self).__init__()
        self.default_test_name = "Taurus Test"
        self.client = client
        self.parameters = parameters
        self.settings = settings
        self.log = parent_log.getChild(self.__class__.__name__)

    def _resolve_project(self):
        proj_name = self.parameters.get("project", self.settings.get("project", None))
        if isinstance(proj_name, (int, float)):
            proj_id = int(proj_name)
            self.log.debug("Treating project name as ID: %s", proj_id)
        elif proj_name is not None:
            proj_id = self.client.project_by_name(proj_name)
        else:
            proj_id = None
        return proj_id

    def resolve_external_test(self):
        project_id = self._resolve_project()
        test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))
        test_config = {"type": "external"}
        existing_test = self.client.find_external_test(test_name, project_id)
        if existing_test:
            test_id = existing_test['id']
        else:
            test_id = self.client.create_test(test_name, test_config, project_id)
        return test_id

    def resolve_test_type(self):
        project_id = self._resolve_project()

        test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))
        use_deprecated = self.settings.get("use-deprecated-api", True)
        default_location = self.settings.get("default-location", None)

        collection = self.client.find_collection(test_name, project_id)
        self.log.debug("Looking for collection: %s", collection)
        if collection:
            self.log.debug("Detected test type: new")
            test_class = CloudCollectionTest
            test_id = collection['id']
        else:
            test = self.client.find_test(test_name, project_id)
            self.log.debug("Looking for test: %s", test)
            if test:
                self.log.debug("Detected test type: old")
                test_class = CloudTaurusTest
                test_id = test['id']
            else:
                if use_deprecated:
                    self.log.debug("Will create old-style test")
                    test_class = CloudTaurusTest
                else:
                    self.log.debug("Will create new-style test")
                    test_class = CloudCollectionTest
                test_id = None

        return test_class(self.client, test_id, project_id, test_name, default_location, self.log)


class BaseCloudTest(object):
    """
    :type client: BlazeMeterClient
    """
    def __init__(self, client, test_id, project_id, test_name, default_location, parent_log):
        self.default_test_name = "Taurus Test"
        self.client = client
        self.log = parent_log.getChild(self.__class__.__name__)
        self.project_id = project_id
        self.test_name = test_name
        self.test_id = test_id
        self.default_location = default_location
        self._last_status = None
        self._sessions = None
        self._started = False

    @abstractmethod
    def prepare_locations(self, executors, engine_config):
        pass

    @abstractmethod
    def prepare_cloud_config(self, engine_config):
        pass

    @abstractmethod
    def resolve_test(self, taurus_config, rfiles):
        pass

    @abstractmethod
    def launch_test(self):
        """launch cloud test"""
        pass

    @abstractmethod
    def start_if_ready(self):
        """start cloud test if all engines are ready"""
        pass

    @abstractmethod
    def get_test_status_text(self):
        pass

    @abstractmethod
    def stop_test(self):
        pass

    def get_master_status(self):
        self._last_status = self.client.get_master_status()
        return self._last_status


class CloudTaurusTest(BaseCloudTest):
    def prepare_locations(self, executors, engine_config):
        available_locations = self.client.get_available_locations(include_harbors=False)

        if CloudProvisioning.LOC in engine_config:
            self.log.warning("Deprecated test API doesn't support global locations")

        for executor in executors:
            if CloudProvisioning.LOC in executor.execution:
                exec_locations = executor.execution[CloudProvisioning.LOC]
                self._check_locations(exec_locations, available_locations)
            else:
                default_loc = self._get_default_location(available_locations)
                executor.execution[CloudProvisioning.LOC] = BetterDict()
                executor.execution[CloudProvisioning.LOC].merge({default_loc: 1})

            executor.get_load()  # we need it to resolve load settings into full form

    def _get_default_location(self, available_locations):
        if self.default_location and self.default_location in available_locations:
            return self.default_location

        self.log.debug("Default location %s not found", self.default_location)

        for location_id in sorted(available_locations):
            location = available_locations[location_id]
            if not location_id.startswith('harbor-') and location['sandbox']:
                return location_id

        self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
        raise ValueError("No sandbox or default location available, please specify locations manually")

    def _check_locations(self, locations, available_locations):
        for location in locations:
            if location not in available_locations:
                self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                raise ValueError("Invalid location requested: %s" % location)

    def prepare_cloud_config(self, engine_config):
        config = copy.deepcopy(engine_config)

        if not isinstance(config[ScenarioExecutor.EXEC], list):
            config[ScenarioExecutor.EXEC] = [config[ScenarioExecutor.EXEC]]

        provisioning = config.pop(Provisioning.PROV)
        for execution in config[ScenarioExecutor.EXEC]:
            execution[ScenarioExecutor.CONCURR] = execution.get(ScenarioExecutor.CONCURR).get(provisioning, None)
            execution[ScenarioExecutor.THRPT] = execution.get(ScenarioExecutor.THRPT).get(provisioning, None)

        for key in list(config.keys()):
            fields = ("scenarios", ScenarioExecutor.EXEC, Service.SERV,
                      CloudProvisioning.LOC, CloudProvisioning.LOC_WEIGHTED)
            if key not in fields:
                config.pop(key)
            elif not config[key]:
                config.pop(key)

        # cleanup configuration from empty values
        default_values = {
            'concurrency': None,
            'iterations': None,
            'ramp-up': None,
            'steps': None,
            'throughput': None,
            'hold-for': 0,
            'files': []
        }
        for execution in config[ScenarioExecutor.EXEC]:
            for key, value in iteritems(default_values):
                if key in execution and execution[key] == value:
                    execution.pop(key)

        assert isinstance(config, Configuration)
        return config

    def resolve_test(self, taurus_config, rfiles):
        if self.test_id is None:
            test_config = {
                "type": "taurus",
                "plugins": {
                    "taurus": {
                        "filename": ""  # without this line it does not work
                    }
                }
            }
            self.test_id = self.client.create_test(self.test_name, test_config, self.project_id)
        self.client.setup_test(self.test_id, taurus_config, rfiles)

    def launch_test(self):
        return self.client.start_cloud_test(self.test_id)

    def start_if_ready(self):
        self._started = True

    def stop_test(self):
        self.client.end_master()

    def get_test_status_text(self):
        if not self._sessions:
            self._sessions = self.client.get_master_sessions()
            if not self._sessions:
                return

        mapping = BetterDict()  # dict(executor -> dict(scenario -> dict(location -> servers count)))
        for session in self._sessions:
            try:
                name_split = [part.strip() for part in session['name'].split('/')]
                location = session['configuration']['location']
                count = session['configuration']['serversCount']
                ex_item = mapping.get(name_split[0])
                if len(name_split) > 1:
                    script_item = ex_item.get(name_split[1])
                else:
                    script_item = ex_item.get("N/A", {})
                script_item[location] = count
            except KeyError:
                self._sessions = None

        txt = "%s #%s\n" % (self.test_name, self.client.master_id)
        for executor, scenarios in iteritems(mapping):
            txt += " %s" % executor
            for scenario, locations in iteritems(scenarios):
                txt += " %s:\n" % scenario
                for location, count in iteritems(locations):
                    txt += "  Agents in %s: %s\n" % (location, count)

        return txt


class CloudCollectionTest(BaseCloudTest):
    def prepare_locations(self, executors, engine_config):
        available_locations = self.client.get_available_locations(include_harbors=True)

        global_locations = engine_config.get(CloudProvisioning.LOC, BetterDict())
        self._check_locations(global_locations, available_locations)

        for executor in executors:
            if CloudProvisioning.LOC in executor.execution:
                exec_locations = executor.execution[CloudProvisioning.LOC]
                self._check_locations(exec_locations, available_locations)
            else:
                if not global_locations:
                    default_loc = self._get_default_location(available_locations)
                    executor.execution[CloudProvisioning.LOC] = BetterDict()
                    executor.execution[CloudProvisioning.LOC].merge({default_loc: 1})

            executor.get_load()  # we need it to resolve load settings into full form

        if global_locations and all(CloudProvisioning.LOC in executor.execution for executor in executors):
            self.log.warning("Each execution has locations specified, global locations won't have any effect")
            engine_config.pop(CloudProvisioning.LOC)

    def _get_default_location(self, available_locations):
        for location_id in sorted(available_locations):
            location = available_locations[location_id]
            if location['sandbox']:
                return location_id

        self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
        raise ValueError("No sandbox or default location available, please specify locations manually")

    def _check_locations(self, locations, available_locations):
        for location in locations:
            if location not in available_locations:
                self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                raise ValueError("Invalid location requested: %s" % location)

    def prepare_cloud_config(self, engine_config):
        config = copy.deepcopy(engine_config)

        if not isinstance(config[ScenarioExecutor.EXEC], list):
            config[ScenarioExecutor.EXEC] = [config[ScenarioExecutor.EXEC]]

        provisioning = config.pop(Provisioning.PROV)
        for execution in config[ScenarioExecutor.EXEC]:
            execution[ScenarioExecutor.CONCURR] = execution.get(ScenarioExecutor.CONCURR).get(provisioning, None)
            execution[ScenarioExecutor.THRPT] = execution.get(ScenarioExecutor.THRPT).get(provisioning, None)

        for key in list(config.keys()):
            fields = ("scenarios", ScenarioExecutor.EXEC, Service.SERV,
                      CloudProvisioning.LOC, CloudProvisioning.LOC_WEIGHTED)
            if key not in fields:
                config.pop(key)
            elif not config[key]:
                config.pop(key)

        # cleanup configuration from empty values
        default_values = {
            'concurrency': None,
            'iterations': None,
            'ramp-up': None,
            'steps': None,
            'throughput': None,
            'hold-for': 0,
            'files': []
        }
        for execution in config[ScenarioExecutor.EXEC]:
            for key, value in iteritems(default_values):
                if key in execution and execution[key] == value:
                    execution.pop(key)

        assert isinstance(config, Configuration)
        return config

    def resolve_test(self, taurus_config, rfiles):
        if self.test_id is None:
            self.log.debug("Creating cloud collection test")
            self.test_id = self.client.create_collection(self.test_name, taurus_config, rfiles, self.project_id)
        else:
            self.log.debug("Overriding cloud collection test")
            self.client.setup_collection(self.test_id, self.test_name, taurus_config, rfiles, self.project_id)

    def launch_test(self):
        return self.client.launch_cloud_collection(self.test_id)

    def start_if_ready(self):
        if self._started:
            return
        if self._last_status is None:
            return
        sessions = self._last_status.get("sessions", [])
        if sessions and all(session["status"] == "JMETER_CONSOLE_INIT" for session in sessions):
            self.client.force_start_master()
            self._started = True

    def await_test_end(self):
        iterations = 0
        while True:
            if iterations > 100:
                self.log.debug("Await: iteration limit reached")
                return
            status = self.client.get_master_status()
            if status.get("status") == "ENDED":
                return
            iterations += 1
            time.sleep(1.0)

    def stop_test(self):
        if self._started:
            self.client.stop_collection(self.test_id)
            self.await_test_end()
        else:
            self.client.end_master()

    def get_test_status_text(self):
        if not self._sessions:
            sessions = self.client.get_master_sessions()
            if not sessions:
                return
            self._sessions = {session["id"]: session for session in sessions}

        if not self._last_status:
            return

        mapping = BetterDict()  # dict(scenario -> dict(location -> servers count))
        for session_status in self._last_status["sessions"]:
            try:
                session_id = session_status["id"]
                session = self._sessions[session_id]
                location = session_status["locationId"]
                servers_count = len(session_status["readyStatus"]["servers"])
                name_split = [part.strip() for part in session['name'].split('/')]
                if len(name_split) > 1:
                    scenario = name_split[1]
                else:
                    scenario = "N/A"
                scenario_item = mapping.get(scenario)
                if location not in scenario_item:
                    scenario_item[location] = 0
                scenario_item[location] += servers_count
            except KeyError:
                self._sessions = None

        txt = "%s #%s\n" % (self.test_name, self.client.master_id)
        for scenario, locations in iteritems(mapping):
            txt += " %s:\n" % scenario
            for location, count in iteritems(locations):
                txt += "  Agents in %s: %s\n" % (location, count)

        return txt


class BlazeMeterClient(object):
    """ Service client class """

    def __init__(self, parent_logger):
        self.kpi_target = 'labels_bulk'
        self.logger_limit = 256
        self.user_id = None
        self.test_id = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.token = None
        self.address = "https://a.blazemeter.com"
        self.data_address = "https://data.blazemeter.com"
        self.results_url = None
        self.session_id = None
        self.master_id = None
        self.data_signature = None
        self.first_ts = sys.maxsize
        self.last_ts = 0
        self.timeout = 10
        self.delete_files_before_test = False

    def _request(self, url, data=None, headers=None, checker=None, method=None):
        if not headers:
            headers = {}
        if self.token:
            headers["X-Api-Key"] = self.token

        log_method = 'GET' if data is None else 'POST'
        if method:
            log_method = method

        url = str(url)
        self.log.debug("Request: %s %s %s", log_method, url, data[:self.logger_limit] if data else None)
        # .encode("utf-8") is probably better
        data = data.encode() if isinstance(data, text_type) else data
        req = Request(url, data, headers)
        if method:
            req.get_method = lambda: method

        response = urlopen(req, timeout=self.timeout)

        if checker:
            checker(response)

        resp = response.read()
        if not isinstance(resp, str):
            resp = resp.decode()

        self.log.debug("Response: %s", resp[:self.logger_limit] if resp else None)
        try:
            return json.loads(resp) if len(resp) else {}
        except ValueError:
            self.log.warning("Non-JSON response from API: %s", resp)
            raise

    def upload_collection_resources(self, resource_files, draft_id):
        url = self.address + "/api/latest/web/elfinder/%s" % draft_id
        body = MultiPartForm()
        body.add_field("cmd", "upload")
        body.add_field("target", "s1_Lw")
        body.add_field('folder', 'drafts')

        for rfile in resource_files:
            body.add_file('upload[]', rfile)

        hdr = {"Content-Type": str(body.get_content_type())}
        resp = self._request(url, body.form_as_bytes(), headers=hdr)
        if "error" in resp:
            raise ValueError("Can't upload resource files")

    def get_collections(self):
        resp = self._request(self.address + "/api/latest/collections")
        return resp['result']

    def import_config(self, config):
        url = self.address + "/api/latest/collections/taurusimport"
        resp = self._request(url, data=to_json(config), headers={"Content-Type": "application/json"}, method="POST")
        return resp['result']

    def update_collection(self, collection_id, coll):
        url = self.address + "/api/latest/collections/%s" % collection_id
        self._request(url, data=to_json(coll), headers={"Content-Type": "application/json"}, method="POST")

    def find_collection(self, collection_name, project_id):
        collections = self.get_collections()
        for collection in collections:
            self.log.debug("Collection: %s", collection)
            if "name" in collection and collection['name'] == collection_name:
                if not project_id or project_id == collection['projectId']:
                    self.log.debug("Matched: %s", collection)
                    return collection

    def find_test(self, test_name, project_id):
        tests = self.get_tests()
        for test in tests:
            self.log.debug("Test: %s", test)
            if "name" in test and test['name'] == test_name:
                if test['configuration']['type'] == "taurus":
                    if not project_id or project_id == test['projectId']:
                        self.log.debug("Matched: %s", test)
                        return test

    def find_external_test(self, test_name, project_id):
        """
        :rtype dict
        """
        tests = self.get_tests()
        for test in tests:
            self.log.debug("Test: %s", test)
            if "name" in test and test['name'] == test_name:
                if test['configuration']['type'] == "external":
                    if not project_id or project_id == test['projectId']:
                        self.log.debug("Matched: %s", test)
                        return test

    def start_online(self, test_id, session_name):
        """
        Start online test

        :type test_id: str
        :type session_name: str
        :return:
        """
        self.log.info("Initiating data feeding...")
        data = urlencode({})

        if self.token:
            url = self.address + "/api/latest/tests/%s/start-external" % test_id
        else:
            url = self.address + "/api/latest/sessions"

        resp = self._request(url, data)

        self.session_id = str(resp['result']['session']['id'])
        self.master_id = str(resp['result']['master']['id'])
        self.data_signature = str(resp['result']['signature'])
        self.test_id = test_id
        self.user_id = str(resp['result']['session']['userId'])
        if self.token:
            self.results_url = self.address + '/app/#reports/%s' % self.master_id
            if session_name:
                url = self.address + "/api/latest/sessions/%s" % self.session_id
                self._request(url, to_json({"name": str(session_name)}),
                              headers={"Content-Type": "application/json"}, method='PATCH')
        else:
            self.test_id = resp['result']['session']['testId']
            self.results_url = resp['result']['publicTokenUrl']
        return self.results_url

    def start_cloud_test(self, test_id):
        """
        Start online test

        :type test_id: str
        :return:
        """
        self.log.info("Initiating cloud test with %s ...", self.address)
        data = urlencode({})

        url = self.address + "/api/latest/tests/%s/start" % test_id

        resp = self._request(url, data)

        self.log.debug("Response: %s", resp['result'])
        self.master_id = str(resp['result']['id'])
        self.results_url = self.address + '/app/#reports/%s' % self.master_id
        return self.results_url

    def launch_cloud_collection(self, collection_id):
        self.log.info("Initiating cloud test with %s ...", self.address)
        # NOTE: delayedStart=true means that BM will not start test until all instances are ready
        # if omitted - instances will start once ready (not simultaneously),
        # which may cause inconsistent data in aggregate report.
        url = self.address + "/api/latest/collections/%s/start?delayedStart=true" % collection_id
        resp = self._request(url, method="POST")
        self.log.debug("Response: %s", resp['result'])
        self.master_id = resp['result']['id']
        self.results_url = self.address + '/app/#reports/%s' % self.master_id
        return self.results_url

    def force_start_master(self):
        self.log.info("All servers are ready, starting cloud test")
        url = self.address + "/api/latest/masters/%s/forceStart" % self.master_id
        self._request(url, method="POST")

    def stop_collection(self, collection_id):
        self.log.info("Shutting down cloud test...")
        url = self.address + "/api/latest/collections/%s/stop" % collection_id
        self._request(url)

    def end_online(self):
        """
        Finish online test
        """
        if not self.session_id:
            self.log.debug("Feeding not started, so not stopping")
        else:
            self.log.info("Ending data feeding...")
            if self.token:
                url = self.address + "/api/latest/sessions/%s/terminate"
                self._request(url % self.session_id)
            else:
                url = self.address + "/api/latest/sessions/%s/terminateExternal"
                data = {"signature": self.data_signature, "testId": self.test_id, "sessionId": self.session_id}
                self._request(url % self.session_id, json.dumps(data))

    def end_master(self):
        if self.master_id:
            self.log.info("Ending cloud test...")
            url = self.address + "/api/latest/masters/%s/terminate"
            self._request(url % self.master_id)

    def project_by_name(self, proj_name):
        """
        :type proj_name: str
        :rtype: int
        """
        projects = self.get_projects()
        matching = []
        for project in projects:
            if project['name'] == proj_name:
                matching.append(project['id'])

        if len(matching) > 1:
            self.log.warning("Several projects IDs matched with '%s': %s", proj_name, matching)
            raise ValueError("Project name is ambiguous, please use project ID instead of name to distinguish it")
        elif len(matching) == 1:
            return matching[0]
        else:
            self.log.info("Creating project '%s'...", proj_name)
            return self.create_project(proj_name)

    def append_note_to_master(self, note):
        data = self.get_master()
        if 'note' in data:
            note = data['note'] + '\n' + note
        note = note.strip()
        if note:
            self.update_master({'note': note})

    def append_note_to_session(self, note):
        data = self.get_session()
        if 'note' in data:
            note = data['note'] + '\n' + note
        note = note.strip()
        if note:
            self.update_session({'note': note})

    def create_collection(self, name, taurus_config, resource_files, proj_id):
        self.log.debug("Creating collection")
        if resource_files:
            draft_id = "taurus_%s" % int(time.time())
            self.upload_collection_resources(resource_files, draft_id)
            taurus_config.merge({"dataFiles": {"draftId": draft_id}})

        collection_draft = self.import_config(taurus_config)
        collection_draft['name'] = name

        self.log.debug("Creating new test collection: %s", name)
        collection_draft['name'] = name
        collection_draft['projectId'] = proj_id
        url = self.address + "/api/latest/collections"
        headers = {"Content-Type": "application/json"}
        resp = self._request(url, data=to_json(collection_draft), headers=headers, method="POST")
        collection_id = resp['result']['id']

        self.log.debug("Using collection ID: %s", collection_id)
        return collection_id

    def setup_collection(self, collection_id, name, taurus_config, resource_files, proj_id):
        self.log.debug("Setting up collection")
        if resource_files:
            draft_id = "taurus_%s" % int(time.time())
            self.upload_collection_resources(resource_files, draft_id)
            taurus_config.merge({"dataFiles": {"draftId": draft_id}})

        collection_draft = self.import_config(taurus_config)
        collection_draft['name'] = name
        collection_draft['projectId'] = proj_id

        self.update_collection(collection_id, collection_draft)

    def create_test(self, name, configuration, proj_id):
        self.log.debug("Creating new test")
        url = self.address + '/api/latest/tests'
        data = {"name": name, "projectId": proj_id, "configuration": configuration}
        hdr = {"Content-Type": " application/json"}
        resp = self._request(url, json.dumps(data), headers=hdr)
        test_id = resp['result']['id']
        self.log.debug("Using test ID: %s", test_id)
        return test_id

    def setup_test(self, test_id, taurus_config, resource_files):
        self.log.debug("Setting up test")
        if self.delete_files_before_test:
            self.delete_test_files(test_id)

        self.log.debug("Uploading files into the test: %s", resource_files)
        url = '%s/api/latest/tests/%s/files' % (self.address, test_id)

        body = MultiPartForm()
        body.add_file_as_string('script', 'taurus.yml', yaml.dump(taurus_config, default_flow_style=False,
                                                                  explicit_start=True, canonical=False))

        for rfile in resource_files:
            body.add_file('files[]', rfile)

        hdr = {"Content-Type": str(body.get_content_type())}
        _ = self._request(url, body.form_as_bytes(), headers=hdr)

    def get_tests(self):
        """
        :rtype: list[dict]
        """
        tests = self._request(self.address + '/api/latest/tests')
        self.log.debug("Tests for user: %s", len(tests['result']))
        return tests['result']

    def __get_kpi_body(self, data_buffer, is_final):
        report_items = BetterDict()
        for dpoint in data_buffer:
            self.first_ts = min(self.first_ts, dpoint[DataPoint.TIMESTAMP])
            self.last_ts = max(self.last_ts, dpoint[DataPoint.TIMESTAMP])

            for label, kpi_set in iteritems(dpoint[DataPoint.CURRENT]):
                report_item = report_items.get(label, self.__label_skel(label))

                interval_item = self.__interval_json(kpi_set, dpoint)
                for r_code, cnt in iteritems(kpi_set[KPISet.RESP_CODES]):
                    fails = [err['cnt'] for err in kpi_set[KPISet.ERRORS] if str(err['rc']) == r_code]
                    interval_item['rc'].append({"n": cnt, 'f': fails, "rc": r_code})

                report_item['intervals'].append(interval_item)

                cumul = dpoint[DataPoint.CUMULATIVE][label]
                report_item['n'] = cumul[KPISet.SAMPLE_COUNT]
                report_item["summary"] = self.__summary_json(cumul)

                self.__add_errors(report_item, kpi_set)

        report_items = [report_items[key] for key in sorted(report_items.keys())]  # convert dict to list
        data = {"labels": report_items, "sourceID": id(self)}

        if is_final:
            data['final'] = True

        return to_json(data)

    @staticmethod
    def __add_errors(report_item, kpi_set):
        errors = kpi_set[KPISet.ERRORS]
        for error in errors:
            if error["type"] == KPISet.ERRTYPE_ERROR:
                report_item['errors'].append({
                    'm': error['msg'],
                    "rc": error['rc'],
                    "count": error['cnt'],
                })
            else:
                report_item['assertions'].append({
                    'failureMessage': error['msg'],
                    'name': 'All Assertions',
                    'failures': error['cnt']
                    # TODO: "count", "errors" = ? (according do Udi's format description)
                    # TODO: Errtype == embedded_resources ?
                })

    def send_kpi_data(self, data_buffer, is_check_response=True, is_final=False):
        """
        Sends online data

        :param is_check_response:
        :param is_final:
        :type data_buffer: list[bzt.modules.aggregator.DataPoint]
        """
        url = self.data_address + "/submit.php?session_id=%s&signature=%s&test_id=%s&user_id=%s"
        url = url % (self.session_id, self.data_signature, self.test_id, self.user_id)
        url += "&pq=0&target=%s&update=1" % self.kpi_target
        hdr = {"Content-Type": " application/json"}
        response = self._request(url, self.__get_kpi_body(data_buffer, is_final), headers=hdr)

        if response and 'response_code' in response and response['response_code'] != 200:
            raise RuntimeError("Failed to feed data, response code %s" % response['response_code'])

        if response and 'result' in response and is_check_response:
            result = response['result']['session']
            self.log.debug("Result: %s", result)
            if 'statusCode' in result and result['statusCode'] > 100:
                self.log.info("Test was stopped through Web UI: %s", result['status'])
                raise ManualShutdown("The test was interrupted through Web UI")

    def __label_skel(self, name):
        return {
            "n": None,
            "name": name if name else 'ALL',
            "interval": 1,
            "intervals": [],
            "samplesNotCounted": 0,
            "assertionsNotCounted": 0,
            "failedEmbeddedResources": [],
            "failedEmbeddedResourcesSpilloverCount": 0,
            "otherErrorsCount": 0,
            "errors": [],
            "assertions": [],
            "percentileHistogram": [],
            "percentileHistogramLatency": [],
            "percentileHistogramBytes": [],
            "empty": False,
        }

    def __summary_json(self, cumul):
        return {
            "first": self.first_ts,
            "last": self.last_ts,
            "duration": self.last_ts - self.first_ts,
            "failed": cumul[KPISet.FAILURES],
            "hits": cumul[KPISet.SAMPLE_COUNT],

            "avg": int(1000 * cumul[KPISet.AVG_RESP_TIME]),
            "min": int(1000 * cumul[KPISet.PERCENTILES]["0.0"]) if "0.0" in cumul[KPISet.PERCENTILES] else 0,
            "max": int(1000 * cumul[KPISet.PERCENTILES]["100.0"]) if "100.0" in cumul[KPISet.PERCENTILES] else 0,
            "std": int(1000 * cumul[KPISet.STDEV_RESP_TIME]),
            "tp90": int(1000 * cumul[KPISet.PERCENTILES]["90.0"]) if "90.0" in cumul[KPISet.PERCENTILES] else 0,
            "tp95": int(1000 * cumul[KPISet.PERCENTILES]["95.0"]) if "95.0" in cumul[KPISet.PERCENTILES] else 0,
            "tp99": int(1000 * cumul[KPISet.PERCENTILES]["99.0"]) if "99.0" in cumul[KPISet.PERCENTILES] else 0,

            "latencyAvg": int(1000 * cumul[KPISet.AVG_LATENCY]),
            "latencyMax": 0,
            "latencyMin": 0,
            "latencySTD": 0,

            "bytes": cumul[KPISet.BYTE_COUNT],
            "bytesMax": 0,
            "bytesMin": 0,
            "bytesAvg": int(cumul[KPISet.BYTE_COUNT] / float(cumul[KPISet.SAMPLE_COUNT])),
            "bytesSTD": 0,

            "otherErrorsSpillcount": 0,
        }

    def __interval_json(self, item, sec):
        return {
            "ec": item[KPISet.FAILURES],
            "ts": sec[DataPoint.TIMESTAMP],
            "na": item[KPISet.CONCURRENCY],
            "n": item[KPISet.SAMPLE_COUNT],
            "failed": item[KPISet.FAILURES],
            "rc": [],  # filled later
            "t": {
                "min": int(1000 * item[KPISet.PERCENTILES]["0.0"]) if "0.0" in item[KPISet.PERCENTILES] else 0,
                "max": int(1000 * item[KPISet.PERCENTILES]["100.0"]) if "100.0" in item[KPISet.PERCENTILES] else 0,
                "sum": 1000 * item[KPISet.AVG_RESP_TIME] * item[KPISet.SAMPLE_COUNT],
                "n": item[KPISet.SAMPLE_COUNT],
                "std": 1000 * item[KPISet.STDEV_RESP_TIME],
                "avg": 1000 * item[KPISet.AVG_RESP_TIME]
            },
            "lt": {
                "min": 0,
                "max": 0,
                "sum": 1000 * item[KPISet.AVG_LATENCY] * item[KPISet.SAMPLE_COUNT],
                "n": 1000 * item[KPISet.SAMPLE_COUNT],
                "std": 0,
                "avg": 1000 * item[KPISet.AVG_LATENCY]
            },
            "by": {
                "min": 0,
                "max": 0,
                "sum": item[KPISet.BYTE_COUNT],
                "n": item[KPISet.SAMPLE_COUNT],
                "std": 0,
                "avg": item[KPISet.BYTE_COUNT] / float(item[KPISet.SAMPLE_COUNT])
            },
        }

    def ping(self):
        """
        Quick check if we can access the service
        """
        self._request(self.address + '/api/latest/web/version')

    def upload_file(self, filename, contents=None):
        """
        Upload single artifact

        :type filename: str
        :type contents: str
        :raise IOError:
        """
        body = MultiPartForm()

        if contents is None:
            body.add_file('file', filename)
        else:
            body.add_file_as_string('file', filename, contents)

        url = self.address + "/api/latest/image/%s/files?signature=%s"
        url = url % (self.session_id, self.data_signature)
        hdr = {"Content-Type": str(body.get_content_type())}
        response = self._request(url, body.form_as_bytes(), headers=hdr)
        if not response['result']:
            raise IOError("Upload failed: %s" % response)

    def get_master(self):
        req = self._request(self.address + '/api/latest/masters/%s' % self.master_id)
        return req['result']

    def get_session(self):
        req = self._request(self.address + '/api/latest/sessions/%s' % self.session_id)
        return req['result']

    def update_master(self, data):
        hdr = {"Content-Type": "application/json"}
        req = self._request(self.address + '/api/latest/masters/%s' % self.master_id,
                            to_json(data), headers=hdr, method="PUT")
        return req['result']

    def update_session(self, data):
        hdr = {"Content-Type": "application/json"}
        req = self._request(self.address + '/api/latest/sessions/%s' % self.session_id,
                            to_json(data), headers=hdr, method="PUT")
        return req['result']

    def get_master_status(self):
        sess = self._request(self.address + '/api/latest/masters/%s/status' % self.master_id)
        return sess['result']

    def get_master_sessions(self):
        sess = self._request(self.address + '/api/latest/masters/%s/sessions' % self.master_id)
        if 'sessions' in sess['result']:
            return sess['result']['sessions']
        else:
            return sess['result']

    def get_projects(self):
        data = self._request(self.address + '/api/latest/projects')
        return data['result']

    def create_project(self, proj_name):
        hdr = {"Content-Type": "application/json"}
        data = self._request(self.address + '/api/latest/projects', to_json({"name": str(proj_name)}), headers=hdr)
        return data['result']['id']

    def get_user_info(self):
        res = self._request(self.address + '/api/latest/user')
        return res

    def get_kpis(self, master_id, min_ts):
        params = [
            ("interval", 1),
            ("from", min_ts),
            ("master_ids[]", master_id),
        ]
        for item in ('t', 'lt', 'by', 'n', 'ec', 'ts', 'na'):
            params.append(("kpis[]", item))

        labels = self.get_labels(master_id)
        for label in labels:
            params.append(("labels[]", label['id']))

        url = self.address + "/api/latest/data/kpis?" + urlencode(params)
        res = self._request(url)
        return res['result']

    def get_labels(self, master_id):
        url = self.address + "/api/latest/data/labels?" + urlencode({'master_id': master_id})
        res = self._request(url)
        return res['result']

    def get_available_locations(self, include_harbors=False):
        user_info = self.get_user_info()
        locations = {}
        for loc in user_info['locations']:
            loc_id = str(loc['id'])
            if loc_id.startswith('harbor-') and not include_harbors:
                continue
            locations[str(loc['id'])] = loc
        return locations

    def get_test_files(self, test_id):
        path = self.address + "/api/latest/web/elfinder/%s" % test_id
        query = urlencode({'cmd': 'open', 'target': 's1_Lw'})
        url = path + '?' + query
        response = self._request(url)
        return response["files"]

    def delete_test_files(self, test_id):
        files = self.get_test_files(test_id)
        self.log.debug("Test files: %s", [filedict['name'] for filedict in files])
        if not files:
            return
        path = "/api/latest/web/elfinder/%s" % test_id
        query = "cmd=rm&" + "&".join("targets[]=%s" % fname['hash'] for fname in files)
        url = self.address + path + '?' + query
        response = self._request(url)
        if len(response['removed']) == len(files):
            self.log.debug("Successfully deleted %d test files", len(response['removed']))

    def get_aggregate_report(self, master_id):
        url = self.address + "/api/latest/masters/%s/reports/aggregatereport/data" % master_id
        res = self._request(url)
        return res['result']

    def send_monitoring_data(self, src_name, data):
        self.upload_file('%s.monitoring.json' % src_name, to_json(data))

    def send_custom_metrics(self, data):
        url = self.address + "/api/latest/data/masters/%s/custom-metrics" % self.master_id
        res = self._request(url, to_json(data), headers={"Content-Type": "application/json"}, method="POST")
        return res

    def send_custom_tables(self, data):
        url = self.address + "/api/latest/data/masters/%s/custom-table" % self.master_id
        res = self._request(url, to_json(data), headers={"Content-Type": "application/json"}, method="POST")
        return res


class MasterProvisioning(Provisioning):
    def get_rfiles(self):
        rfiles = []
        for executor in self.executors:
            rfiles += executor.get_resource_files()

        self.log.debug("All resource files are: %s", rfiles)
        rfiles = [self.engine.find_file(x) for x in rfiles]

        rbases = [os.path.basename(get_full_path(rfile)) for rfile in rfiles]
        rpaths = [get_full_path(rfile, step_up=1) for rfile in rfiles]
        while rbases:
            base, path = rbases.pop(), rpaths.pop()
            if base in rbases:
                index = rbases.index(base)
                if path != rpaths[index]:
                    message = 'Resource "%s" occurs more than one time, rename to avoid data loss' % base
                    raise ValueError(message)

        prepared_files = self.__pack_dirs(rfiles)
        replace_in_config(self.engine.config, rfiles, [os.path.basename(f) for f in prepared_files], log=self.log)

        return prepared_files

    def __pack_dirs(self, source_list):
        result_list = []  # files for upload
        packed_list = []  # files for unpacking

        for source in source_list:
            source = get_full_path(source)
            if os.path.isfile(source):
                result_list.append(source)
            else:  # source is dir
                self.log.debug("Compress directory '%s'", source)
                base_dir_name = os.path.basename(source)
                zip_name = self.engine.create_artifact(base_dir_name, '.zip')
                relative_prefix_len = len(os.path.dirname(source))
                with zipfile.ZipFile(zip_name, 'w') as zip_file:
                    for _file in get_files_recursive(source):
                        zip_file.write(_file, _file[relative_prefix_len:])
                result_list.append(zip_name)
                packed_list.append(base_dir_name + '.zip')

        if packed_list:
            services = self.engine.config.get(Service.SERV, [])
            services.append({'module': Unpacker.UNPACK, Unpacker.FILES: packed_list, 'run-at': 'local'})

        return result_list


class CloudProvisioning(MasterProvisioning, WidgetProvider):
    """
    :type client: BlazeMeterClient
    :type results_reader: ResultsFromBZA
    :type test: BaseCloudTest
    """

    LOC = "locations"
    LOC_WEIGHTED = "locations-weighted"

    def __init__(self):
        super(CloudProvisioning, self).__init__()
        self.results_reader = None
        self.client = BlazeMeterClient(self.log)
        self.__last_master_status = None
        self.browser_open = 'start'
        self.widget = None
        self.detach = False
        self.test = None
        self.test_ended = False
        self.check_interval = 5.0
        self.__last_check_time = None

    def _merge_with_blazemeter_config(self):
        if 'blazemeter' not in self.engine.config.get('modules'):
            self.log.debug("Module 'blazemeter' wasn't found in base config")
            return
        bm_mod = self.engine.instantiate_module('blazemeter')
        bm_settings = copy.deepcopy(bm_mod.settings)
        bm_settings.update(self.settings)
        self.settings = bm_settings

    def prepare(self):
        self._merge_with_blazemeter_config()
        if self.settings.get("dump-locations", False):
            self.log.warning("Dumping available locations instead of running the test")
            self._configure_client()
            use_deprecated = self.settings.get("use-deprecated-api", True)
            locations = self.client.get_available_locations(include_harbors=not use_deprecated)
            for location_id in sorted(locations):
                location = locations[location_id]
                self.log.info("Location: %s\t%s", location_id, location['title'])
            raise ManualShutdown("Done listing locations")

        super(CloudProvisioning, self).prepare()
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        self.detach = self.settings.get("detach", self.detach)
        self.check_interval = dehumanize_time(self.settings.get("check-interval", self.check_interval))
        self._configure_client()
        self._filter_reporting()

        finder = ProjectFinder(self.parameters, self.settings, self.client, self.log)
        finder.default_test_name = "Taurus Cloud Test"
        self.test = finder.resolve_test_type()
        self.test.prepare_locations(self.executors, self.engine.config)
        config = self.test.prepare_cloud_config(self.engine.config)
        config.dump(self.engine.create_artifact("cloud", ""))

        self.test.resolve_test(config, self.get_rfiles())

        self.widget = CloudProvWidget(self.test)

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.results_reader = ResultsFromBZA(self.client)
            self.results_reader.log = self.log
            self.engine.aggregator.add_underling(self.results_reader)

    def _filter_reporting(self):
        reporting = self.engine.config.get(Reporter.REP, [])
        new_reporting = []
        for index, reporter in enumerate(reporting):
            reporter = ensure_is_dict(reporting, index, "module")
            cls = reporter.get('module', ValueError())
            if cls == 'blazemeter':
                self.log.warning("Explicit blazemeter reporting is skipped for cloud")
            else:
                new_reporting.append(reporter)
        self.engine.config[Reporter.REP] = new_reporting

    def _configure_client(self):
        self.client.logger_limit = self.settings.get("request-logging-limit", self.client.logger_limit)
        self.client.address = self.settings.get("address", self.client.address)
        self.client.token = self.settings.get("token", self.client.token)
        self.client.timeout = dehumanize_time(self.settings.get("timeout", self.client.timeout))
        self.client.delete_files_before_test = self.settings.get("delete-test-files", True)
        if not self.client.token:
            raise ValueError("You must provide API token to use cloud provisioning")

    def startup(self):
        super(CloudProvisioning, self).startup()
        self.test.launch_test()
        self.log.info("Started cloud test: %s", self.client.results_url)
        if self.client.results_url:
            if self.browser_open in ('start', 'both'):
                open_browser(self.client.results_url)

    def _should_skip_check(self):
        now = time.time()
        if self.__last_check_time is None:
            return False
        elif now >= self.__last_check_time + self.check_interval:
            return False
        else:
            return True

    def check(self):
        if self.detach:
            self.log.warning('Detaching Taurus from started test...')
            return True

        if self._should_skip_check():
            self.log.debug("Skipping cloud status check")
            return False

        self.__last_check_time = time.time()

        try:
            master = self.test.get_master_status()
        except (URLError, SSLError):
            self.log.warning("Failed to get test status, will retry in %s seconds...", self.client.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.client.timeout)
            master = self.test.get_master_status()
            self.log.info("Succeeded with retry")

        if "status" in master and master['status'] != self.__last_master_status:
            self.__last_master_status = master['status']
            self.log.info("Cloud test status: %s", self.__last_master_status)

        if self.results_reader is not None and 'progress' in master and master['progress'] >= 100:
            self.results_reader.master_id = self.client.master_id

        if 'progress' in master and master['progress'] > 100:
            self.log.info("Test was stopped in the cloud: %s", master['status'])
            status = self.client.get_master()
            if 'note' in status and status['note']:
                self.log.warning("Cloud test has probably failed with message: %s", status['note'])

            self.client.master_id = None
            self.test_ended = True
            return True

        self.test.start_if_ready()

        self.widget.update()
        return super(CloudProvisioning, self).check()

    def post_process(self):
        if not self.detach and self.test and not self.test_ended:
            self.test.stop_test()
        if self.client.results_url:
            if self.browser_open in ('end', 'both'):
                open_browser(self.client.results_url)

    def get_widget(self):
        if not self.widget:
            self.widget = CloudProvWidget(self.test)
        return self.widget


class BlazeMeterClientEmul(BlazeMeterClient):
    def __init__(self, parent_logger):
        super(BlazeMeterClientEmul, self).__init__(parent_logger)
        self.requests = []
        self.results = []
        self.requests = []

    def _request(self, url, data=None, headers=None, checker=None, method=None):
        self.log.debug("Request %s: %s", url, data)
        self.requests.append({"url": url, "data": data, "headers": headers, "checker": checker, "method": method})
        res = self.results.pop(0)
        self.log.debug("Response: %s", res)
        if isinstance(res, BaseException):
            raise res
        return res


class ResultsFromBZA(ResultsProvider):
    """
    :type client: BlazeMeterClient
    """

    def __init__(self, client):
        super(ResultsFromBZA, self).__init__()
        self.client = client
        self.master_id = None  # must be set afterwards
        self.min_ts = 0
        self.log = logging.getLogger('')

    def _calculate_datapoints(self, final_pass=False):
        if self.master_id is None:
            return

        data, aggr_raw = self.query_data()
        aggr = {}
        for label in aggr_raw:
            aggr[label['labelName']] = label

        for label in data:
            if label['kpis'] and not final_pass:
                label['kpis'].pop(-1)  # never take last second since it could be incomplete

        timestamps = []
        for label in data:
            if label['label'] == 'ALL':
                timestamps.extend([kpi['ts'] for kpi in label['kpis']])

        for tstmp in timestamps:
            point = DataPoint(tstmp)
            for label in data:
                for kpi in label['kpis']:
                    if kpi['ts'] != tstmp:
                        continue

                    label_str = label['label']
                    if label_str not in aggr:
                        self.log.warning("Skipping inconsistent data from API for label: %s", label_str)
                        continue

                    kpiset = self.__get_kpiset(aggr, kpi, label_str)
                    point[DataPoint.CURRENT]['' if label_str == 'ALL' else label_str] = kpiset

            point.recalculate()
            self.min_ts = point[DataPoint.TIMESTAMP] + 1
            yield point

    def __get_kpiset(self, aggr, kpi, label):
        kpiset = KPISet()
        kpiset[KPISet.FAILURES] = kpi['ec']
        kpiset[KPISet.CONCURRENCY] = kpi['na']
        kpiset[KPISet.SAMPLE_COUNT] = kpi['n']
        kpiset.sum_rt += kpi['t_avg'] * kpi['n'] / 1000.0
        kpiset.sum_lt += kpi['lt_avg'] * kpi['n'] / 1000.0
        perc_map = {'90line': 90.0, "95line": 95.0, "99line": 99.0}
        for field, level in iteritems(perc_map):
            kpiset[KPISet.PERCENTILES][str(level)] = aggr[label][field] / 1000.0
        return kpiset

    def query_data(self):
        try:
            data = self.client.get_kpis(self.master_id, self.min_ts)
        except URLError:
            self.log.warning("Failed to get result KPIs, will retry in %s seconds...", self.client.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.client.timeout)
            data = self.client.get_kpis(self.master_id, self.min_ts)
            self.log.info("Succeeded with retry")

        try:
            aggr = self.client.get_aggregate_report(self.master_id)
        except URLError:
            self.log.warning("Failed to get aggregate results, will retry in %s seconds...", self.client.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.client.timeout)
            aggr = self.client.get_aggregate_report(self.master_id)
            self.log.info("Succeeded with retry")

        return data, aggr


class CloudProvWidget(Pile, PrioritizedWidget):
    def __init__(self, test):
        """
        :type test: BaseCloudTest
        """
        self.test = test
        self.text = Text("")
        super(CloudProvWidget, self).__init__([self.text])
        PrioritizedWidget.__init__(self)

    def update(self):
        txt = self.test.get_test_status_text()
        if txt:
            self.text.set_text(txt)


class ServiceStubScreenshoter(Service):
    def startup(self):
        if not isinstance(self.engine.provisioning, CloudProvisioning):
            self.log.warning("Stub for service 'screenshoter', use cloud provisioning to have it working")


class ServiceStubCaptureHAR(Service):
    def startup(self):
        if not isinstance(self.engine.provisioning, CloudProvisioning):
            self.log.warning("Stub for service 'capturehar', use cloud provisioning to have it working")
