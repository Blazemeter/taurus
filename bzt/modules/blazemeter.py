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
import math
import os
import sys
import time
import traceback
import zipfile
import platform
from collections import OrderedDict

import yaml
from urwid import Pile, Text

from bzt import ManualShutdown
from bzt.engine import Reporter, Provisioning, ScenarioExecutor, Configuration, Service
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator, ResultsProvider, AggregatorListener
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.jmeter import JMeterExecutor
from bzt.modules.monitoring import Monitoring, MonitoringListener
from bzt.modules.services import Unpacker
from bzt.six import BytesIO, text_type, iteritems, HTTPError, urlencode, Request, urlopen, r_input, URLError
from bzt.utils import open_browser, get_full_path, get_files_recursive, replace_in_config
from bzt.utils import to_json, dehumanize_time, MultiPartForm, BetterDict, ensure_is_dict


class BlazeMeterUploader(Reporter, AggregatorListener, MonitoringListener):
    """
    Reporter class

    :type client: BlazeMeterClient
    """

    def __init__(self):
        super(BlazeMeterUploader, self).__init__()
        self.monitoring = []
        self.send_monitoring = True
        self.browser_open = 'start'
        self.client = BlazeMeterClient(self.log)
        self.test_id = ""
        self.kpi_buffer = []
        self.send_interval = 30
        self.sess_name = None
        self._last_status_check = time.time()

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
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        token = self.settings.get("token", "")
        if not token:
            self.log.warning("No BlazeMeter API key provided, will upload anonymously")
        self.client.token = token

        self.client.active_session_id = self.parameters.get("session-id", None)
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
                finder = ProjectFinder(self.parameters, self.settings, self.client, self.engine)
                self.test_id = finder.resolve_test_id({"type": "external"}, self.engine.config, [])

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

        if not self.client.active_session_id:
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
        max_file_size = self.settings.get('artifact-upload-size-limit', 10) * 1024 * 1024  # 10MB
        with zipfile.ZipFile(mfile, mode='w', compression=zipfile.ZIP_DEFLATED, allowZip64=True) as zfh:
            for handler in self.engine.log.parent.handlers:
                if isinstance(handler, logging.FileHandler):
                    zfh.write(handler.baseFilename, os.path.basename(handler.baseFilename))

            for root, _, files in os.walk(self.engine.artifacts_dir):
                for filename in files:
                    if os.path.getsize(os.path.join(root, filename)) <= max_file_size:
                        zfh.write(os.path.join(root, filename),
                                  os.path.join(os.path.relpath(root, self.engine.artifacts_dir), filename))
                    else:
                        msg = "File %s exceeds maximum size quota of %s and won't be included into upload"
                        self.log.warning(msg, filename, max_file_size)
        return mfile

    def __upload_artifacts(self):
        """
        If token provided, upload artifacts folder contents and jmeter_log
        else: jmeter_log only
        :return:
        """
        if self.client.token:
            self.log.info("Uploading all artifacts as jtls_and_more.zip ...")
            mfile = self.__get_jtls_and_more()
            self.client.upload_file("jtls_and_more.zip", mfile.getvalue())

        for executor in self.engine.provisioning.executors:
            if isinstance(executor, JMeterExecutor):
                if executor.jmeter_log:
                    self.log.info("Uploading %s", executor.jmeter_log)
                    self.client.upload_file(executor.jmeter_log)

    def post_process(self):
        """
        Upload results if possible
        """
        if not self.client.active_session_id:
            self.log.debug("No feeding session obtained, nothing to finalize")
            return

        self.log.debug("KPI bulk buffer len in post-proc: %s", len(self.kpi_buffer))
        try:
            self.__send_data(self.kpi_buffer, False, True)
            self.kpi_buffer = []
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

    def _postproc_phase3(self):
        try:
            self.client.end_online()
            if self.engine.stopping_reason:
                note = "%s: %s" % (self.engine.stopping_reason.__class__.__name__, str(self.engine.stopping_reason))
                sess = self.client.get_session(self.client.active_session_id)
                if 'note' in sess:
                    note += "\n" + sess['note']

                if note.strip():
                    self.client.update_session(self.client.active_session_id, {"note": note.strip()})
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
                self.kpi_buffer = []
        return super(BlazeMeterUploader, self).check()

    def __send_data(self, data, do_check=True, is_final=False):
        """
        :param data: list[bzt.modules.aggregator.DataPoint]
        :return:
        """
        if not self.client.active_session_id:
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

        if not data:
            return

        try:
            self.client.send_error_summary(data)
        except IOError as exc:
            self.log.debug("Failed sending error summary: %s", traceback.format_exc())
            self.log.warning("Failed to send error summary: %s", exc)

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
            self.monitoring.extend(data)

    def __send_monitoring(self):
        # self.log.info("Mon: %s", self.monitoring)
        src_name = platform.node()
        data = self.get_monitoring_json()
        try:
            self.client.send_monitoring_data(src_name, data)
        except IOError:
            self.log.debug("Error sending data: %s", traceback.format_exc())
            self.log.warning("Failed to send data, will retry in %s sec...", self.client.timeout)
            try:
                time.sleep(self.client.timeout)
                self.client.send_monitoring_data(src_name, data)
                self.log.info("Succeeded with retry")
            except IOError:
                self.log.error("Fatal error sending data: %s", traceback.format_exc())
                self.log.warning("Will skip failed data and continue running")

    def get_monitoring_json(self):
        results = {}
        hosts = []
        kpis = {}

        for item in self.monitoring:
            if item['source'] == 'local':
                item['source'] = platform.node()

            src_name = item['source']
            if src_name not in results:
                results[src_name] = {
                    "name": src_name,
                    "intervals": OrderedDict()
                }

            if src_name not in hosts:
                hosts.append(src_name)

            src = results[src_name]
            tstmp = int(item['ts']) * 1000
            tstmp_key = '%d' % tstmp

            if tstmp_key not in src['intervals']:
                src['intervals'][tstmp_key] = {
                    "start": tstmp,
                    "duration": 1000,
                    "indicators": {}
                }

            for field, value in iteritems(item):
                if field not in ("ts", "source"):
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
                "sessionId": self.client.active_session_id,
                "timestamp": time.time(),
                "userId": self.client.user_id,
                "testId": self.client.test_id,
                "type": "MONITOR",
                "testName": ""
            },
            "kpis": kpis,
            "hosts": hosts,
            "results": results
        }


class ProjectFinder(object):
    def __init__(self, parameters, settings, client, engine):
        super(ProjectFinder, self).__init__()
        self.default_test_name = "Taurus Test"
        self.client = client
        self.parameters = parameters
        self.settings = settings
        self.engine = engine
        self.test_name = None

    def resolve_test_id(self, test_config, taurus_config, rfiles):
        proj_name = self.parameters.get("project", self.settings.get("project", None))
        if isinstance(proj_name, (int, float)):
            proj_id = int(proj_name)
            self.engine.log.debug("Treating project name as ID: %s", proj_id)
        elif proj_name is not None:
            proj_id = self.client.project_by_name(proj_name)
        else:
            proj_id = None

        self.test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))
        return self.client.test_by_name(self.test_name, test_config, taurus_config, rfiles, proj_id)


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
        self.active_session_id = None  # FIXME: it's not good using it for both session id and master ID
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

    def start_online(self, test_id, session_name):
        """
        Start online test

        :type test_id: str
        :return:
        """
        self.log.info("Initiating data feeding...")
        data = urlencode({})

        if self.token:
            url = self.address + "/api/latest/tests/%s/start-external" % test_id
        else:
            url = self.address + "/api/latest/sessions"

        resp = self._request(url, data)

        self.active_session_id = str(resp['result']['session']['id'])
        self.data_signature = str(resp['result']['signature'])
        self.test_id = test_id
        self.user_id = str(resp['result']['session']['userId'])
        if self.token:
            self.results_url = self.address + '/app/#reports/%s' % self.active_session_id
            if session_name:
                url = self.address + "/api/latest/sessions/%s" % self.active_session_id
                self._request(url, to_json({"name": str(session_name)}),
                              headers={"Content-Type": "application/json"}, method='PATCH')
        else:
            self.test_id = resp['result']['session']['testId']
            self.results_url = resp['result']['publicTokenUrl']
        return self.results_url

    def start_taurus(self, test_id):
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
        self.active_session_id = str(resp['result']['id'])
        self.results_url = self.address + '/app/#reports/%s' % self.active_session_id
        return self.results_url

    def end_online(self):
        """
        Finish online test
        """
        if not self.active_session_id:
            self.log.debug("Feeding not started, so not stopping")
        else:
            self.log.info("Ending data feeding...")
            if self.token:
                url = self.address + "/api/latest/sessions/%s/terminate"
                self._request(url % self.active_session_id)
            else:
                url = self.address + "/api/latest/sessions/%s/terminateExternal"
                data = {"signature": self.data_signature, "testId": self.test_id, "sessionId": self.active_session_id}
                self._request(url % self.active_session_id, json.dumps(data))

    def end_master(self, master_id):
        if master_id:
            self.log.info("Ending cloud test...")
            url = self.address + "/api/latest/masters/%s/terminate"
            self._request(url % master_id)

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

    def test_by_name(self, name, configuration, taurus_config, resource_files, proj_id):
        """

        :type name: str
        :rtype: str
        """
        tests = self.get_tests()
        test_id = None
        for test in tests:
            self.log.debug("Test: %s", test)
            if "name" in test and test['name'] == name:
                if test['configuration']['type'] == configuration['type']:
                    if not proj_id or proj_id == test['projectId']:
                        test_id = test['id']
                        self.log.debug("Matched: %s", test)

        if not test_id:
            self.log.debug("Creating new test")
            url = self.address + '/api/latest/tests'
            data = {"name": name, "projectId": proj_id, "configuration": configuration}
            hdr = {"Content-Type": " application/json"}
            resp = self._request(url, json.dumps(data), headers=hdr)
            test_id = resp['result']['id']

        if self.delete_files_before_test:
            self.delete_test_files(test_id)

        if configuration['type'] == 'taurus':  # FIXME: this is weird way to code, subclass it or something
            self.log.debug("Uploading files into the test: %s", resource_files)
            url = '%s/api/latest/tests/%s/files' % (self.address, test_id)

            body = MultiPartForm()

            body.add_file_as_string('script', 'taurus.yml', yaml.dump(taurus_config, default_flow_style=False,
                                                                      explicit_start=True, canonical=False))

            for rfile in resource_files:
                body.add_file('files[]', rfile)

            hdr = {"Content-Type": str(body.get_content_type())}
            _ = self._request(url, body.form_as_bytes(), headers=hdr)

        self.log.debug("Using test ID: %s", test_id)
        return test_id

    def get_tests(self):
        """

        :rtype: list
        """
        tests = self._request(self.address + '/api/latest/tests')
        self.log.debug("Tests for user: %s", len(tests['result']))
        return tests['result']

    def send_kpi_data(self, data_buffer, is_check_response=True, is_final=False):
        """
        Sends online data

        :param is_check_response:
        :type data_buffer: list[bzt.modules.aggregator.DataPoint]
        """
        data = []

        for sec in data_buffer:
            self.first_ts = min(self.first_ts, sec[DataPoint.TIMESTAMP])
            self.last_ts = max(self.last_ts, sec[DataPoint.TIMESTAMP])

            for lbl, item in iteritems(sec[DataPoint.CURRENT]):
                if lbl == '':
                    label = "ALL"
                else:
                    label = lbl

                json_item = None
                for lbl_item in data:
                    if lbl_item["name"] == label:
                        json_item = lbl_item
                        break

                if not json_item:
                    json_item = self.__label_skel(label)
                    data.append(json_item)

                interval_item = self.__interval_json(item, sec)
                for r_code, cnt in iteritems(item[KPISet.RESP_CODES]):
                    interval_item['rc'].append({"n": cnt, "rc": r_code})

                json_item['intervals'].append(interval_item)

                cumul = sec[DataPoint.CUMULATIVE][lbl]
                json_item['n'] = cumul[KPISet.SAMPLE_COUNT]
                json_item["summary"] = self.__summary_json(cumul)

        data = {"labels": data, "sourceID": id(self)}
        if is_final:
            data['final'] = True

        url = self.data_address + "/submit.php?session_id=%s&signature=%s&test_id=%s&user_id=%s"
        url = url % (self.active_session_id, self.data_signature, self.test_id, self.user_id)
        url += "&pq=0&target=%s&update=1" % self.kpi_target
        hdr = {"Content-Type": " application/json"}
        response = self._request(url, to_json(data), headers=hdr)

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
            "name": name,
            "interval": 1,
            "intervals": [],
            "samplesNotCounted": 0,
            "assertionsNotCounted": 0,
            "failedEmbeddedResources": [],
            "failedEmbeddedResourcesSpilloverCount": 0,
            "otherErrorsCount": 0,
            "errors": [],
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

            "bytes": 0,
            "bytesMax": 0,
            "bytesMin": 0,
            "bytesAvg": 0,
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
                "sum": 0,
                "n": 0,
                "std": 0,
                "avg": 0
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
        url = url % (self.active_session_id, self.data_signature)
        hdr = {"Content-Type": str(body.get_content_type())}
        response = self._request(url, body.form_as_bytes(), headers=hdr)
        if not response['result']:
            raise IOError("Upload failed: %s" % response)

    def send_error_summary(self, data_buffer):
        """
        Sends error summary file

        :type data_buffer: list[bzt.modules.aggregator.DataPoint]
        """
        if not data_buffer:
            return

        recent = data_buffer[-1]
        if not recent[DataPoint.CUMULATIVE][''][KPISet.ERRORS]:
            return

        errors = self.__errors_skel(recent[DataPoint.TIMESTAMP], self.active_session_id, self.test_id, self.user_id)
        for label, label_data in iteritems(recent[DataPoint.CUMULATIVE]):
            if not label_data[KPISet.ERRORS]:
                continue

            if label == '':
                label = 'ALL'

            error_item = self.__error_item_skel(label)
            for err_item in label_data[KPISet.ERRORS]:
                if err_item["type"] == KPISet.ERRTYPE_ASSERT:
                    error_item['assertionsCount'] += err_item['cnt']
                    error_item['assertions'].append({
                        "name": "All Assertions",
                        "failureMessage": err_item['msg'],
                        "failure": True,
                        "error": False,
                        "count": err_item['cnt']
                    })
                else:
                    error_item['count'] += err_item['cnt']
                    error_item['responseInfo'].append({
                        "description": err_item['msg'],
                        "code": err_item['rc'],
                        "count": err_item['cnt'],
                    })
            errors['summery']['labels'].append(error_item)

        self.upload_file("sample.jtl.blazemeter.summery.json", to_json(errors))

    def __errors_skel(self, t_stamp, sess_id, test_id, user_id):
        return {
            "reportInfo": {
                "sessionId": sess_id,
                "timestamp": t_stamp,
                "userId": user_id,
                "testId": test_id,
                "type": "SUMMERY",
                # "testName": test_name
            },
            "timestamp": t_stamp,
            "summery": {
                "labels": [],
                "empty": False
            }
        }

    def __error_item_skel(self, label):
        return {
            "name": label,

            "count": 0,
            "responseInfo": [],

            "assertionsCount": 0,
            "assertions": [],

            "embeddedResourcesCount": 0,
            "embeddedResources": [],
        }

    def get_session(self, session_id):
        sess = self._request(self.address + '/api/latest/sessions/%s' % session_id)
        return sess['result']

    def get_master(self, master_id):
        sess = self._request(self.address + '/api/latest/masters/%s' % master_id)
        return sess['result']

    def get_master_status(self, master_id):
        sess = self._request(self.address + '/api/latest/masters/%s/status' % master_id)
        return sess['result']

    def get_master_sessions(self, master_id):
        sess = self._request(self.address + '/api/latest/masters/%s/sessions' % master_id)
        return sess['result']['sessions'] if 'sessions' in sess['result'] else sess['result']

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

    def update_session(self, active_session_id, data):
        hdr = {"Content-Type": "application/json"}
        data = self._request(self.address + '/api/latest/sessions/%s' % active_session_id, to_json(data),
                             headers=hdr, method="PUT")
        return data['result']

    def get_available_locations(self):
        user_info = self.get_user_info()
        return {str(x['id']): x for x in user_info['locations'] if not x['id'].startswith('harbor-')}

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
        replace_in_config(self.engine.config, rfiles, list(map(os.path.basename, prepared_files)), log=self.log)

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
    """

    LOC = "locations"

    def __init__(self):
        super(CloudProvisioning, self).__init__()
        self.results_reader = None
        self.client = BlazeMeterClient(self.log)
        self.test_id = None
        self.test_name = None
        self.__last_master_status = None
        self.browser_open = 'start'
        self.widget = None

    def prepare(self):
        if self.settings.get("dump-locations", False):
            self.log.warning("Dumping available locations instead of running the test")
            self._configure_client()
            info = self.client.get_user_info()
            locations = self.client.get_available_locations()
            for item in info['locations']:
                if item['id'] in locations:
                    self.log.info("Location: %s\t%s", item['id'], item['title'])
            raise ManualShutdown("Done listing locations")

        super(CloudProvisioning, self).prepare()
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        self._configure_client()
        self.__prepare_locations()
        rfiles = self.get_rfiles()

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

        config = self.get_config_for_cloud()
        bza_plugin = self.__get_bza_test_config()
        finder = ProjectFinder(self.parameters, self.settings, self.client, self.engine)
        finder.default_test_name = "Taurus Cloud Test"
        self.test_id = finder.resolve_test_id(bza_plugin, config, rfiles)

        self.test_name = finder.test_name
        self.widget = CloudProvWidget(self)

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.results_reader = ResultsFromBZA(self.client)
            self.results_reader.log = self.log
            self.engine.aggregator.add_underling(self.results_reader)

    def _configure_client(self):
        self.client.logger_limit = self.settings.get("request-logging-limit", self.client.logger_limit)
        # TODO: go to "blazemeter" section for these settings by default?
        self.client.address = self.settings.get("address", self.client.address)
        self.client.token = self.settings.get("token", self.client.token)
        self.client.timeout = dehumanize_time(self.settings.get("timeout", self.client.timeout))
        self.client.delete_files_before_test = self.settings.get("delete-test-files", True)
        if not self.client.token:
            bmmod = self.engine.instantiate_module('blazemeter')
            self.client.token = bmmod.settings.get("token")
            if not self.client.token:
                raise ValueError("You must provide API token to use cloud provisioning")

    def __prepare_locations(self):
        available_locations = self.client.get_available_locations()
        for executor in self.executors:
            locations = self._get_locations(available_locations, executor)
            executor.get_load()  # we need it to resolve load settings into full form

            for location in locations.keys():
                if location not in available_locations:
                    self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                    raise ValueError("Invalid location requested: %s" % location)

    def get_config_for_cloud(self):
        config = copy.deepcopy(self.engine.config)

        if not isinstance(config[ScenarioExecutor.EXEC], list):
            config[ScenarioExecutor.EXEC] = [config[ScenarioExecutor.EXEC]]

        provisioning = config.pop(Provisioning.PROV)
        for execution in config[ScenarioExecutor.EXEC]:
            execution[ScenarioExecutor.CONCURR] = execution.get(ScenarioExecutor.CONCURR).get(provisioning, None)
            execution[ScenarioExecutor.THRPT] = execution.get(ScenarioExecutor.THRPT).get(provisioning, None)

        for key in list(config.keys()):
            if key not in ("scenarios", ScenarioExecutor.EXEC, "included-configs", Service.SERV):
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
                if execution[key] == value:
                    execution.pop(key)

        assert isinstance(config, Configuration)
        config.dump(self.engine.create_artifact("cloud", ""))
        return config

    def __get_bza_test_config(self):
        bza_plugin = {
            "type": "taurus",
            "plugins": {
                "taurus": {
                    "filename": ""  # without this line it does not work
                }
            }
        }
        return bza_plugin

    def _get_locations(self, available_locations, executor):
        locations = executor.execution.get(self.LOC, BetterDict())
        if not locations:
            for location in available_locations.values():
                error = ValueError("No location specified and no default-location configured")
                def_loc = self.settings.get("default-location", error)
                if location['sandbox'] or location['id'] == def_loc:
                    locations.merge({location['id']: 1})
        if not locations:
            self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
            raise ValueError("No sandbox location available, please specify locations manually")
        return locations

    def startup(self):
        super(CloudProvisioning, self).startup()
        self.client.start_taurus(self.test_id)
        self.log.info("Started cloud test: %s", self.client.results_url)
        if self.client.results_url:
            if self.browser_open in ('start', 'both'):
                open_browser(self.client.results_url)

    def check(self):
        # TODO: throttle down requests
        try:
            master = self.client.get_master_status(self.client.active_session_id)
        except URLError:
            self.log.warning("Failed to get test status, will retry in %s seconds...", self.client.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.client.timeout)
            master = self.client.get_master_status(self.client.active_session_id)
            self.log.info("Succeeded with retry")

        if "status" in master and master['status'] != self.__last_master_status:
            self.__last_master_status = master['status']
            self.log.info("Cloud test status: %s", self.__last_master_status)

        if self.results_reader is not None and 'progress' in master and master['progress'] >= 100:
            self.results_reader.master_id = self.client.active_session_id

        if 'progress' in master and master['progress'] > 100:
            self.log.info("Test was stopped in the cloud: %s", master['status'])
            status = self.client.get_master(self.client.active_session_id)
            if 'note' in status and status['note']:
                self.log.warning("Cloud test has probably failed with message: %s", status['note'])

            self.client.active_session_id = None
            return True

        self.widget.update()
        return super(CloudProvisioning, self).check()

    def post_process(self):
        self.client.end_master(self.client.active_session_id)
        if self.client.results_url:
            if self.browser_open in ('end', 'both'):
                open_browser(self.client.results_url)

    def weight_locations(self, locations, load, available_locations):
        total = float(sum(locations.values()))
        for loc_name, share in iteritems(locations):
            loc_info = available_locations[loc_name]
            limits = loc_info['limits']

            if load.duration > limits['duration'] * 60:
                msg = "Test duration %s exceeds limit %s for location %s"
                self.log.warning(msg, load.duration, limits['duration'] * 60, loc_name)

            if load.concurrency:
                locations[loc_name] = int(math.ceil(load.concurrency * share / total / limits['threadsPerEngine']))
            else:
                locations[loc_name] = 1

    def get_widget(self):
        self.widget = CloudProvWidget(self)
        return self.widget


class BlazeMeterClientEmul(BlazeMeterClient):
    def __init__(self, parent_logger):
        super(BlazeMeterClientEmul, self).__init__(parent_logger)
        self.results = []

    def _request(self, url, data=None, headers=None, checker=None, method=None):
        self.log.debug("Request %s: %s", url, data)
        res = self.results.pop(0)
        self.log.debug("Response: %s", res)
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

                    kpiset = KPISet()
                    kpiset[KPISet.FAILURES] = kpi['ec']
                    kpiset[KPISet.CONCURRENCY] = kpi['na']
                    kpiset[KPISet.SAMPLE_COUNT] = kpi['n']
                    kpiset.sum_rt += kpi['t_avg'] * kpi['n'] / 1000.0
                    kpiset.sum_lt += kpi['lt_avg'] * kpi['n'] / 1000.0

                    perc_map = {'90line': 90.0, "95line": 95.0, "99line": 99.0}
                    for field, level in iteritems(perc_map):
                        kpiset[KPISet.PERCENTILES][str(level)] = aggr[label['label']][field]

                    point[DataPoint.CURRENT]['' if label['label'] == 'ALL' else label['label']] = kpiset

            point.recalculate()
            self.min_ts = point[DataPoint.TIMESTAMP] + 1
            yield point

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
    def __init__(self, prov):
        """
        :type prov: CloudProvisioning
        """
        self.prov = prov
        self.text = Text("")
        self._sessions = None
        super(CloudProvWidget, self).__init__([self.text])
        PrioritizedWidget.__init__(self)

    def update(self):
        if not self._sessions:
            self._sessions = self.prov.client.get_master_sessions(self.prov.client.active_session_id)
            if not self._sessions:
                return

        mapping = BetterDict()
        cnt = 0
        for session in self._sessions:
            try:
                cnt += 1
                name_split = session['name'].split('/')
                location = session['configuration']['location']
                count = session['configuration']['serversCount']
                mapping.get(name_split[0]).get(name_split[1])[location] = count
            except KeyError:
                self._sessions = None

        txt = "%s #%s\n" % (self.prov.test_name, self.prov.client.active_session_id)
        for executor, scenarios in iteritems(mapping):
            txt += " %s" % executor
            for scenario, locations in iteritems(scenarios):
                txt += " %s:\n" % scenario
                for location, count in iteritems(locations):
                    txt += "  Agents in %s: %s\n" % (location, count)

        self.text.set_text(txt)
