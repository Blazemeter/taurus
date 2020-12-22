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
import logging
import os
import platform
import re
import sys
import time
import traceback
import zipfile
from abc import abstractmethod
from collections import defaultdict, OrderedDict, Counter, namedtuple
from functools import wraps
from io import BytesIO
from ssl import SSLError
from urllib.error import HTTPError, URLError
from time import sleep

import requests
import yaml
from requests.exceptions import ReadTimeout
from terminaltables import SingleTable, AsciiTable
from urwid import Pile, Text

from bzt import AutomatedShutdown
from bzt import TaurusInternalException, TaurusConfigError, TaurusException, TaurusNetworkError, NormalShutdown
from bzt.bza import User, Session, Test, Workspace, MultiTest, BZA_TEST_DATA_RECEIVED, ENDED
from bzt.engine import Reporter, Provisioning, Configuration, Service
from bzt.engine import Singletone, SETTINGS, ScenarioExecutor, EXEC
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator, ResultsProvider, AggregatorListener
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.functional import FunctionalResultsReader, FunctionalSample
from bzt.modules.monitoring import Monitoring, MonitoringListener, LocalClient
from bzt.modules.services import Unpacker
from bzt.modules.selenium import SeleniumExecutor
from bzt.requests_model import has_variable_pattern
from bzt.utils import iteritems, b, open_browser, BetterDict, ExceptionalDownloader, ProgressBarContext
from bzt.utils import to_json, dehumanize_time, get_full_path, get_files_recursive, replace_in_config, humanize_bytes

TAURUS_TEST_TYPE = "taurus"
FUNC_API_TEST_TYPE = "functionalApi"
FUNC_GUI_TEST_TYPE = "functionalGui"

CLOUD_CONFIG_BLACK_LIST = {
    "settings": {
        "artifacts-dir": True,
        "aggregator": True,
        "proxy": True,
        "check-updates": True
    },
    "included-configs": True,
    "cli": True,
    "cli-aliases": True,
    "install-id": True,
    "version": True,
    "modules": {
        "jmeter": {
            "path": True
        },
        "ab": {
            "path": True
        },
        "gatling": {
            "path": True
        },
        "grinder": {
            "path": True
        },
        "junit": {
            "path": True
        },
        "molotov": {
            "path": True
        },
        "siege": {
            "path": True
        },
        "testng": {
            "path": True
        },
        "tsung": {
            "path": True
        },
        "console": {
            "disable": True,
        },
        "blazemeter": {
            "address": True,
            "data-address": True,
            "token": True,
        },
        "cloud": {
            "address": True,
            "data-address": True,
            "token": True,
        },
    },
    "provisioning": True,

}

NETWORK_PROBLEMS = (IOError, URLError, SSLError, ReadTimeout, TaurusNetworkError)
NOTE_SIZE_LIMIT = 2048


def send_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        if not isinstance(self, BlazeMeterUploader):
            raise TaurusInternalException("send_with_retry should only be applied to BlazeMeterUploader methods")

        try:
            method(self, *args, **kwargs)
        except (IOError, TaurusNetworkError):
            self.log.debug("Error sending data: %s", traceback.format_exc())
            self.log.warning("Failed to send data, will retry in %s sec...", self._user.timeout)
            try:
                time.sleep(self._user.timeout)
                method(self, *args, **kwargs)
                self.log.info("Succeeded with retry")
            except NETWORK_PROBLEMS:
                self.log.error("Fatal error sending data: %s", traceback.format_exc())
                self.log.warning("Will skip failed data and continue running")

    return _impl


def get_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        if not isinstance(self, CloudProvisioning):
            raise TaurusInternalException("get_with_retry should only be applied to CloudProvisioning class methods")

        while True:
            try:
                return method(self, *args, **kwargs)
            except NETWORK_PROBLEMS:
                self.log.debug("Error making request: %s", traceback.format_exc())
                self.log.warning("Failed to make request, will retry in %s sec...", self.user.timeout)
                time.sleep(self.user.timeout)

    return _impl


def parse_blazemeter_test_link(link):
    """
    https://a.blazemeter.com/app/#/accounts/97961/workspaces/89846/projects/229969/tests/5823512

    :param link:
    :return:
    """
    if not isinstance(link, str):
        return None

    regex = r'https://a.blazemeter.com/app/#/accounts/(\d+)/workspaces/(\d+)/projects/(\d+)/tests/(\d+)(?:/\w+)?'
    match = re.match(regex, link)
    if match is None:
        return None

    TestParams = namedtuple('TestParams', 'account_id,workspace_id,project_id,test_id')
    return TestParams(*[int(x) for x in match.groups()])


class BlazeMeterUploader(Reporter, AggregatorListener, MonitoringListener, Singletone):
    """
    Reporter class

    :type _test: bzt.bza.Test
    :type _master: bzt.bza.Master
    :type _session: bzt.bza.Session
    """

    def __init__(self):
        super(BlazeMeterUploader, self).__init__()
        self.browser_open = 'start'
        self.kpi_buffer = []
        self.send_interval = 30
        self._last_status_check = time.time()
        self.send_data = True
        self.upload_artifacts = True
        self.send_monitoring = True
        self.monitoring_buffer = None
        self.public_report = False
        self.last_dispatch = 0
        self.results_url = None
        self._user = User()
        self._test = None
        self._master = None
        self._session = None
        self.first_ts = sys.maxsize
        self.last_ts = 0
        self.report_name = None
        self._dpoint_serializer = DatapointSerializer(self)

    def prepare(self):
        """
        Read options for uploading, check that they're sane
        """
        super(BlazeMeterUploader, self).prepare()
        self.send_interval = dehumanize_time(self.settings.get("send-interval", self.send_interval))
        self.send_monitoring = self.settings.get("send-monitoring", self.send_monitoring)
        monitoring_buffer_limit = self.settings.get("monitoring-buffer-limit", 500)
        self.monitoring_buffer = MonitoringBuffer(monitoring_buffer_limit, self.log)
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        self.public_report = self.settings.get("public-report", self.public_report)
        self.upload_artifacts = self.parameters.get("upload-artifacts", self.upload_artifacts)
        self._dpoint_serializer.multi = self.settings.get("report-times-multiplier", self._dpoint_serializer.multi)
        token = self.settings.get("token", "")
        if not token:
            self.log.warning("No BlazeMeter API key provided, will upload anonymously")
        self._user.token = token

        # usual fields
        self._user.logger_limit = self.settings.get("request-logging-limit", self._user.logger_limit)
        self._user.address = self.settings.get("address", self._user.address).rstrip("/")
        self._user.data_address = self.settings.get("data-address", self._user.data_address).rstrip("/")
        self._user.timeout = dehumanize_time(self.settings.get("timeout", self._user.timeout))
        if isinstance(self._user.http_session, requests.Session):
            self.log.debug("Installing http client")
            self._user.http_session = self.engine.get_http_client()
            self._user.http_request = self._user.http_session.request

        # direct data feeding case
        sess_id = self.parameters.get("session-id")
        if sess_id:
            self._session = Session(self._user, {'id': sess_id})
            self._session['userId'] = self.parameters.get("user-id", None)
            self._session['testId'] = self.parameters.get("test-id", None)
            self._test = Test(self._user, {'id': self._session['testId']})
            exc = TaurusConfigError("Need signature for session")
            self._session.data_signature = self.parameters.get("signature", exc)
            self._session.kpi_target = self.parameters.get("kpi-target", self._session.kpi_target)
            self.send_data = self.parameters.get("send-data", self.send_data)
        else:
            try:
                self._user.ping()  # to check connectivity and auth
            except HTTPError:
                self.log.error("Cannot reach online results storage, maybe the address/token is wrong")
                raise

            if token:
                wsp = self._user.accounts().workspaces()
                if not wsp:
                    raise TaurusNetworkError("Your account has no active workspaces, please contact BlazeMeter support")
                finder = ProjectFinder(self.parameters, self.settings, self._user, wsp, self.log)
                self._test = finder.resolve_external_test()
            else:
                self._test = Test(self._user, {'id': None})

        self.report_name = self.parameters.get("report-name", self.settings.get("report-name", self.report_name))
        if self.report_name == 'ask' and sys.stdin.isatty():
            self.report_name = input("Please enter report-name: ")

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
        self._user.log = self.log.getChild(self.__class__.__name__)

        if not self._session:
            url = self._start_online()
            self.log.info("Started data feeding: %s", url)
            if self.browser_open in ('start', 'both'):
                open_browser(url)

            if self._user.token and self.public_report:
                report_link = self._master.make_report_public()
                self.log.info("Public report link: %s", report_link)

    def _start_online(self):
        """
        Start online test

        """
        self.log.info("Initiating data feeding...")

        if self._test['id']:
            self._session, self._master = self._test.start_external()
        else:
            self._session, self._master, self.results_url = self._test.start_anonymous_external_test()
            self._test['id'] = self._session['testId']

        if self._test.token:
            self.results_url = self._master.address + '/app/#/masters/%s' % self._master['id']
            if self.report_name:
                self._session.set({"name": str(self.report_name)})

        return self.results_url

    def __get_jtls_and_more(self):
        """
        Compress all files in artifacts dir to single zipfile
        :rtype: (io.BytesIO,dict)
        """
        mfile = BytesIO()
        listing = {}

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

                    fsize = os.path.getsize(full_path)
                    if fsize <= max_file_size:
                        zfh.write(full_path, os.path.join(os.path.relpath(root, self.engine.artifacts_dir), filename))
                        listing[full_path] = fsize
                    else:
                        msg = "File %s exceeds maximum size quota of %s and won't be included into upload"
                        self.log.warning(msg, filename, max_file_size)

            for filename in logs:  # upload logs unconditionally
                zfh.write(filename, os.path.basename(filename))
                listing[filename] = os.path.getsize(filename)
        return mfile, listing

    def __upload_artifacts(self):
        """
        If token provided, upload artifacts folder contents and bzt.log
        """
        if not self._session.token:
            return

        worker_index = self.engine.config.get('modules').get('shellexec').get('env').get('TAURUS_INDEX_ALL')
        if worker_index:
            suffix = '-%s' % worker_index
        else:
            suffix = ''
        artifacts_zip = "artifacts%s.zip" % suffix
        mfile, zip_listing = self.__get_jtls_and_more()
        self.log.info("Uploading all artifacts as %s ...", artifacts_zip)
        self._session.upload_file(artifacts_zip, mfile.getvalue())
        self._session.upload_file(artifacts_zip + '.tail.bz', self.__format_listing(zip_listing))

        handlers = self.engine.log.parent.handlers
        for handler in handlers:
            if isinstance(handler, logging.FileHandler):
                fname = handler.baseFilename
                self.log.info("Uploading %s", fname)
                fhead, ftail = os.path.splitext(os.path.split(fname)[-1])
                modified_name = fhead + suffix + ftail
                with open(fname, 'rb') as _file:
                    self._session.upload_file(modified_name, _file.read())
                    _file.seek(-4096, 2)
                    tail = _file.read()
                    tail = tail[tail.index(b("\n")) + 1:]
                    self._session.upload_file(modified_name + ".tail.bz", tail)

    def post_process(self):
        """
        Upload results if possible
        """
        if not self._session:
            self.log.debug("No feeding session obtained, nothing to finalize")
            return

        self.log.debug("KPI bulk buffer len in post-proc: %s", len(self.kpi_buffer))
        try:
            self.log.info("Sending remaining KPI data to server...")
            if self.send_data:
                self.__send_data(self.kpi_buffer, False, True)
                self.kpi_buffer = []

            if self.send_monitoring:
                self.__send_monitoring()
        finally:
            self._postproc_phase2()

        if self.results_url:
            if self.browser_open in ('end', 'both'):
                open_browser(self.results_url)
            self.log.info("Online report link: %s", self.results_url)

    def _postproc_phase2(self):
        try:
            if self.upload_artifacts:
                self.__upload_artifacts()
        except (IOError, TaurusNetworkError):
            self.log.warning("Failed artifact upload: %s", traceback.format_exc())
        finally:
            self._last_status_check = self.parameters.get('forced-last-check', self._last_status_check)
            self.log.debug("Set last check time to: %s", self._last_status_check)

            tries = self.send_interval  # NOTE: you dirty one...
            while not self._last_status_check and tries > 0:
                self.log.info("Waiting for ping...")
                time.sleep(self.send_interval)
                tries -= 1

            self._postproc_phase3()

    def _postproc_phase3(self):
        try:
            if self.send_data:
                self.end_online()

            if self._user.token and self.engine.stopping_reason:
                exc_class = self.engine.stopping_reason.__class__.__name__
                note = "%s: %s" % (exc_class, str(self.engine.stopping_reason))
                self.append_note_to_session(note)
                if self._master:
                    self.append_note_to_master(note)

        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            self.log.debug("Failed to finish online: %s", traceback.format_exc())
            self.log.warning("Failed to finish online: %s", exc)

    def end_online(self):
        """
        Finish online test
        """
        if not self._session:
            self.log.debug("Feeding not started, so not stopping")
        else:
            self.log.info("Ending data feeding...")
            if self._user.token:
                self._session.stop()
            else:
                self._session.stop_anonymous()

    def append_note_to_session(self, note):
        self._session.fetch()
        if 'note' in self._session:
            note = self._session['note'] + '\n' + note
        note = note.strip()
        if note:
            self._session.set({'note': note[:NOTE_SIZE_LIMIT]})

    def append_note_to_master(self, note):
        self._master.fetch()
        if 'note' in self._master:
            note = self._master['note'] + '\n' + note
        note = note.strip()
        if note:
            self._master.set({'note': note[:NOTE_SIZE_LIMIT]})

    def check(self):
        """
        Send data if any in buffer
        """
        self.log.debug("KPI bulk buffer len: %s", len(self.kpi_buffer))
        if self.last_dispatch < (time.time() - self.send_interval):
            self.last_dispatch = time.time()
            if self.send_data and len(self.kpi_buffer):
                self.__send_data(self.kpi_buffer)
                self.kpi_buffer = []

            if self.send_monitoring:
                self.__send_monitoring()
        return super(BlazeMeterUploader, self).check()

    @send_with_retry
    def __send_data(self, data, do_check=True, is_final=False):
        """
        :type data: list[bzt.modules.aggregator.DataPoint]
        """
        if not self._session:
            return

        serialized = self._dpoint_serializer.get_kpi_body(data, is_final)
        self._session.send_kpi_data(serialized, do_check)

    def aggregated_second(self, data):
        """
        Send online data
        :param data: DataPoint
        """
        if self.send_data:
            self.kpi_buffer.append(data)

    def monitoring_data(self, data):
        if self.send_monitoring:
            self.monitoring_buffer.record_data(data)

    @send_with_retry
    def __send_monitoring(self):
        engine_id = self.engine.config.get('modules').get('shellexec').get('env').get('TAURUS_INDEX_ALL', '')
        if not engine_id:
            engine_id = "0"
        data = self.monitoring_buffer.get_monitoring_json(self._session)
        self._session.send_monitoring_data(engine_id, data)

    def __format_listing(self, zip_listing):
        lines = []
        for fname in sorted(zip_listing.keys()):
            bytestr = humanize_bytes(zip_listing[fname])
            if fname.startswith(self.engine.artifacts_dir):
                fname = fname[len(self.engine.artifacts_dir) + 1:]
            lines.append(bytestr + " " + fname)
        return "\n".join(lines)


class MonitoringBuffer(object):
    def __init__(self, size_limit, parent_log):
        self.size_limit = size_limit
        self.data = defaultdict(OrderedDict)
        self.log = parent_log.getChild(self.__class__.__name__)
        # data :: dict(datasource -> dict(interval -> datapoint))
        # datapoint :: dict(metric -> value)

    def record_data(self, data):
        for monitoring_item in data:
            item = copy.deepcopy(monitoring_item)
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
            self.log.debug("Monitoring buffer size '%s': %s", source, len(self.data[source]))

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

    def get_monitoring_json(self, session):
        """
        :type session: Session
        """
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
                    if field.lower().startswith('conn-all'):
                        field = 'Connections'
                    elif field.lower().startswith('cpu'):
                        field = 'CPU'
                    elif field.lower().startswith('mem'):
                        field = 'Memory'
                        value *= 100
                    elif field == 'bytes-recv' or field.lower().startswith('net'):
                        field = 'Network I/O'
                    elif field == 'engine-loop':
                        field = 'Busy Taurus'
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

        kpis = {"Network I/O": "Network I/O", "Memory": "Memory", "CPU": "CPU", "Connections": "Connections"}
        return {
            "reportInfo": {
                "sessionId": session['id'],
                "timestamp": time.time(),
                "userId": session['userId'],
                "testId": session['testId'],
                "type": "MONITOR",
                "testName": ""
            },
            "kpis": kpis,
            "hosts": hosts,
            "results": results
        }


class DatapointSerializer(object):
    def __init__(self, owner):
        """
        :type owner: BlazeMeterUploader
        """
        super(DatapointSerializer, self).__init__()
        self.owner = owner
        self.multi = 1000  # miltiplier factor for reporting

    def get_kpi_body(self, data_buffer, is_final):
        # - reporting format:
        #   {labels: <data>,    # see below
        #    sourceID: <id of BlazeMeterClient object>,
        #    [is_final: True]}  # for last report
        #
        # - elements of 'data' are described in __get_label()
        #
        # - elements of 'intervals' are described in __get_interval()
        #   every interval contains info about response codes have gotten on it.
        report_items = BetterDict()
        if data_buffer:
            self.owner.first_ts = min(self.owner.first_ts, data_buffer[0][DataPoint.TIMESTAMP])
            self.owner.last_ts = max(self.owner.last_ts, data_buffer[-1][DataPoint.TIMESTAMP])

            # following data is received in the cumulative way
            for label, kpi_set in iteritems(data_buffer[-1][DataPoint.CUMULATIVE]):
                report_item = self.__get_label(label, kpi_set)
                self.__add_errors(report_item, kpi_set)  # 'Errors' tab
                report_items[label] = report_item

            # fill 'Timeline Report' tab with intervals data
            # intervals are received in the additive way
            for dpoint in data_buffer:
                time_stamp = dpoint[DataPoint.TIMESTAMP]
                for label, kpi_set in iteritems(dpoint[DataPoint.CURRENT]):
                    exc = TaurusInternalException('Cumulative KPISet is non-consistent')
                    report_item = report_items.get(label, exc)
                    report_item['intervals'].append(self.__get_interval(kpi_set, time_stamp))

        report_items = [report_items[key] for key in sorted(report_items.keys())]  # convert dict to list
        data = {"labels": report_items, "sourceID": id(self.owner)}
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
            elif error["type"] == KPISet.ERRTYPE_SUBSAMPLE:
                report_item['failedEmbeddedResources'].append({
                    "count": error['cnt'],
                    "rm": error['msg'],
                    "rc": error['rc'],
                    "url": list(error['urls'])[0] if error['urls'] else None,
                })
            else:
                report_item['assertions'].append({
                    'failureMessage': error['msg'],
                    'name': error['tag'] if error['tag'] else 'All Assertions',
                    'failures': error['cnt']
                    # TODO: "count", "errors" = ? (according do Udi's format description)
                })

    def __get_label(self, name, cumul):
        return {
            "n": cumul[KPISet.SAMPLE_COUNT],  # total count of samples
            "name": name if name else 'ALL',  # label
            "interval": 1,  # not used
            "intervals": [],  # list of intervals, fill later
            "samplesNotCounted": 0,  # not used
            "assertionsNotCounted": 0,  # not used
            "failedEmbeddedResources": [],  # not used
            "failedEmbeddedResourcesSpilloverCount": 0,  # not used
            "otherErrorsCount": 0,  # not used
            "errors": [],  # list of errors, fill later
            "assertions": [],  # list of assertions, fill later
            "percentileHistogram": [],  # not used
            "percentileHistogramLatency": [],  # not used
            "percentileHistogramBytes": [],  # not used
            "empty": False,  # not used
            "summary": self.__get_summary(cumul)  # summary info
        }

    def __get_summary(self, cumul):
        return {
            "first": self.owner.first_ts,
            "last": self.owner.last_ts,
            "duration": self.owner.last_ts - self.owner.first_ts,
            "failed": cumul[KPISet.FAILURES],
            "hits": cumul[KPISet.SAMPLE_COUNT],

            "avg": int(self.multi * cumul[KPISet.AVG_RESP_TIME]),
            "min": int(self.multi * cumul[KPISet.PERCENTILES]["0.0"]) if "0.0" in cumul[KPISet.PERCENTILES] else 0,
            "max": int(self.multi * cumul[KPISet.PERCENTILES]["100.0"]) if "100.0" in cumul[KPISet.PERCENTILES] else 0,
            "std": int(self.multi * cumul[KPISet.STDEV_RESP_TIME]),
            "tp90": int(self.multi * cumul[KPISet.PERCENTILES]["90.0"]) if "90.0" in cumul[KPISet.PERCENTILES] else 0,
            "tp95": int(self.multi * cumul[KPISet.PERCENTILES]["95.0"]) if "95.0" in cumul[KPISet.PERCENTILES] else 0,
            "tp99": int(self.multi * cumul[KPISet.PERCENTILES]["99.0"]) if "99.0" in cumul[KPISet.PERCENTILES] else 0,

            "latencyAvg": int(self.multi * cumul[KPISet.AVG_LATENCY]),
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

    def __get_interval(self, item, time_stamp):
        #   rc_list - list of info about response codes:
        #   {'n': <number of code encounters>,
        #    'f': <number of failed request (e.q. important for assertions)>
        #    'rc': <string value of response code>}
        rc_list = []
        for r_code, cnt in iteritems(item[KPISet.RESP_CODES]):
            fails = [err['cnt'] for err in item[KPISet.ERRORS] if str(err['rc']) == r_code]
            rc_list.append({"n": cnt, 'f': fails, "rc": r_code})

        return {
            "ec": item[KPISet.FAILURES],
            "ts": time_stamp,
            "na": item[KPISet.CONCURRENCY],
            "n": item[KPISet.SAMPLE_COUNT],
            "failed": item[KPISet.FAILURES],
            "rc": rc_list,
            "t": {
                "min": int(self.multi * item[KPISet.PERCENTILES]["0.0"]) if "0.0" in item[KPISet.PERCENTILES] else 0,
                "max": int(self.multi * item[KPISet.PERCENTILES]["100.0"]) if "100.0" in item[
                    KPISet.PERCENTILES] else 0,
                "sum": self.multi * item[KPISet.AVG_RESP_TIME] * item[KPISet.SAMPLE_COUNT],
                "n": item[KPISet.SAMPLE_COUNT],
                "std": self.multi * item[KPISet.STDEV_RESP_TIME],
                "avg": self.multi * item[KPISet.AVG_RESP_TIME]
            },
            "lt": {
                "min": 0,
                "max": 0,
                "sum": self.multi * item[KPISet.AVG_LATENCY] * item[KPISet.SAMPLE_COUNT],
                "n": item[KPISet.SAMPLE_COUNT],
                "std": 0,
                "avg": self.multi * item[KPISet.AVG_LATENCY]
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


class ProjectFinder(object):
    def __init__(self, parameters, settings, user, workspaces, parent_log):
        super(ProjectFinder, self).__init__()
        self.default_test_name = "Taurus Test"
        self.parameters = parameters
        self.settings = settings
        self.log = parent_log.getChild(self.__class__.__name__)
        self.user = user
        self.workspaces = workspaces
        self.test_type = None

    def _find_project(self, proj_name):
        """
        :rtype: bzt.bza.Project
        """
        if isinstance(proj_name, (int, float)):  # project id
            proj_id = int(proj_name)
            self.log.debug("Treating project name as ID: %s", proj_id)
            project = self.workspaces.projects(ident=proj_id).first()
            if not project:
                raise TaurusConfigError("BlazeMeter project not found by ID: %s" % proj_id)
        elif proj_name:
            project = self.workspaces.projects(name=proj_name).first()
        else:
            project = None

        return project

    def _ws_proj_switch(self, project):
        if project:
            return project
        else:
            return self.workspaces

    def resolve_external_test(self):
        proj_name = self.parameters.get("project", self.settings.get("project"))
        test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))

        project = self._find_project(proj_name)
        if not project and proj_name:
            project = self.workspaces.first().create_project(proj_name)

        test = self._ws_proj_switch(project).tests(name=test_name, test_type='external').first()

        if not test:
            if not project:
                info = self.user.fetch()
                project = self.workspaces.projects(ident=info['defaultProject']['id']).first()
                if not project:
                    project = self.workspaces.first().create_project("Taurus Tests Project")

            test = project.create_test(test_name, {"type": "external"})

        return test

    def resolve_account(self, account_name):
        account = None

        if isinstance(account_name, (int, float)):  # TODO: what if it's string "123"?
            acc_id = int(account_name)
            self.log.debug("Treating account name as ID: %s", acc_id)
            account = self.user.accounts(ident=acc_id).first()
            if not account:
                raise TaurusConfigError("BlazeMeter account not found by ID: %s" % acc_id)
        elif account_name:
            account = self.user.accounts(name=account_name).first()
            if not account:
                raise TaurusConfigError("BlazeMeter account not found by name: %s" % account_name)

        if account:
            return account

        self.user.fetch()
        account = self.user.accounts(ident=self.user['defaultProject']['accountId']).first()
        self.log.debug("Using default account: %s", account)
        return account

    def resolve_workspace(self, account, workspace_name):
        workspace = None

        if isinstance(workspace_name, (int, float)):  # TODO: what if it's string "123"?
            workspace_id = int(workspace_name)
            self.log.debug("Treating workspace name as ID: %s", workspace_id)
            workspace = account.workspaces(ident=workspace_id).first()
            if not workspace:
                raise TaurusConfigError("BlazeMeter workspace not found by ID: %s" % workspace_id)
        elif workspace_name is not None:
            workspace = account.workspaces(name=workspace_name).first()
            if not workspace:
                raise TaurusConfigError("BlazeMeter workspace not found: %s" % workspace_name)

        if workspace is None:
            workspace = account.workspaces(ident=self.user['defaultProject']['workspaceId']).first()
            self.log.debug("Using first workspace: %s" % workspace)

        return workspace

    def resolve_project(self, workspace, project_name):
        if isinstance(project_name, (int, float)):  # project id
            project_id = int(project_name)
            self.log.debug("Treating project name as ID: %s", project_id)
            project = workspace.projects(ident=project_id).first()
            if not project:
                raise TaurusConfigError("BlazeMeter project not found by ID: %s" % project_id)
        elif project_name:
            project = workspace.projects(name=project_name).first()
        else:
            project = None

        if not project:
            project = self._create_project_or_use_default(workspace, project_name)

        return project

    def _create_project_or_use_default(self, workspace, proj_name):
        if proj_name:
            return workspace.create_project(proj_name)
        else:
            info = self.user.fetch()
            self.log.debug("Looking for default project: %s", info['defaultProject']['id'])
            project = self.workspaces.projects(ident=info['defaultProject']['id']).first()
            if not project:
                project = workspace.create_project("Taurus Tests Project")
            return project

    def resolve_test(self, project, test_name, test_type):
        is_int = isinstance(test_name, (int, float))
        is_digit = isinstance(test_name, str) and test_name.isdigit()

        if is_int or is_digit:
            test_id = int(test_name)
            self.log.debug("Treating project name as ID: %s", test_id)
            test = project.multi_tests(ident=test_id).first()
            if not test:
                test = project.tests(ident=test_id, test_type=test_type).first()
            if not test:
                raise TaurusConfigError("BlazeMeter test not found by ID: %s" % test_id)
        elif test_name is not None:
            test = project.multi_tests(name=test_name).first()
            if not test:
                test = project.tests(name=test_name, test_type=test_type).first()
        else:
            test = None

        return test

    def get_test_router(self):
        use_deprecated = self.settings.get("use-deprecated-api", True)
        default_location = self.settings.get("default-location", None)
        account_name = self.parameters.get("account", self.settings.get("account", None))
        workspace_name = self.parameters.get("workspace", self.settings.get("workspace", None))
        project_name = self.parameters.get("project", self.settings.get("project"))
        test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))
        launch_existing_test = self.settings.get("launch-existing-test", False)

        # if we're to launch existing test - don't use test_type filter (look for any type)
        filter_test_type = None if launch_existing_test else self.test_type

        test_spec = parse_blazemeter_test_link(test_name)
        self.log.debug("Parsed test link: %s", test_spec)
        if test_spec is not None:
            account, workspace, project, test = self.user.test_by_ids(test_spec.account_id, test_spec.workspace_id,
                                                                      test_spec.project_id, test_spec.test_id,
                                                                      test_type=filter_test_type)
            if not test:
                raise TaurusConfigError("Test not found: %s", test_name)
            self.log.info("Found test by link: %s", test_name)
        else:
            account = self.resolve_account(account_name)
            workspace = self.resolve_workspace(account, workspace_name)
            project = self.resolve_project(workspace, project_name)
            test = self.resolve_test(project, test_name, test_type=filter_test_type)

        if isinstance(test, MultiTest):
            self.log.debug("Detected test type: multi")
            test_class = CloudCollectionTest
        elif isinstance(test, Test):
            self.log.debug("Detected test type: standard")
            test_class = CloudTaurusTest
        else:   # test not found
            if launch_existing_test:
                raise TaurusConfigError("Can't find test for launching: %r" % test_name)

            if use_deprecated or self.settings.get("cloud-mode") == 'taurusCloud':
                self.log.debug("Will create standard test")
                test_class = CloudTaurusTest
            else:
                self.log.debug("Will create a multi test")
                test_class = CloudCollectionTest

        router = test_class(self.user, test, project, test_name, default_location, launch_existing_test, self.log)
        router._workspaces = self.workspaces
        router.cloud_mode = self.settings.get("cloud-mode", None)
        router.dedicated_ips = self.settings.get("dedicated-ips", False)
        router.test_type = self.test_type
        router.send_report_email = self.settings.get("send-report-email", False)
        return router


class BaseCloudTest(object):
    """
    :type _user: bzt.bza.User
    :type _project: bzt.bza.Project
    :type _test: bzt.bza.Test
    :type master: bzt.bza.Master
    :type cloud_mode: str
    """

    def __init__(self, user, test, project, test_name, default_location, launch_existing_test, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.default_location = default_location
        self._test_name = test_name
        self._last_status = None
        self._sessions = None
        self._started = False
        self._user = user
        self._project = project
        self._test = test
        self.launch_existing_test = launch_existing_test
        self.master = None
        self._workspaces = None
        self.cloud_mode = None
        self.dedicated_ips = False
        self.test_type = None
        self.send_report_email = False

    @abstractmethod
    def prepare_locations(self, executors, engine_config):
        pass

    @abstractmethod
    def resolve_test(self, taurus_config, rfiles, delete_old_files=False):
        pass

    @abstractmethod
    def launch_test(self):
        """launch cloud test"""
        pass

    @abstractmethod
    def sanitize_test(self):
        """Sanitize cloud test"""
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
        self._last_status = self.master.get_status()
        return self._last_status


class CloudTaurusTest(BaseCloudTest):
    def prepare_locations(self, executors, engine_config):
        available_locations = {}
        is_taurus4 = True
        workspace = Workspace(self._project, {'id': self._project['workspaceId']})
        for loc in workspace.locations(include_private=is_taurus4):
            available_locations[loc['id']] = loc

        if CloudProvisioning.LOC in engine_config and not is_taurus4:
            self.log.warning("Deprecated test API doesn't support global locations")

        for executor in executors:
            if CloudProvisioning.LOC in executor.execution \
                    and isinstance(executor.execution[CloudProvisioning.LOC], dict):
                exec_locations = executor.execution[CloudProvisioning.LOC]
                self._check_locations(exec_locations, available_locations)
            elif CloudProvisioning.LOC in engine_config and is_taurus4:
                self._check_locations(engine_config[CloudProvisioning.LOC], available_locations)
            else:
                default_loc = self._get_default_location(available_locations)
                executor.execution[CloudProvisioning.LOC] = BetterDict.from_dict({default_loc: 1})

            executor.get_load()  # we need it to resolve load settings into full form

    def _get_default_location(self, available_locations):
        if self.default_location and self.default_location in available_locations:
            return self.default_location

        self.log.debug("Default location %s not found", self.default_location)

        for location_id in sorted(available_locations):
            location = available_locations[location_id]
            if location['sandbox'] and not location.get('purposes', {}).get('functional', False):
                return location_id

        if available_locations:
            location_id = sorted(available_locations.keys())[0]
            self.log.warning("Using first location as default: %s", location_id)
            return location_id

        self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
        raise TaurusConfigError("No sandbox or default location available, please specify locations manually")

    def _check_locations(self, locations, available_locations):
        for location in locations:
            if location not in available_locations:
                self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                raise TaurusConfigError("Invalid location requested: %s" % location)

    def resolve_test(self, taurus_config, rfiles, delete_old_files=False):
        if self.launch_existing_test:
            return

        if self._test is not None:
            test_type = self._test.get("configuration").get("type")
            if test_type != self.test_type:
                self.log.debug("Can't reuse test type %r as Taurus test, will create new one", test_type)
                self._test = None

        if self._test is None:
            test_config = {
                "type": self.test_type,
                "plugins": {
                    "taurus": {
                        "filename": ""  # without this line it does not work
                    }
                }
            }

            self._test = self._project.create_test(self._test_name, test_config)

        if delete_old_files:
            self._test.delete_files()

        taurus_config = yaml.safe_dump(taurus_config, default_flow_style=False, explicit_start=True, canonical=False)
        self._test.upload_files(taurus_config, rfiles)
        self._test.update_props({'configuration': {'executionType': self.cloud_mode}})
        self._test.update_props({
            "shouldSendReportEmail": self.send_report_email
        })

    def sanitize_test(self):
        self._test.update_props({'overrideExecutions': []})
        self._test.update_props({'configuration': {'scriptType': 'taurus'}})

    def launch_test(self):
        self.log.info("Initiating cloud test with %s ...", self._test.address)
        self.master = self._test.start(as_functional=self.test_type in (FUNC_API_TEST_TYPE, FUNC_GUI_TEST_TYPE))
        return self.master.address + '/app/#/masters/%s' % self.master['id']

    def start_if_ready(self):
        self._started = True

    def stop_test(self):
        if self.master:
            self.log.info("Ending cloud test...")
            if not self._last_status:
                self.get_master_status()

            if self._last_status["progress"] >= 100:
                self.master.stop()
            else:
                self.master.terminate()

    def get_test_status_text(self):
        if not self._sessions:
            self._sessions = self.master.sessions()
            if not self._sessions:
                return

        mapping = BetterDict()  # dict(executor -> dict(scenario -> dict(location -> servers count)))
        for session in self._sessions:
            try:
                name_split = [part.strip() for part in session['name'].split('/')]
                location = session['configuration']['location']
                count = session['configuration']['serversCount']
                ex_item = mapping.get(name_split[0], force_set=True)

                if len(name_split) > 1:
                    name = name_split[1]
                else:
                    name = "N/A"

                ex_item.get(name, force_set=True)[location] = count
            except KeyError:
                self._sessions = None

        txt = "%s #%s\n" % (self._test['name'], self.master['id'])
        for executor, scenarios in iteritems(mapping):
            txt += " %s" % executor
            for scenario, locations in iteritems(scenarios):
                txt += " %s:\n" % scenario
                for location, count in iteritems(locations):
                    txt += "  Agents in %s: %s\n" % (location, count)

        return txt


class CloudCollectionTest(BaseCloudTest):
    def prepare_locations(self, executors, engine_config):
        available_locations = {}
        for loc in self._workspaces.locations(include_private=True):
            available_locations[loc['id']] = loc

        global_locations = engine_config.get(CloudProvisioning.LOC)
        self._check_locations(global_locations, available_locations)

        for executor in executors:
            if CloudProvisioning.LOC in executor.execution:
                exec_locations = executor.execution[CloudProvisioning.LOC]
                self._check_locations(exec_locations, available_locations)
            else:
                if not global_locations:
                    default_loc = self._get_default_location(available_locations)
                    executor.execution[CloudProvisioning.LOC] = BetterDict.from_dict({default_loc: 1})

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
        raise TaurusConfigError("No sandbox or default location available, please specify locations manually")

    def _check_locations(self, locations, available_locations):
        for location in locations:
            if location not in available_locations:
                self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                raise TaurusConfigError("Invalid location requested: %s" % location)

    def resolve_test(self, taurus_config, rfiles, delete_old_files=False):
        self.log.warning("Using collection-based tests is deprecated in Taurus, will be removed in future versions")
        if self.launch_existing_test:
            return

        # TODO: handle delete_old_files ?
        if not self._project:
            raise TaurusInternalException()  # TODO: build unit test to catch this situation

        collection_draft = self._user.collection_draft(self._test_name, taurus_config, rfiles)
        if self._test is None:
            self.log.debug("Creating cloud collection test")
            self._test = self._project.create_multi_test(collection_draft)
        else:
            self.log.debug("Overriding cloud collection test")
            collection_draft['projectId'] = self._project['id']
            self._test.update_collection(collection_draft)

    def launch_test(self):
        self.log.info("Initiating cloud test with %s ...", self._test.address)
        self.master = self._test.start()
        return self.master.address + '/app/#/masters/%s' % self.master['id']

    def start_if_ready(self):
        if self._started:
            return
        if self._last_status is None:
            return
        sessions = self._last_status.get("sessions")
        if sessions and all(session["status"] == "JMETER_CONSOLE_INIT" for session in sessions):
            self.log.info("All servers are ready, starting cloud test")
            self.master.force_start()
            self._started = True

    def await_test_end(self):
        iterations = 0
        while True:
            if iterations > 100:
                self.log.debug("Await: iteration limit reached")
                return
            status = self.master.get_status()
            if status.get("status") == "ENDED":
                return
            iterations += 1
            time.sleep(1.0)

    def stop_test(self):
        if self._started and self._test:
            self.log.info("Shutting down cloud test...")
            self._test.stop()
            self.await_test_end()
        elif self.master:
            self.log.info("Shutting down cloud test...")
            self.master.stop()

    def get_test_status_text(self):
        if not self._sessions:
            sessions = self.master.sessions()
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
                scenario_item = mapping.get(scenario, force_set=True)
                scenario_item.get(location, 0, force_set=True)

                scenario_item[location] += servers_count
            except (KeyError, TypeError):
                self._sessions = None

        txt = "%s #%s\n" % (self._test['name'], self.master['id'])
        for scenario, locations in iteritems(mapping):
            txt += " %s:\n" % scenario
            for location, count in iteritems(locations):
                txt += "  Agents in %s: %s\n" % (location, count)

        return txt


class MasterProvisioning(Provisioning):
    def get_rfiles(self):
        rfiles = []
        additional_files = []
        for executor in self.executors:
            executor_rfiles = executor.get_resource_files()
            config = to_json(self.engine.config.get('execution'))
            config += to_json(self.engine.config.get('scenarios'))
            config += to_json(executor.settings)
            for rfile in executor_rfiles:
                if has_variable_pattern(rfile):
                    continue

                if not os.path.exists(self.engine.find_file(rfile)):
                    raise TaurusConfigError("%s: resource file '%s' not found" % (executor, rfile))
                if to_json(rfile) not in config:  # TODO: might be check is needed to improve
                    additional_files.append(rfile)
            rfiles += executor_rfiles

        if additional_files:
            raise TaurusConfigError("Following files can't be handled in cloud: %s" % additional_files)

        rfiles = list(set(rfiles))
        rfiles = [x for x in rfiles if not has_variable_pattern(x)]
        self.log.debug("All resource files are: %s", rfiles)
        return rfiles

    def _fix_filenames(self, old_names):
        # check for concurrent base names
        old_full_names = [self.engine.find_file(x) for x in old_names]
        rbases = [os.path.basename(get_full_path(rfile)) for rfile in old_full_names]
        rpaths = [get_full_path(rfile, step_up=1) for rfile in old_full_names]
        while rbases:
            base, path = rbases.pop(), rpaths.pop()
            if base in rbases:
                index = rbases.index(base)
                if path != rpaths[index]:
                    msg = 'Resource "%s" occurs more than one time, rename to avoid data loss'
                    raise TaurusConfigError(msg % base)

        old_full_names = self.__pack_dirs(old_full_names)
        new_base_names = [os.path.basename(f) for f in old_full_names]
        self.log.debug('Replace file names in config: %s with %s', old_names, new_base_names)
        replace_in_config(self.engine.config, old_names, new_base_names, log=self.log)
        old_full_names = list(set(old_full_names))
        return old_full_names

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
            services = self.engine.config.get(Service.SERV, [], force_set=True)
            unpacker = BetterDict.from_dict({'module': Unpacker.UNPACK, Unpacker.FILES: packed_list, 'run-at': 'local'})
            services.append(unpacker)

        return result_list


class CloudProvisioning(MasterProvisioning, WidgetProvider):
    """
    :type user: bzt.bza.User
    :type router: BaseCloudTest
    :type _workspaces: bzt.bza.BZAObjectsList[bzt.bza.Workspace]
    """

    LOC = "locations"
    LOC_WEIGHTED = "locations-weighted"
    DEDICATED_IPS = "dedicated-ips"

    def __init__(self):
        super(CloudProvisioning, self).__init__()
        self.results_url = None
        self.results_reader = None
        self.user = User()
        self.__last_master_status = None
        self.browser_open = 'start'
        self.widget = None
        self.detach = False
        self.router = None
        self.test_ended = False
        self.check_interval = 5.0
        self._last_check_time = None
        self.public_report = False
        self.report_name = None
        self._workspaces = []
        self.launch_existing_test = None
        self.disallow_empty_execution = False

    @staticmethod
    def merge_with_blazemeter_config(module):
        if 'blazemeter' not in module.engine.config.get('modules'):
            module.log.debug("Module 'blazemeter' wasn't found in base config")
            return
        bm_mod = module.engine.instantiate_module('blazemeter')
        bm_settings = copy.deepcopy(bm_mod.settings)
        bm_settings.update(module.settings)
        module.settings = bm_settings

    def prepare(self):
        reporting = self.engine.config.get(Reporter.REP)

        CloudProvisioning.merge_with_blazemeter_config(self)
        CloudProvisioning.configure_client(self)
        self._workspaces = self.user.accounts().workspaces()
        if not self._workspaces:
            raise TaurusNetworkError("Your account has no active workspaces, please contact BlazeMeter support")

        self.__dump_locations_if_needed()

        super(CloudProvisioning, self).prepare()
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        self.detach = self.settings.get("detach", self.detach)
        self.check_interval = dehumanize_time(self.settings.get("check-interval", self.check_interval))
        self.public_report = self.settings.get("public-report", self.public_report)
        is_execution_empty = not self.engine.config.get("execution")
        self.launch_existing_test = self.settings.get("launch-existing-test", is_execution_empty, force_set=True)

        if not self.launch_existing_test:
            self._filter_reporting()

        finder = ProjectFinder(self.parameters, self.settings, self.user, self._workspaces, self.log)
        finder.default_test_name = "Taurus Cloud Test"

        test_type = self.settings.get("test-type")  # user test type. should we mention it in doc?
        if not test_type:
            func_mode = self.engine.is_functional_mode()
            gui_mode = func_mode and (
                    (len(self.executors) == 1) and
                    isinstance(self.executors[0], SeleniumExecutor))

            if func_mode:
                if gui_mode:
                    test_type = FUNC_GUI_TEST_TYPE
                else:
                    test_type = FUNC_API_TEST_TYPE
            else:
                test_type = TAURUS_TEST_TYPE

        finder.test_type = test_type

        self.router = finder.get_test_router()

        if not self.launch_existing_test:
            self.router.prepare_locations(self.executors, self.engine.config)

            res_files = self.get_rfiles()
            files_for_cloud = self._fix_filenames(res_files)

            config_for_cloud = self.prepare_cloud_config()
            config_for_cloud.dump(self.engine.create_artifact("cloud", ""))
            del_files = self.settings.get("delete-test-files", True)
            self.router.resolve_test(config_for_cloud, files_for_cloud, del_files)

        self.router.sanitize_test()

        self.report_name = self.settings.get("report-name", self.report_name)
        if self.report_name == 'ask' and sys.stdin.isatty():
            self.report_name = input("Please enter report-name: ")

        self.widget = self.get_widget()

        if self.engine.is_functional_mode():
            self.results_reader = FunctionalBZAReader(self.log)
            self.engine.aggregator.add_underling(self.results_reader)
        else:
            self.results_reader = ResultsFromBZA()
            self.results_reader.log = self.log
            self.engine.aggregator.add_underling(self.results_reader)

        validate_passfail = any(reporter.get('module') == 'passfail' for reporter in reporting)

        if validate_passfail:
            if self.router._test.started_passfail_validation():
                timeout = 100
                for i in range(timeout):
                    if self.router._test.get_passfail_validation():
                        return
                    self.log.warning(f"Unsuccessful Passfail validation attempt [{i+1}]. Retrying...")
                    if not i % 10:
                        self.log.warning("Please keep in mind that validation can take time.")
                    sleep(1)
                self.log.error("Unable get Passfail validation!")
            else:
                self.log.error("Unable to validate Passfail configuration!")

    @staticmethod
    def _get_other_modules(config):
        used_classes = LocalClient.__name__, BlazeMeterUploader.__name__
        used_modules = []

        for module in config.get("modules"):
            class_name = config.get("modules").get(module).get("class")
            if class_name and (class_name.split('.')[-1] in used_classes):
                used_modules.append(module)
        return used_modules

    def _get_executors(self):
        executors = []
        for executor in self.executors:
            executors.append(executor.execution.get("executor"))
            if isinstance(executor, SeleniumExecutor):
                executors.append(executor.runner.execution.get("executor"))

        return executors

    def _filter_unused_modules(self, config, provisioning):
        services = [service.get("module") for service in config.get(Service.SERV)]
        reporters = [reporter.get("module") for reporter in config.get(Reporter.REP)]
        consolidator = config.get(SETTINGS).get("aggregator")

        used_modules = self._get_executors() + self._get_other_modules(config)
        used_modules += services + reporters + [consolidator, provisioning]

        modules = set(config.get("modules").keys())
        for module in modules:
            if config.get("modules")[module].get("send-to-blazemeter"):
                continue
            if module not in used_modules:
                del config.get("modules")[module]
            elif config.get("modules")[module].get("class"):
                del config.get("modules")[module]["class"]

    def prepare_cloud_config(self):
        # expand concurrency and throughput
        for executor in self.executors:
            executor.get_load()

        config = copy.deepcopy(self.engine.config)

        provisioning = config.get(Provisioning.PROV)
        self._filter_unused_modules(config, provisioning)

        # todo: should we remove config['version'] before sending to cloud?
        config['local-bzt-version'] = config.get('version', 'N/A')

        config.filter(CLOUD_CONFIG_BLACK_LIST, black_list=True)

        for execution in config[EXEC]:
            if execution.get("files") == []:
                del execution["files"]

            for param in (ScenarioExecutor.CONCURR, ScenarioExecutor.THRPT):
                param_value = execution.get(param).get(provisioning, None)
                if param_value is None:
                    del execution[param]
                else:
                    execution[param] = param_value

        if self.router.dedicated_ips:
            config[CloudProvisioning.DEDICATED_IPS] = True

        assert isinstance(config, Configuration)
        return config

    def __dump_locations_if_needed(self):
        if self.settings.get("dump-locations", False):
            locations = {}
            for loc in self._workspaces.locations(include_private=True):
                locations[loc['id']] = loc

            data = [("ID", "Name")]
            for location_id in sorted(locations):
                location = locations[location_id]
                data.append((location_id, location['title']))
            table = SingleTable(data) if sys.stdout and sys.stdout.isatty() else AsciiTable(data)
            self.log.warning("Dumping available locations instead of running the test:\n%s", table.table)
            raise NormalShutdown("Done listing locations")

    def _filter_reporting(self):
        reporting = self.engine.config.get(Reporter.REP, [])
        new_reporting = []
        for index, reporter in enumerate(reporting):
            exc = TaurusConfigError("'module' attribute not found in %s" % reporter)
            cls = reporter.get('module', exc)
            if cls == "blazemeter":
                self.log.warning("Explicit blazemeter reporting is skipped for cloud")
            else:
                new_reporting.append(reporter)

        self.engine.config[Reporter.REP] = new_reporting

    @staticmethod
    def configure_client(module):
        module.user.log = module.log
        module.user.logger_limit = module.settings.get("request-logging-limit", module.user.logger_limit)
        module.user.address = module.settings.get("address", module.user.address)
        module.user.token = module.settings.get("token", module.user.token)
        module.user.timeout = dehumanize_time(module.settings.get("timeout", module.user.timeout))
        if isinstance(module.user.http_session, requests.Session):
            module.log.debug("Installing http client")
            module.user.http_session = module.engine.get_http_client()
            module.user.http_request = module.user.http_session.request
        if not module.user.token:
            raise TaurusConfigError("You must provide API token to use cloud provisioning")

    def startup(self):
        super(CloudProvisioning, self).startup()
        self.results_url = self.router.launch_test()
        self.log.info("Started cloud test: %s", self.results_url)
        if self.results_url:
            if self.browser_open in ('start', 'both'):
                open_browser(self.results_url)

        if self.user.token and self.public_report:
            public_link = self.router.master.make_report_public()
            self.log.info("Public report link: %s", public_link)

        if self.report_name:
            self.router.master.set({"name": str(self.report_name)})

    def _should_skip_check(self):
        now = time.time()
        if self._last_check_time is None:
            return False
        elif now >= self._last_check_time + self.check_interval:
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

        self._last_check_time = time.time()

        master = self._check_master_status()
        status = master.get('status')
        progress = master.get('progress')   # number value of status, see BZA API

        if status != self.__last_master_status:
            self.__last_master_status = status
            self.log.info("Cloud test status: %s", status)

        if self.results_reader and progress and progress >= BZA_TEST_DATA_RECEIVED:
            self.results_reader.master = self.router.master

        if progress == ENDED:
            self.log.info("Test was stopped in the cloud: %s", status)
            self.test_ended = True
            return True

        self.router.start_if_ready()

        self.widget.update()
        return super(CloudProvisioning, self).check()

    @get_with_retry
    def _check_master_status(self):
        return self.router.get_master_status()

    def post_process(self):
        self.log.warning('Part of result data might be missed here due to BM API specifics')

        if not self.detach and self.router and not self.test_ended:
            self.router.stop_test()

        if self.results_url:
            if self.browser_open in ('end', 'both'):
                open_browser(self.results_url)

        if self.router and self.router.master:
            full = self.router.master.get_full()
            if 'note' in full and full['note']:
                self.log.warning("Cloud test has probably failed with message: %s", full['note'])

            for session in full.get('sessions', ()):
                for error in session.get("errors", ()):
                    raise TaurusException(to_json(error))

            if "hasThresholds" in full and full["hasThresholds"]:
                thresholds = self.router.master.get_thresholds()
                for item in thresholds.get('data', []):
                    if item.get('success', None) is False:
                        reason = None
                        for assertion in item.get('assertions', []):
                            if assertion.get('success', None) is False:
                                criterion = assertion.get('field', '')
                                label = assertion.get('label', '')
                                reason = "Cloud failure criterion %r (on label %r) was met" % (criterion, label)
                                break
                        if reason is None:
                            reason = "Cloud tests failed because failure criteria were met"
                        self.log.warning(reason)
                        raise AutomatedShutdown(reason)

            # if we have captured HARs, let's download them
            for service in self.engine.config.get(Service.SERV, []):
                mod = service.get('module', TaurusConfigError("No 'module' specified for service"))
                assert isinstance(mod, str), mod
                module = self.engine.instantiate_module(mod)
                if isinstance(module, ServiceStubCaptureHAR):
                    self._download_logs()
                    break

            if "functionalSummary" in full:
                summary = full["functionalSummary"]
                if summary is None or summary.get("isFailed", False):
                    raise AutomatedShutdown("Cloud tests failed")

    def _download_logs(self):
        for session in self.router.master.sessions():
            assert isinstance(session, Session)
            for log in session.get_logs():
                self.log.info("Downloading %s from the cloud", log['filename'])
                cloud_dir = os.path.join(self.engine.artifacts_dir, 'cloud-artifacts')
                if not os.path.exists(cloud_dir):
                    os.makedirs(cloud_dir)
                dest = os.path.join(cloud_dir, log['filename'])
                dwn = ExceptionalDownloader(self.engine.get_http_client())
                with ProgressBarContext() as pbar:
                    try:
                        dwn.get(log['dataUrl'], dest, reporthook=pbar.download_callback)
                    except BaseException:
                        self.log.debug("Error is: %s", traceback.format_exc())
                        self.log.warning("Failed to download from %s", log['dataUrl'])
                        continue

                    if log['filename'].startswith('artifacts') and log['filename'].endswith('.zip'):
                        with zipfile.ZipFile(dest) as zipf:
                            for name in zipf.namelist():
                                ext = name.split('.')[-1].lower()
                                if ext in ('har', 'jpg', 'js', 'html', 'css'):
                                    self.log.debug("Extracting %s to %s", name, cloud_dir)
                                    zipf.extract(name, cloud_dir)

    def get_widget(self):
        if not self.widget:
            self.widget = CloudProvWidget(self.router)
        return self.widget


class ResultsFromBZA(ResultsProvider):
    """
    :type master: bzt.bza.Master
    """

    def __init__(self, master=None):
        super(ResultsFromBZA, self).__init__()
        self.master = master
        self.min_ts = 0
        self.log = logging.getLogger('')
        self.prev_errors = BetterDict()
        self.cur_errors = BetterDict()
        self.handle_errors = True

    def _get_err_diff(self):
        # find diff of self.prev_errors and self.cur_errors
        diff = {}
        for label in self.cur_errors:
            if label not in self.prev_errors:
                diff[label] = self.cur_errors[label]
                continue

            for msg in self.cur_errors[label]:
                if msg not in self.prev_errors[label]:
                    prev_count = 0
                else:
                    prev_count = self.prev_errors[label][msg]['count']

                delta = self.cur_errors[label][msg]['count'] - prev_count
                if delta > 0:
                    if label not in diff:
                        diff[label] = {}
                    diff[label][msg] = {'count': delta, 'rc': self.cur_errors[label][msg]['rc']}

        return {k: diff[k] for k in diff if diff[k]}  # clean from empty items

    def _calculate_datapoints(self, final_pass=False):
        if self.master is None:
            return

        data, aggr_raw = self.query_data()
        aggr = {}
        for label in aggr_raw:
            aggr[label['labelName']] = label

        for label in data:
            if label.get('kpis') and not final_pass:
                label['kpis'].pop(-1)  # never take last second since it could be incomplete

        timestamps = []
        for label in data:
            if label.get('label') == 'ALL':
                timestamps.extend([kpi['ts'] for kpi in label.get('kpis', [])])

        self.handle_errors = True

        for tstmp in timestamps:
            point = DataPoint(tstmp)
            point[DataPoint.SOURCE_ID] = self.master['id']
            self.__generate_kpisets(aggr, data, point, tstmp)

            if self.handle_errors:
                self.handle_errors = False
                self.cur_errors = self.__get_errors_from_bza()
                err_diff = self._get_err_diff()
                if err_diff:
                    self.__add_err_diff(point, err_diff)
                    self.prev_errors = self.cur_errors

            point.recalculate()

            self.min_ts = point[DataPoint.TIMESTAMP] + 1
            yield point

    def __add_err_diff(self, point, err_diff):
        for label in err_diff:
            point_label = '' if label == 'ALL' else label
            if point_label not in point[DataPoint.CURRENT]:
                self.log.warning("Got inconsistent kpi/error data for label: %s", point_label)
                kpiset = KPISet()
                point[DataPoint.CURRENT][point_label] = kpiset
                kpiset[KPISet.SAMPLE_COUNT] = sum([item['count'] for item in err_diff[label].values()])
            else:
                kpiset = point[DataPoint.CURRENT][point_label]

            kpiset[KPISet.ERRORS] = self.__get_kpi_errors(err_diff[label])
            kpiset[KPISet.FAILURES] = sum([x['cnt'] for x in kpiset[KPISet.ERRORS]])
            kpiset[KPISet.SAMPLE_COUNT] = kpiset[KPISet.SUCCESSES] + kpiset[KPISet.FAILURES]
            assert kpiset[KPISet.SAMPLE_COUNT] > 0, point_label

    def __generate_kpisets(self, aggr, data, point, tstmp):
        for label in data:
            for kpi in label.get('kpis', []):
                if kpi['ts'] != tstmp:
                    continue
                label_str = label.get('label')
                if label_str is None or label_str not in aggr:
                    self.log.warning("Skipping inconsistent data from API for label: %s", label_str)
                    continue

                if kpi['n'] <= 0:
                    self.log.warning("Skipping empty KPI item got from API: %s", kpi)
                    continue

                kpiset = self.__get_kpiset(aggr, kpi, label_str)
                point[DataPoint.CURRENT]['' if label_str == 'ALL' else label_str] = kpiset

    def __get_errors_from_bza(self):
        #
        # This method reads error report from BZA
        #
        # internal errors format:
        # <request_label>:
        #   <error_message>:
        #     'count': <count of errors>
        #     'rc': <response code>
        #
        result = {}
        try:
            errors = self.master.get_errors()
        except (URLError, TaurusNetworkError):
            self.log.warning("Failed to get errors, will retry in %s seconds...", self.master.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.master.timeout)
            errors = self.master.get_errors()
            self.log.info("Succeeded with retry")

        for e_record in errors:
            _id = e_record["_id"]
            if _id == "ALL":
                _id = ""
            result[_id] = {}
            for error in e_record['errors']:
                result[_id][error['m']] = {'count': error['count'], 'rc': error['rc']}
            for assertion in e_record['assertions']:
                result[_id][assertion['failureMessage']] = {'count': assertion['failures'], 'rc': assertion['name']}
        return result

    def __get_kpi_errors(self, errors):
        result = []
        for msg in errors:
            kpi_error = KPISet.error_item_skel(
                error=msg,
                ret_c=errors[msg]['rc'],
                cnt=errors[msg]['count'],
                errtype=KPISet.ERRTYPE_ERROR,  # TODO: what about asserts?
                urls=Counter(), tag=None)
            result.append(kpi_error)
        return result

    def __get_kpiset(self, aggr, kpi, label):
        kpiset = KPISet()
        kpiset[KPISet.FAILURES] = kpi['ec']
        kpiset[KPISet.CONCURRENCY] = kpi['na']
        kpiset[KPISet.SAMPLE_COUNT] = kpi['n']
        assert kpi['n'] > 0 and kpi['n'] >= kpi['ec']
        kpiset[KPISet.SUCCESSES] = kpi['n'] - kpi['ec']
        kpiset.sum_rt += kpi['t_avg'] * kpi['n'] / 1000.0
        kpiset.sum_lt += kpi['lt_avg'] * kpi['n'] / 1000.0
        perc_map = {'90line': 90.0, "95line": 95.0, "99line": 99.0}
        for field, level in iteritems(perc_map):
            kpiset[KPISet.PERCENTILES][str(level)] = aggr[label][field] / 1000.0
        return kpiset

    def query_data(self):
        try:
            data = self.master.get_kpis(self.min_ts)
        except (URLError, TaurusNetworkError):
            self.log.warning("Failed to get result KPIs, will retry in %s seconds...", self.master.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.master.timeout)
            data = self.master.get_kpis(self.min_ts)
            self.log.info("Succeeded with retry")

        try:
            aggr = self.master.get_aggregate_report()
        except (URLError, TaurusNetworkError):
            self.log.warning("Failed to get aggregate results, will retry in %s seconds...", self.master.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.master.timeout)
            aggr = self.master.get_aggregate_report()
            self.log.info("Succeeded with retry")

        return data, aggr


class FunctionalBZAReader(FunctionalResultsReader):
    def __init__(self, parent_log, master=None):
        super(FunctionalBZAReader, self).__init__()
        self.master = master
        self.log = parent_log.getChild(self.__class__.__name__)

    @staticmethod
    def extract_samples_from_group(group, group_summary):
        group_name = group_summary.get("name") or "Tests"
        for sample in group["samples"]:
            status = "PASSED"
            if sample["error"]:
                status = "FAILED"
            error_msg = ""
            error_trace = ""
            assertions = sample.get("assertions")
            if assertions:
                for assertion in assertions:
                    if assertion.get("isFailed"):
                        error_msg = assertion.get("errorMessage")
                        status = "BROKEN"

            rtm = sample.get("responseTime") or 0.0
            yield FunctionalSample(
                test_case=sample["label"],
                test_suite=group_name,
                status=status,
                start_time=int(sample["created"]),
                duration=rtm / 1000.0,
                error_msg=error_msg,
                error_trace=error_trace,
                extras={},
                subsamples=[],
            )

    def read(self, last_pass=False):
        if self.master is None:
            return

        if last_pass:
            try:
                groups = self.master.get_functional_report_groups()
            except (URLError, TaurusNetworkError):
                self.log.warning("Failed to get test groups, will retry in %s seconds...", self.master.timeout)
                self.log.debug("Full exception: %s", traceback.format_exc())
                time.sleep(self.master.timeout)
                groups = self.master.get_functional_report_groups()
                self.log.info("Succeeded with retry")

            for group_summary in groups:
                group_id = group_summary['groupId']
                try:
                    group = self.master.get_functional_report_group(group_id)
                except (URLError, TaurusNetworkError):
                    self.log.warning("Failed to get test group, will retry in %s seconds...", self.master.timeout)
                    self.log.debug("Full exception: %s", traceback.format_exc())
                    time.sleep(self.master.timeout)
                    group = self.master.get_functional_report_group(group_id)
                    self.log.info("Succeeded with retry")

                for sample in self.extract_samples_from_group(group, group_summary):
                    yield sample


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
