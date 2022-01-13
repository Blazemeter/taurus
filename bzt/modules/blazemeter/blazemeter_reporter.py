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
import sys
import time
import traceback
import zipfile
from collections import defaultdict, OrderedDict
from io import BytesIO
from urllib.error import HTTPError

import requests

from bzt import TaurusInternalException, TaurusConfigError, TaurusNetworkError
from bzt.bza import User, Session, Test
from bzt.engine import Reporter, Singletone
from bzt.utils import b, humanize_bytes, iteritems, open_browser, BetterDict, to_json, dehumanize_time
from bzt.modules.aggregator import AggregatorListener, DataPoint, KPISet, ResultsProvider, ConsolidatingAggregator
from bzt.modules.monitoring import Monitoring, MonitoringListener
from bzt.modules.blazemeter.project_finder import ProjectFinder
from bzt.modules.blazemeter.const import NOTE_SIZE_LIMIT


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

    def __send_data(self, data, do_check=True, is_final=False):
        """
        :type data: list[bzt.modules.aggregator.DataPoint]
        """
        if not self._session:
            return

        self.engine.aggregator.converter(data)
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
        #   every interval contains info about response codes that were received on it.
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
            if report_items:
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
