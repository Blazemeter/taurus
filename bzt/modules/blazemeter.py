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
import sys
import traceback
import time
import webbrowser
import zipfile
import math

from urwid import Pile, Text

from bzt import ManualShutdown
from bzt.engine import Reporter, AggregatorListener, Provisioning
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator, ResultsProvider
from bzt.modules.console import WidgetProvider
from bzt.modules.jmeter import JMeterExecutor
from bzt.utils import to_json, dehumanize_time, MultiPartForm, BetterDict
from bzt.six import BytesIO, text_type, iteritems, HTTPError, urlencode, Request, urlopen, r_input


class BlazeMeterUploader(Reporter, AggregatorListener):
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

    def prepare(self):
        """
        Read options for uploading, check that they're sane
        """
        super(BlazeMeterUploader, self).prepare()
        self.client.address = self.settings.get("address", self.client.address)
        self.client.data_address = self.settings.get("data-address", self.client.data_address)
        self.client.timeout = dehumanize_time(self.settings.get("timeout", self.client.timeout))
        self.send_interval = dehumanize_time(self.settings.get("send-interval", self.send_interval))
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        token = self.settings.get("token", "")
        if not token:
            self.log.warning("No BlazeMeter API key provided, will upload anonymously")
        self.client.token = token

        self.client.active_session_id = self.parameters.get("session-id", None)
        self.client.test_id = self.parameters.get("test-id", None)
        self.client.user_id = self.parameters.get("user-id", None)
        self.client.data_signature = self.parameters.get("signature", None)

        if not self.client.test_id:
            try:
                self.client.ping()  # to check connectivity and auth
            except HTTPError:
                self.log.error("Cannot reach online results storage, maybe the address/token is wrong")
                raise

            self.__get_test_id(token)

        self.sess_name = self.parameters.get("report-name", self.settings.get("report-name", self.sess_name))
        if self.sess_name == 'ask' and sys.stdin.isatty():
            self.sess_name = r_input("Please enter report-name: ")

    def __get_test_id(self, token):
        if not token:
            return

        proj_name = self.parameters.get("project", self.settings.get("project", None))
        if isinstance(proj_name, (int, float)):
            proj_id = int(proj_name)
            self.log.debug("Treating project name as ID: %s", proj_id)
        elif proj_name is not None:
            proj_id = self.client.project_by_name(proj_name)
        else:
            proj_id = None

        test_name = self.parameters.get("test", self.settings.get("test", "Taurus Test"))
        self.test_id = self.client.test_by_name(test_name, {"type": "external"}, self.engine.config, [], proj_id)

    def startup(self):
        """
        Initiate online test
        """
        super(BlazeMeterUploader, self).startup()

        if not self.client.active_session_id:
            try:
                url = self.client.start_online(self.test_id, self.sess_name)
                self.log.info("Started data feeding: %s", url)
                if self.browser_open in ('start', 'both'):
                    webbrowser.open(url)
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
        max_file_size = self.settings.get('artifact-upload-size-limit', 10) * 1048576  # 10MB
        with zipfile.ZipFile(mfile, mode='w', compression=zipfile.ZIP_DEFLATED, allowZip64=True) as zfh:
            for handler in self.engine.log.parent.handlers:
                if isinstance(handler, logging.FileHandler):
                    zfh.write(handler.baseFilename, os.path.basename(handler.baseFilename))

            for root, _dirs, files in os.walk(self.engine.artifacts_dir):
                for filename in files:
                    if os.path.getsize(os.path.join(root, filename)) <= max_file_size:
                        zfh.write(os.path.join(root, filename),
                                  os.path.join(os.path.relpath(root, self.engine.artifacts_dir), filename))
                    else:
                        self.log.warning("File %s exceeded maximum size quota of %s and won't be included into upload",
                                         filename, max_file_size)
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
        super(BlazeMeterUploader, self).post_process()

        if not self.client.active_session_id:
            self.log.debug("No feeding session obtained, not uploading artifacts")
            return

        if len(self.kpi_buffer):
            self.__send_data(self.kpi_buffer, False)
            self.kpi_buffer = []

        try:
            self.__upload_artifacts()
        except IOError as _:
            self.log.warning("Failed artifact upload: %s", traceback.format_exc())
        finally:
            try:
                self.client.end_online()
            except KeyboardInterrupt:
                raise
            except BaseException as exc:
                self.log.warning("Failed to finish online: %s", exc)

        if self.client.results_url:
            if self.browser_open in ('end', 'both'):
                webbrowser.open(self.client.results_url)
            self.log.info("Online report link: %s", self.client.results_url)

    def check(self):
        """
        Send data if any in buffer

        :return:
        """
        self.log.debug("KPI bulk buffer len: %s", len(self.kpi_buffer))
        if len(self.kpi_buffer):
            if self.client.last_ts < (time.time() - self.send_interval):
                self.__send_data(self.kpi_buffer)
                self.kpi_buffer = []
        return super(BlazeMeterUploader, self).check()

    def __send_data(self, data, do_check=True):
        """
        :param data: list[bzt.modules.aggregator.DataPoint]
        :return:
        """
        if self.client.active_session_id:
            try:
                self.client.send_kpi_data(data, do_check)
            except IOError as _:
                self.log.debug("Error sending data: %s", traceback.format_exc())
                self.log.warning("Failed to send data, will retry in %s sec...", self.client.timeout)
                try:
                    time.sleep(self.client.timeout)
                    self.client.send_kpi_data(data, do_check)
                    self.log.info("Succeeded with retry")
                except IOError as _:
                    self.log.error("Fatal error sending data: %s", traceback.format_exc())
                    self.log.warning("Will skip failed data and continue running")

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


class BlazeMeterClient(object):
    """ Service client class """

    def __init__(self, parent_logger):
        self.logger_limit = 512  # NOTE: provide control over it
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

    def _request(self, url, data=None, headers=None, checker=None, method=None):
        if not headers:
            headers = {}
        if self.token:
            headers["X-API-Key"] = self.token

        log_method = 'GET' if data is None else 'POST'
        if method:
            log_method = method

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
        return json.loads(resp) if len(resp) else {}

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
        self.log.info("Initiating cloud test...")
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

        if configuration['type'] == 'taurus':  # FIXME: this is weird way to code
            self.log.debug("Uploading files into the test")
            url = '%s/api/latest/tests/%s/files' % (self.address, test_id)

            body = MultiPartForm()

            body.add_file_as_string('script', 'taurus.json', to_json(taurus_config))

            for rfile in resource_files:
                body.add_file('files[]', rfile)

            hdr = {"Content-Type": body.get_content_type()}
            response = self._request(url, body.form_as_bytes(), headers=hdr)

        self.log.debug("Using test ID: %s", test_id)
        return test_id

    def get_tests(self):
        """

        :rtype: list
        """
        tests = self._request(self.address + '/api/latest/tests')
        self.log.debug("Tests for user: %s", len(tests['result']))
        return tests['result']

    def send_kpi_data(self, data_buffer, is_check_response=True):
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

        data = {"labels": data}

        url = self.data_address + "/submit.php?session_id=%s&signature=%s&test_id=%s&user_id=%s"
        url = url % (self.active_session_id, self.data_signature, self.test_id, self.user_id)
        url += "&pq=0&target=labels_bulk&update=1"
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
        hdr = {"Content-Type": body.get_content_type()}
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


class CloudProvisioning(Provisioning, WidgetProvider):
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
        self.__last_master_status = None
        self.browser_open = 'start'

    def prepare(self):
        super(CloudProvisioning, self).prepare()
        self.browser_open = self.settings.get("browser-open", self.browser_open)

        # TODO: go to "blazemeter" section for these settings by default?
        self.client.address = self.settings.get("address", self.client.address)
        self.client.token = self.settings.get("token", self.client.token)
        self.client.timeout = dehumanize_time(self.settings.get("timeout", self.client.timeout))

        if not self.client.token:
            raise ValueError("You must provide API token to use cloud provisioning")

        user_info = self.client.get_user_info()
        available_locations = {str(x['id']): x for x in user_info['locations']}

        for executor in self.executors:
            locations = self._get_locations(available_locations, executor)

            for location in locations.keys():
                if location not in available_locations:
                    self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                    raise ValueError("Invalid location requested: %s" % location)

            if executor.parameters.get("locations-weighted", True):
                self.weight_locations(locations, executor.get_load(), available_locations)

        config = copy.deepcopy(self.engine.config)
        if Provisioning.PROV in config:
            config.pop(Provisioning.PROV)

        rfiles = self.__get_rfiles()
        bza_plugin = self.__get_bza_test_config()
        test_name = self.settings.get("test-name", "Taurus Test")
        self.test_id = self.client.test_by_name(test_name, bza_plugin, config, rfiles, None)  # FIXME: set project id

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.results_reader = ResultsFromBZA(self.client)
            self.engine.aggregator.add_underling(self.results_reader)

    def __get_rfiles(self):
        rfiles = []
        for executor in self.executors:
            rfiles += executor.get_resource_files()
        return rfiles

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
                if location['sandbox']:
                    locations.merge({location['id']: 1})
        if not locations:
            raise ValueError("No sandbox location available, please specify locations manually")
        return locations

    def startup(self):
        super(CloudProvisioning, self).startup()
        self.client.start_taurus(self.test_id)
        self.log.info("Started cloud test: %s", self.client.results_url)
        if self.client.results_url:
            if self.browser_open in ('start', 'both'):
                webbrowser.open(self.client.results_url)

    def check(self):
        # TODO: throttle down requests
        master = self.client.get_master_status(self.client.active_session_id)
        if "status" in master and master['status'] != self.__last_master_status:
            self.__last_master_status = master['status']
            self.log.info("Cloud test status: %s", self.__last_master_status)

        if self.results_reader is not None and 'progress' in master and master['progress'] >= 100:
            self.results_reader.master_id = self.client.active_session_id

        if 'progress' in master and master['progress'] > 100:
            self.log.info("Test was stopped in the cloud: %s", master['status'])
            self.client.active_session_id = None
            return True

        return super(CloudProvisioning, self).check()

    def shutdown(self):
        self.client.end_master(self.client.active_session_id)
        if self.client.results_url:
            if self.browser_open in ('end', 'both'):
                webbrowser.open(self.client.results_url)

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
        return CloudProvWidget(self)


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

    def _calculate_datapoints(self, final_pass=False):
        if self.master_id is None:
            return

        data = self.client.get_kpis(self.master_id, self.min_ts)
        for label in data:
            if label['kpis']:
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
                    point[DataPoint.CURRENT]['' if label['label'] == 'ALL' else label['label']] = kpiset

            point.recalculate()
            self.min_ts = point[DataPoint.TIMESTAMP] + 1
            yield point


class CloudProvWidget(Pile):
    def __init__(self, prov):
        """
        :type prov: CloudProvisioning
        """
        self.prov = prov
        self.text = Text("")
        super(CloudProvWidget, self).__init__([self.text])

    def render(self, size, focus=False):
        txt = "Cloud test #%s\n" % self.prov.client.active_session_id
        cnt = 0
        for executor in self.prov.executors:
            cnt += 1
            name = executor.execution.get("executor", ValueError("Execution type is not yet defined"))
            txt += "  %s. %s" % (cnt, name)
            txt += " machines:\n"
            locations = executor.execution.get("locations")
            for location in sorted(locations.keys()):
                txt += "    %s: %s\n" % (location, locations[location])
        self.text.set_text(txt)
        return super(CloudProvWidget, self).render(size, focus)
