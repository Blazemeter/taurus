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
import json
import logging
import os
import urllib
import urllib2
import sys
import traceback
import time
import StringIO
from urllib2 import HTTPError
import webbrowser
import zipfile

from bzt import ManualShutdown
from bzt.engine import Reporter, AggregatorListener
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.jmeter import JMeterExecutor
from bzt.utils import to_json, dehumanize_time, MultiPartForm


class BlazeMeterUploader(Reporter, AggregatorListener):
    """
    Reporter class

    :type client: BlazeMeterClient
    """

    def __init__(self):
        super(BlazeMeterUploader, self).__init__()
        self.browser_open = 'start'
        self.client = BlazeMeterClient(self.log)
        self.test = "Taurus Test"
        self.test_id = None
        self.kpi_buffer = []
        self.bulk_size = 5

    def prepare(self):
        """
        Read options for uploading, check that they're sane
        """
        super(BlazeMeterUploader, self).prepare()
        self.client.address = self.settings.get("address", "https://a.blazemeter.com")
        self.client.data_address = self.settings.get("data-address", "https://data.blazemeter.com")
        self.client.timeout = dehumanize_time(self.settings.get("timeout", self.client.timeout))
        self.bulk_size = self.settings.get("bulk-size", self.bulk_size)
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        token = self.settings.get("token", "")
        if not token:
            self.log.warning("No BlazeMeter API key provided, will upload anonymously")
        self.client.token = token

        self.test = self.parameters.get("test", self.test)  # TODO: provide a way to put datetime into test name
        try:
            self.client.ping()  # to check connectivity and auth
            if token:
                self.test_id = self.client.test_by_name(self.test)
        except HTTPError:
            self.log.error("Cannot reach online results storage, maybe the address/token is wrong")
            raise

    def startup(self):
        """
        Initiate online test
        """
        super(BlazeMeterUploader, self).startup()
        try:
            url = self.client.start_online(self.test_id)
            self.log.info("Started data feeding: %s", url)
            if self.browser_open in ('start', 'both'):
                webbrowser.open(url)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            self.log.debug("Exception: %s", traceback.format_exc(exc))
            self.log.warn("Failed to start feeding: %s", exc)

    def __get_jtls_and_more(self):
        mf = StringIO.StringIO()
        with zipfile.ZipFile(mf, mode='w', compression=zipfile.ZIP_DEFLATED, allowZip64=True) as zfh:
            for handler in self.engine.log.parent.handlers:
                if isinstance(handler, logging.FileHandler):
                    zfh.write(handler.baseFilename, os.path.basename(handler.baseFilename))

            for root, dirs, files in os.walk(self.engine.artifacts_dir):
                for filename in files:
                    zfh.write(os.path.join(root, filename), filename)
        return mf

    def __upload_artifacts(self):
        if self.client.token:
            self.log.info("Uploading all artifacts as jtls_and_more.zip ...")
            mf = self.__get_jtls_and_more()
            self.client.upload_file("jtls_and_more.zip", mf.getvalue())

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
        except IOError as exc:
            self.log.warn("Failed artifact upload: %s", traceback.format_exc(exc))
        finally:
            try:
                self.client.end_online()
            except KeyboardInterrupt:
                raise
            except BaseException as exc:
                self.log.warn("Failed to finish online: %s", exc)

        if self.client.results_url:
            if self.browser_open in ('end', 'both'):
                webbrowser.open(self.client.results_url)
            self.log.info("Online report link: %s", self.client.results_url)

    def check(self):
        """
        Send data if any in buffer

        :return:
        """
        if len(self.kpi_buffer):
            if len(self.kpi_buffer) >= self.bulk_size:
                self.__send_data(self.kpi_buffer)
                self.kpi_buffer = []
        return super(BlazeMeterUploader, self).check()

    def __send_data(self, data, do_check=True):
        if self.client.active_session_id:
            try:
                self.client.send_kpi_data(data, do_check)
            except IOError as exc:
                self.log.debug("Error sending data: %s", traceback.format_exc(exc))
                self.log.warn("Failed to send data, will retry in %s sec...", self.client.timeout)
                try:
                    time.sleep(self.client.timeout)
                    self.client.send_kpi_data(data, do_check)
                    self.log.info("Succeeded with retry")
                except IOError as exc:
                    self.log.error("Fatal error sending data: %s", traceback.format_exc(exc))
                    self.log.warn("Will skip failed data and continue running")

            try:
                self.client.send_error_summary(data)
            except IOError as exc:
                self.log.debug("Failed sending error summary: %s", traceback.format_exc(exc))
                self.log.warn("Failed to send error summary: %s", exc)

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
        self.logger_limit = 512  # TODO: provide control over it
        self.user_id = None
        self.test_id = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.token = None
        self.address = None
        self.data_address = None
        self.results_url = None
        self.active_session_id = None
        self.data_signature = None
        self._first = sys.maxint
        self._last = 0
        self.timeout = 5

    def _request(self, url, data=None, headers=None, checker=None):
        if not headers:
            headers = {}
        headers["X-API-Key"] = self.token
        self.log.debug("Request %s: %s", url, data[:self.logger_limit] if data else None)
        request = urllib2.Request(url, data, headers)

        response = urllib2.urlopen(request, timeout=self.timeout)

        if checker:
            checker(response)

        resp = response.read()
        self.log.debug("Response: %s", resp[:self.logger_limit] if resp else None)
        return json.loads(resp) if len(resp) else {}

    def start_online(self, test_id):
        """
        Start online test

        :type test_id: str
        :return:
        """
        self.log.info("Initiating data feeding...")
        data = urllib.urlencode({})

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
        else:
            self.test_id = resp['result']['session']['testId']
            self.results_url = resp['result']['publicTokenUrl']
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

    def test_by_name(self, name):
        """

        :type name: str
        :rtype: str
        """
        tests = self.get_tests()
        test_id = None
        for test in tests:
            self.log.debug("Test: %s", test)
            if "name" in test and test['name'] == name and test['configuration']['type'] == 'external':
                test_id = test['id']

        if not test_id:
            url = self.address + '/api/latest/tests'
            data = {"name": name, "configuration": {"type": "external"}}
            hdr = {"Content-Type": " application/json"}
            resp = self._request(url, json.dumps(data), headers=hdr)
            test_id = resp['result']['id']

        self.log.debug("Using test ID: %s", test_id)
        return test_id

    def get_tests(self):
        """

        :rtype: list
        """
        tests = self._request(self.address + '/api/latest/tests')
        self.log.debug("Tests for user: %s", tests['result'])
        return tests['result']

    def send_kpi_data(self, data_buffer, is_check_response=True):
        """
        Sends online data

        :param is_check_response:
        :type data_buffer: list[bzt.modules.aggregator.DataPoint]
        """
        data = []

        for sec in data_buffer:
            self._first = min(self._first, sec[DataPoint.TIMESTAMP])
            self._last = max(self._last, sec[DataPoint.TIMESTAMP])

            for lbl, item in sec[DataPoint.CURRENT].iteritems():
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
                for rc, cnt in item[KPISet.RESP_CODES].iteritems():
                    interval_item['rc'].append({"n": cnt, "rc": rc})

                json_item['intervals'].append(interval_item)

                cumul = sec[DataPoint.CUMULATIVE][lbl]
                json_item['n'] = cumul[KPISet.SAMPLE_COUNT]
                json_item["summary"] = self.__summary_json(cumul)

                """
                for err, cnt in item[KPISet.ERRORS].iteritems():
                    json_item['errors'].append({
                        "m": err[1],
                        "rc": err[0],
                        "count": cnt,
                    })
                """

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
            "first": self._first,
            "last": self._last,
            "duration": self._last - self._first,
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
                "min": 0,
                "max": 0,
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
        self._request(self.address + '/api/latest/explorer/resources.json')

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
        response = self._request(url, str(body), headers=hdr)
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
        for label, label_data in recent[DataPoint.CUMULATIVE].iteritems():
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

    def __errors_skel(self, ts, sess_id, test_id, user_id):
        return {
            "reportInfo": {
                "sessionId": sess_id,
                "timestamp": ts,
                "userId": user_id,
                "testId": test_id,
                "type": "SUMMERY",
                # "testName": test_name
            },
            "timestamp": ts,
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