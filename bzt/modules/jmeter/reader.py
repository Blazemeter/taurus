"""
Module holds results readers for JMeter executor

Copyright 2017 BlazeMeter Inc.

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
import csv
import traceback
from collections import Counter

from cssselect import GenericTranslator

from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet
from bzt.modules.functional import FunctionalResultsReader, FunctionalSample
from bzt.six import PY2, iteritems, StringIO, etree, unicode_decode
from bzt.utils import BetterDict, guess_csv_dialect, FileReader


class JTLReader(ResultsReader):
    """
    Class to read KPI JTL
    :type errors_reader: JTLErrorsReader
    """

    def __init__(self, filename, parent_logger, errors_filename=None):
        super(JTLReader, self).__init__()
        self.is_distributed = False
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.csvreader = IncrementalCSVReader(self.log, filename)
        self.read_records = 0
        if errors_filename:
            self.errors_reader = JTLErrorsReader(errors_filename, parent_logger)
        else:
            self.errors_reader = None

    def _read(self, final_pass=False):
        """
        Generator method that returns next portion of data

        :type final_pass: bool
        """
        if self.errors_reader:
            self.errors_reader.read_file(final_pass)

        for row in self.csvreader.read(final_pass):
            label = unicode_decode(row["label"])
            if self.is_distributed:
                concur = int(row["grpThreads"])
                trname = row["Hostname"] + row["threadName"][:row["threadName"].rfind('-')]
            else:
                concur = int(row["allThreads"])
                trname = ''

            rtm = int(row["elapsed"]) / 1000.0
            ltc = int(row["Latency"]) / 1000.0
            if "Connect" in row:
                cnn = int(row["Connect"]) / 1000.0
                if cnn < ltc:  # this is generally bad idea...
                    ltc -= cnn  # fixing latency included into connect time
            else:
                cnn = None

            rcd = row["responseCode"]
            if rcd.endswith('Exception'):
                rcd = rcd.split('.')[-1]

            if row["success"] != "true":
                error = row["responseMessage"]
            else:
                error = None

            byte_count = int(row.get("bytes", 0))

            tstmp = int(int(row["timeStamp"]) / 1000)
            self.read_records += 1
            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _calculate_datapoints(self, final_pass=False):
        for point in super(JTLReader, self)._calculate_datapoints(final_pass):
            if self.errors_reader:
                data = self.errors_reader.get_data(point[DataPoint.TIMESTAMP])
                for label, label_data in iteritems(point[DataPoint.CURRENT]):
                    if label in data:
                        label_data[KPISet.ERRORS] = data[label]
                    else:
                        label_data[KPISet.ERRORS] = {}

            yield point


class FuncJTLReader(FunctionalResultsReader):
    """
    Class to read trace.jtl
    :type filename: str
    :type parent_logger: logging.Logger
    """

    FILE_EXTRACTED_FIELDS = ["requestBody", "responseBody", "requestCookiesRaw"]

    def __init__(self, filename, engine, parent_logger):
        super(FuncJTLReader, self).__init__()
        self.executor_label = "JMeter"
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.parser = etree.XMLPullParser(events=('end',), recover=True)
        self.engine = engine
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.failed_processing = False
        self.read_records = 0

    def read(self, last_pass=True):
        """
        Read the next part of the file
        """
        if self.failed_processing:
            return

        self.__read_next_chunk(last_pass)

        for _, elem in self.parser.read_events():
            if elem.getparent() is not None and elem.getparent().tag == 'testResults':
                sample = self._extract_sample(elem)
                self.read_records += 1

                elem.clear()
                while elem.getprevious() is not None:
                    del elem.getparent()[0]

                yield sample

    def __read_next_chunk(self, last_pass):
        while not self.failed_processing:
            read = self.file.get_bytes(size=1024 * 1024)
            if not read or not read.strip():
                break

            try:
                self.parser.feed(read)
            except etree.XMLSyntaxError as exc:
                self.failed_processing = True
                self.log.debug("Error reading trace.jtl: %s", traceback.format_exc())
                self.log.warning("Failed to parse errors XML: %s", exc)

            if not last_pass:
                break

    def _write_sample_data(self, filename, contents):
        artifact = self.engine.create_artifact(filename, ".bin")
        with open(artifact, 'wb') as fds:
            fds.write(contents.encode('utf-8'))
        return artifact

    @staticmethod
    def _extract_sample_assertions(sample_elem):
        assertions = []
        for result in sample_elem.findall("assertionResult"):
            name = result.findtext("name")
            failed = result.findtext("failure") == "true" or result.findtext("error") == "true"
            error_message = ""
            if failed:
                error_message = result.findtext("failureMessage")
            assertions.append({"name": name, "isFailed": failed, "errorMessage": error_message})
        return assertions

    def _parse_http_headers(self, header_str):
        headers = {}
        for line in header_str.split("\n"):
            clean_line = line.strip()
            if ":" in clean_line:
                key, value = clean_line.split(":", 1)
                headers[key] = value
        return headers

    def _parse_http_cookies(self, cookie_str):
        cookies = {}
        clean_line = cookie_str.strip()
        if "; " in clean_line:
            for item in clean_line.split("; "):
                key, value = item.split("=", 1)
                cookies[key] = value
        return cookies

    def _extract_sample_extras(self, sample_elem):
        method = sample_elem.findtext("method")
        uri = sample_elem.findtext("java.net.URL")  # smells like Java automarshalling
        req_headers = sample_elem.findtext("requestHeader") or ""
        resp_headers = sample_elem.findtext("responseHeader") or ""
        req_cookies = sample_elem.findtext("cookies") or ""

        thread_id = sample_elem.get("tn")
        split = thread_id.split("-")
        thread_group = "-".join(split[:-1])

        sample_extras = {
            "responseCode": sample_elem.get("rc"),
            "responseMessage": sample_elem.get("rm"),
            "responseTime": int(sample_elem.get("t") or 0),
            "connectTime": int(sample_elem.get("ct") or 0),
            "latency": int(sample_elem.get("lt") or 0),
            "responseSize": int(sample_elem.get("by") or 0),
            "requestSize": int(sample_elem.get("sby") or 0),
            "requestMethod": method,
            "requestURI": uri,

            "threadId": thread_id,
            "threadGroup": thread_group,

            "assertions": self._extract_sample_assertions(sample_elem),
            "requestHeaders": self._parse_http_headers(req_headers),
            "responseHeaders": self._parse_http_headers(resp_headers),
            "requestCookies": self._parse_http_cookies(req_cookies),

            "requestBody": sample_elem.findtext("queryString") or "",
            "responseBody": sample_elem.findtext("responseData") or "",
            "requestCookiesRaw": req_cookies,
        }

        sample_extras["requestBodySize"] = len(sample_extras["requestBody"])
        sample_extras["responseBodySize"] = len(sample_extras["responseBody"])
        sample_extras["requestCookiesSize"] = len(sample_extras["requestCookiesRaw"])

        return sample_extras

    def __write_sample_data_to_artifacts(self, sample_extras):
        for file_field in self.FILE_EXTRACTED_FIELDS:
            contents = sample_extras.pop(file_field)
            if contents:
                filename = "sample-%s" % file_field
                artifact = self._write_sample_data(filename, contents)
                sample_extras[file_field] = artifact

    def _extract_sample(self, sample_elem):
        tstmp = int(float(sample_elem.get("ts")) / 1000)
        label = sample_elem.get("lb")
        duration = float(sample_elem.get("t")) / 1000.0
        success = sample_elem.get("s") == "true"

        if success:
            status = "PASSED"
            error_msg = ""
            error_trace = ""
        else:
            assertion = self.__get_failed_assertion(sample_elem)
            if assertion is not None:
                status = "FAILED"
                error_msg = assertion.find("failureMessage").text
                error_trace = ""
            else:
                status = "BROKEN"
                error_msg, error_trace = self.get_failure(sample_elem)

        if error_msg.startswith("The operation lasted too long"):
            error_msg = "The operation lasted too long"

        sample_extras = self._extract_sample_extras(sample_elem)
        self.__write_sample_data_to_artifacts(sample_extras)

        return FunctionalSample(test_case=label, test_suite=self.executor_label, status=status,
                                start_time=tstmp, duration=duration,
                                error_msg=error_msg, error_trace=error_trace,
                                extras=sample_extras, subsamples=[])

    def get_failure(self, element):
        """
        Returns failure message and a stack trace
        """
        r_code = element.get('rc')
        if r_code and r_code.startswith("2") and element.get('s') == "false":
            children = [elem for elem in element.iterchildren() if elem.tag == "httpSample"]
            for child in children:
                child_failure = self.get_failure(child)
                if child_failure:
                    return child_failure
        else:
            message = element.get('rm')
            response_data = element.find("responseData")
            if response_data is not None:
                trace = response_data.text
            else:
                trace = ""
            return message, trace

    @staticmethod
    def __get_failed_assertion(element):
        """
        Returns first failed assertion, or None

        :rtype lxml.etree.Element
        """
        assertions = [elem for elem in element.iterchildren() if elem.tag == "assertionResult"]
        for assertion in assertions:
            failed = assertion.find("failure")
            error = assertion.find("error")
            if failed.text == "true" or error.text == "true":
                return assertion
        return None


class IncrementalCSVReader(object):
    """
    JTL csv reader
    """

    def __init__(self, parent_logger, filename):
        self.buffer = StringIO()
        self.csv_reader = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.indexes = {}
        self.partial_buffer = ""
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.read_speed = 1024 * 1024

    def read(self, last_pass=False):
        """
        read data from jtl
        yield csv row
        :type last_pass: bool
        """
        lines = self.file.get_lines(size=self.read_speed, last_pass=last_pass)

        lines_read = 0
        bytes_read = 0

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            lines_read += 1
            bytes_read += len(line)

            if self.csv_reader is None:
                dialect = guess_csv_dialect(line, force_doublequote=True)  # TODO: configurable doublequoting?
                self.csv_reader = csv.DictReader(self.buffer, [], dialect=dialect)
                self.csv_reader.fieldnames += line.strip().split(self.csv_reader.dialect.delimiter)
                self.log.debug("Analyzed header line: %s", self.csv_reader.fieldnames)
                continue

            if PY2: # todo: fix csv parsing of unicode strings on PY2
                line = line.encode('utf-8')

            self.buffer.write(line)

        if lines_read:
            self.log.debug("Read: %s lines / %s bytes (at speed %s)", lines_read, bytes_read, self.read_speed)
            self._tune_speed(bytes_read)

            self.buffer.seek(0)
            for row in self.csv_reader:
                yield row

            self.buffer.seek(0)
            self.buffer.truncate(0)

    def _tune_speed(self, bytes_read):
        if bytes_read >= self.read_speed:
            self.read_speed = min(8 * 1024 * 1024, self.read_speed * 2)
        elif bytes_read < self.read_speed / 2:
            self.read_speed = max(self.read_speed / 2, 1024 * 1024)


class JTLErrorsReader(object):
    """
    Reader for errors.jtl, which is in XML max-verbose format

    :type filename: str
    :type parent_logger: logging.Logger
    """
    assertionMessage = GenericTranslator().css_to_xpath("assertionResult>failureMessage")
    url_xpath = GenericTranslator().css_to_xpath("java\\.net\\.URL")

    def __init__(self, filename, parent_logger):
        # http://stackoverflow.com/questions/9809469/python-sax-to-lxml-for-80gb-xml/9814580#9814580
        super(JTLErrorsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.parser = etree.XMLPullParser(events=('end',))
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.buffer = BetterDict()
        self.failed_processing = False

    def read_file(self, final_pass=False):
        """
        Read the next part of the file
        """
        while not self.failed_processing:
            read = self.file.get_bytes(size=1024 * 1024)
            if not read or not read.strip():
                break

            try:
                self.parser.feed(read)  # "Huge input lookup" error without capping :)
            except etree.XMLSyntaxError as exc:
                self.failed_processing = True
                self.log.debug("Error reading errors.jtl: %s", traceback.format_exc())
                self.log.warning("Failed to parse errors XML: %s", exc)

            for _, elem in self.parser.read_events():
                if elem.getparent() is not None and elem.getparent().tag == 'testResults':
                    self._parse_element(elem)
                    elem.clear()  # cleanup processed from the memory
                    while elem.getprevious() is not None:
                        del elem.getparent()[0]

            if not final_pass:
                break

    def _parse_element(self, elem):
        if elem.get('s'):
            result = elem.get('s')
        else:
            result = elem.xpath('success')[0].text
        if result == 'false':
            if elem.items():
                self._extract_standard(elem)
            else:
                self._extract_nonstandard(elem)

    def get_data(self, max_ts):
        """
        Get accumulated errors data up to specified timestamp
        """
        result = BetterDict()
        for t_stamp in sorted(self.buffer.keys()):
            if t_stamp > max_ts:
                break
            labels = self.buffer.pop(t_stamp)
            for label, label_data in iteritems(labels):
                res = result.get(label, [])
                for err_item in label_data:
                    KPISet.inc_list(res, ('msg', err_item['msg']), err_item)

        return result

    def _extract_standard(self, elem):
        t_stamp = int(elem.get("ts")) / 1000
        label = elem.get("lb")
        r_code = elem.get("rc")
        urls = elem.xpath(self.url_xpath)
        if urls:
            url = Counter({urls[0].text: 1})
        else:
            url = Counter()
        errtype = KPISet.ERRTYPE_ERROR

        failed_assertion = self.__get_failed_assertion(elem)
        if failed_assertion is not None:
            errtype = KPISet.ERRTYPE_ASSERT

        message = self.get_failure_message(elem)
        if message is None:
            message = elem.get('rm')
        err_item = KPISet.error_item_skel(message, r_code, 1, errtype, url)
        KPISet.inc_list(self.buffer.get(t_stamp).get(label, []), ("msg", message), err_item)
        KPISet.inc_list(self.buffer.get(t_stamp).get('', []), ("msg", message), err_item)

    def _extract_nonstandard(self, elem):
        t_stamp = int(self.__get_child(elem, 'timeStamp')) / 1000  # NOTE: will it be sometimes EndTime?
        label = self.__get_child(elem, "label")
        message = self.__get_child(elem, "responseMessage")
        r_code = self.__get_child(elem, "responseCode")

        urls = elem.xpath(self.url_xpath)
        if urls:
            url = Counter({urls[0].text: 1})
        else:
            url = Counter()
        errtype = KPISet.ERRTYPE_ERROR
        massert = elem.xpath(self.assertionMessage)
        if massert:
            errtype = KPISet.ERRTYPE_ASSERT
            message = massert[0].text
        err_item = KPISet.error_item_skel(message, r_code, 1, errtype, url)
        KPISet.inc_list(self.buffer.get(t_stamp).get(label, []), ("msg", message), err_item)
        KPISet.inc_list(self.buffer.get(t_stamp).get('', []), ("msg", message), err_item)

    def get_failure_message(self, element):
        """
        Returns failure message
        """

        failed_assertion = self.__get_failed_assertion(element)
        if failed_assertion is not None:
            assertion_message = self.__get_assertion_message(failed_assertion)
            if assertion_message:
                return assertion_message
            else:
                return element.get('rm')
        r_code = element.get('rc')
        if r_code and r_code.startswith("2"):
            if element.get('s') == "false":
                children = [elem for elem in element.iterchildren() if elem.tag == "httpSample"]
                for child in children:
                    child_message = self.get_failure_message(child)
                    if child_message:
                        return child_message
        else:
            return element.get('rm')

    def __get_assertion_message(self, assertion_element):
        """
        Returns assertion failureMessage if "failureMessage" element exists
        """
        failure_message_elem = assertion_element.find("failureMessage")
        if failure_message_elem is not None:
            msg = failure_message_elem.text
            if msg.startswith("The operation lasted too long"):
                msg = "The operation lasted too long"

            return msg

    def __get_failed_assertion(self, element):
        """
        Returns first failed assertion, or None

        :rtype lxml.etree.Element
        """
        assertions = [elem for elem in element.iterchildren() if elem.tag == "assertionResult"]
        for assertion in assertions:
            if self.__assertion_is_failed(assertion):
                return assertion

    def __assertion_is_failed(self, assertion_element):
        """`
        returns True if assertion failed
        """
        failed = assertion_element.find("failure")
        error = assertion_element.find("error")
        if failed.text == "true" or error.text == "true":
            return True
        return False

    def __get_child(self, elem, tag):
        for child in elem:
            if child.tag == tag:
                return child.text
