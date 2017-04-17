import json
import sys
import time
import traceback
from collections import OrderedDict
from optparse import OptionParser

import nose
from nose.plugins import Plugin


REPORT_ITEM_KEYS = [
    "test_case",  # test label (test method name)
    "test_suite",  # test suite name (class name)
    "status",  # test status (PASSED/FAILED/BROKEN/SKIPPED)
    "start_time",  # test start time
    "duration",  # test duration
    "error_msg",  # short error message
    "error_trace",  # traceback of a failure
    "extras",  # extra info: 'file' - test location, 'full_name' - fully qualified name, 'decsription' - docstring
]


class BZTPlugin(Plugin):
    """
    Saves test results in a format suitable for Taurus.
    """

    name = 'bzt_plugin'
    enabled = True

    def __init__(self, output_file):
        super(BZTPlugin, self).__init__()
        self.output_file = output_file
        self.test_count = 0
        self.success_count = 0
        self.test_dict = None
        self.out_stream = None

    def __enter__(self):
        self.out_stream = open(self.output_file, "wt", buffering=1)
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.out_stream.close()

    def begin(self):
        """
        Before any test runs
        open descriptor here
        :return:
        """
        pass

    def finalize(self, result):
        """
        After all tests
        :param result:
        :return:
        """
        del result
        if not self.test_count:
            raise RuntimeError("Nothing to test.")

    def startTest(self, test):  # pylint: disable=invalid-name
        """
        before test run
        :param test:
        :return:
        """
        self.test_dict = OrderedDict((key, None) for key in REPORT_ITEM_KEYS)

        test_file, _, _ = test.address()  # file path, module name, class.method
        test_fqn = test.id()  # [package].module.class.method
        class_name, method_name = test_fqn.split('.')[-2:]

        self.test_dict["test_case"] = method_name
        self.test_dict["test_suite"] = class_name
        self.test_dict["start_time"] = time.time()
        self.test_dict["extras"] = {
            "file": test_file,
            "full_name": test_fqn,
            "description": test.shortDescription()
        }

    def addError(self, test, error):  # pylint: disable=invalid-name
        """
        when a test raises an uncaught exception
        :param test:
        :param error:
        :return:
        """
        # test_dict will be None if startTest wasn't called (i.e. exception in setUp/setUpClass)
        if self.test_dict is not None:
            self.test_dict["status"] = "BROKEN"
            self.test_dict["error_msg"] = str(error[1]).split('\n')[0]
            self.test_dict["error_trace"] = self._get_trace(error)

    @staticmethod
    def _get_trace(error):
        if sys.version > '3':
            lines = traceback.format_exception(*error, chain=not isinstance(error[1], str))
        else:
            lines = traceback.format_exception(*error)
        return ''.join(lines).rstrip()

    def addFailure(self, test, error):  # pylint: disable=invalid-name
        """
        when a test fails
        :param test:
        :param error:

        :return:
        """
        self.test_dict["status"] = "FAILED"
        self.test_dict["error_msg"] = str(error[1]).split('\n')[0]
        self.test_dict["error_trace"] = self._get_trace(error)

    def addSkip(self, test):  # pylint: disable=invalid-name
        """
        when a test is skipped
        :param test:
        :return:
        """
        self.test_dict["status"] = "SKIPPED"

    def addSuccess(self, test):  # pylint: disable=invalid-name
        """
        when a test passes
        :param test:
        :return:
        """
        self.test_dict["status"] = "PASSED"
        self.success_count += 1

    @staticmethod
    def _headers_from_dict(headers):
        return "\n".join(key + ": " + value for key, value in headers.items())

    @staticmethod
    def _cookies_from_dict(cookies):
        return "; ".join(key + "=" + value for key, value in cookies.items())

    def _extract_apiritif_extras(self, test_case):
        import apiritif

        recording = apiritif.recorder.get_recording(test_case)
        if not recording:
            return

        recorded_requests = [event for event in recording if isinstance(event, apiritif.Request)]
        if not recorded_requests:
            return None

        request_event = recorded_requests[0]
        assertion_events = [
            event for event in recording
            if isinstance(event, (apiritif.Assertion, apiritif.AssertionFailure))
            and event.response == request_event.response
        ]
        assertions = {}
        for event in assertion_events:
            if event.name not in assertions:
                assertions[event.name] = {"name": event.name, "isFailed": False, "failureMessage": ""}

            if isinstance(event, apiritif.AssertionFailure):
                assertions[event.name]["isFailed"] = True
                assertions[event.name]["failureMessage"] = event.failure_message

        py_response = request_event.response.py_response
        baked_request = request_event.request

        record = {
            'responseCode': py_response.status_code,
            'responseMessage': py_response.reason,
            'responseTime': py_response.elapsed.total_seconds(),
            'connectTime': 0,
            'latency': 0,
            'responseSize': len(py_response.content),
            'requestSize': 0,
            'requestMethod': baked_request.method,
            'requestURI': baked_request.url,
            'assertions': [assertion for assertion in assertions.values()],
            'responseBody': py_response.text,
            'requestBody': baked_request.body or "",
            'requestCookies': dict(request_event.session.cookies),
            'requestHeaders': dict(py_response.request.headers),
            'responseHeaders': dict(py_response.headers),
        }

        record["requestCookiesRaw"] = self._cookies_from_dict(record["requestCookies"])
        record["responseBodySize"] = len(record["responseBody"])
        record["requestBodySize"] = len(record["requestBody"])
        record["requestCookiesSize"] = len(record["requestCookiesRaw"])
        record["requestHeadersSize"] = len(self._headers_from_dict(record["requestHeaders"]))
        record["responseHeadersSize"] = len(self._headers_from_dict(record["responseHeaders"]))

        return record

    def process_apiritif_samples(self, sample_dict):
        apiritif_extras = self._extract_apiritif_extras(sample_dict["test_case"])
        if apiritif_extras:
            sample_dict["extras"].update(apiritif_extras)

        self.test_count += 1
        self.write_sample(sample_dict)
        self.write_stdout_report(sample_dict["test_case"])

    def process_sample(self, sample_dict):
        self.test_count += 1
        self.write_sample(sample_dict)
        self.write_stdout_report(sample_dict["test_case"])

    def write_sample(self, sample):
        self.out_stream.write("%s\n" % json.dumps(sample))
        self.out_stream.flush()

    def write_stdout_report(self, label):
        report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        failed = self.test_count - self.success_count
        sys.stdout.write(report_pattern % (label, self.test_count, self.success_count, failed))
        sys.stdout.flush()

    def stopTest(self, test):  # pylint: disable=invalid-name
        """
        after the test has been run
        :param test:
        :return:
        """
        self.test_dict["duration"] = time.time() - self.test_dict["start_time"]

        try:
            import apiritif
        except ImportError:
            apiritif = None

        if apiritif is not None:
            self.process_apiritif_samples(self.test_dict)
        else:
            self.process_sample(self.test_dict)

        self.test_dict = None


def run_nose(report_file, files, iteration_limit, hold):
    argv = [__file__, '-v']
    argv.extend(files)
    argv.extend(['--with-bzt_plugin', '--nocapture', '--exe'])

    if iteration_limit == 0:
        if hold > 0:
            iteration_limit = sys.maxsize
        else:
            iteration_limit = 1

    start_time = int(time.time())
    with BZTPlugin(report_file) as plugin:
        iteration = 0
        while True:
            nose.run(addplugins=[plugin], argv=argv)
            iteration += 1
            if 0 < hold < int(time.time()) - start_time:
                break
            if iteration >= iteration_limit:
                break


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('-r', '--report-file', action='store', default='nose_report.ldjson')
    parser.add_option('-i', '--iterations', action='store', default=0)
    parser.add_option('-d', '--duration', action='store', default=0)
    parser.add_option('-w', '--with-nose_plugin', action='store', default=0)
    opts, args = parser.parse_args()

    run_nose(opts.report_file, args, int(opts.iterations), float(opts.duration))
