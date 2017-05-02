import copy
import json
import sys
import time
import traceback
from optparse import OptionParser

import nose
from nose.plugins import Plugin


def get_apiritif():
    try:
        import apiritif
    except ImportError:
        apiritif = None
    return apiritif


class Sample(object):
    def __init__(self, test_suite=None, test_case=None, status=None, start_time=None, duration=None,
                 error_msg=None, error_trace=None):
        self.test_suite = test_suite  # test label (test method name)
        self.test_case = test_case  # test suite name (class name)
        self.status = status  # test status (PASSED/FAILED/BROKEN/SKIPPED)
        self.start_time = start_time  # test start time
        self.duration = duration  # test duration
        self.error_msg = error_msg  # short error message
        self.error_trace = error_trace  # traceback of a failure
        self.extras = {}  # extra info: ('file' - location, 'full_name' - full qualified name, 'decsription' - docstr)
        self.subsamples = []  # subsamples list

    def add_subsample(self, sample):
        self.subsamples.append(sample)

    def to_dict(self):
        # type: () -> dict
        return {
            "test_suite": self.test_suite,
            "test_case": self.test_case,
            "status": self.status,
            "start_time": self.start_time,
            "duration": self.duration,
            "error_msg": self.error_msg,
            "error_trace": self.error_trace,
            "extras": self.extras,
            "subsamples": [sample.to_dict() for sample in self.subsamples],
        }

    def __repr__(self):
        return "Sample(%r)" % self.to_dict()


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
        self.current_sample = None
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
        test_file, _, _ = test.address()  # file path, module name, class.method
        test_fqn = test.id()  # [package].module.class.method
        class_name, method_name = test_fqn.split('.')[-2:]

        self.current_sample = Sample(test_case=method_name, test_suite=class_name, start_time=time.time())
        self.current_sample.extras.update({
            "file": test_file,
            "full_name": test_fqn,
            "description": test.shortDescription()
        })

    def addError(self, test, error):  # pylint: disable=invalid-name
        """
        when a test raises an uncaught exception
        :param test:
        :param error:
        :return:
        """
        # test_dict will be None if startTest wasn't called (i.e. exception in setUp/setUpClass)
        if self.current_sample is not None:
            self.current_sample.status = "BROKEN"
            self.current_sample.error_msg = str(error[1]).split('\n')[0]
            self.current_sample.error_trace = self._get_trace(error)

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
        self.current_sample.status = "FAILED"
        self.current_sample.error_msg = str(error[1]).split('\n')[0]
        self.current_sample.error_trace = self._get_trace(error)

    def addSkip(self, test):  # pylint: disable=invalid-name
        """
        when a test is skipped
        :param test:
        :return:
        """
        self.current_sample.status = "SKIPPED"

    def addSuccess(self, test):  # pylint: disable=invalid-name
        """
        when a test passes
        :param test:
        :return:
        """
        self.current_sample.status = "PASSED"
        self.success_count += 1

    def process_apiritif_samples(self, sample):
        samples_processed = 0
        apiritif = get_apiritif()
        test_case = sample.test_case

        recording = apiritif.recorder.get_recording(test_case)
        if not recording:
            return samples_processed

        samples = ApiritifExtractor.parse_recording(recording, sample)
        for sample in samples:
            samples_processed += 1
            self.test_count += 1
            self.write_sample(sample)
            self.write_stdout_report(sample.test_case)

        return samples_processed

    def process_sample(self, sample):
        self.test_count += 1
        self.write_sample(sample)
        self.write_stdout_report(sample.test_case)

    def write_sample(self, sample):
        self.out_stream.write("%s\n" % json.dumps(sample.to_dict()))
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
        self.current_sample.duration = time.time() - self.current_sample.start_time

        if get_apiritif() is not None:
            samples_processed = self.process_apiritif_samples(self.current_sample)
            if samples_processed == 0:
                self.process_sample(self.current_sample)
        else:
            self.process_sample(self.current_sample)

        self.current_sample = None


class ApiritifExtractor(object):
    @staticmethod
    def parse_recording(recording, test_case_sample):
        """

        :type recording: list[apiritif.Event]
        :type test_case_sample: Sample
        :rtype: list[Sample]
        """
        apiritif = get_apiritif()
        test_case_name = test_case_sample.test_case
        active_transactions = [test_case_sample]
        response_map = {}  # response -> sample
        transactions_present = False
        for item in recording:
            if isinstance(item, apiritif.Request):
                sample = Sample(
                    test_suite=test_case_name,
                    test_case=item.address,
                    status="PASSED",
                    start_time=item.timestamp,
                    duration=item.response.elapsed.total_seconds(),
                )
                extras = ApiritifExtractor._extract_extras(item)
                if extras:
                    sample.extras.update(extras)
                response_map[item.response] = sample
                active_transactions[-1].add_subsample(sample)
            elif isinstance(item, apiritif.TransactionStarted):
                transactions_present = True
                tran_sample = Sample(test_case=item.transaction_name, test_suite=test_case_name)
                active_transactions.append(tran_sample)
            elif isinstance(item, apiritif.TransactionEnded):
                tran_sample = active_transactions.pop()
                assert tran_sample.test_case == item.transaction_name
                tran_sample.start_time = item.transaction.start_time()
                tran_sample.duration = item.transaction.duration()
                if item.transaction.success is None:
                    tran_sample.status = "PASSED"
                    for sample in tran_sample.subsamples:
                        if sample.status in ("FAILED", "BROKEN"):
                            tran_sample.status = sample.status
                            tran_sample.error_msg = sample.error_msg
                            tran_sample.error_trace = sample.error_trace
                elif item.transaction.success:
                    tran_sample.status = "PASSED"
                else:
                    tran_sample.status = "FAILED"
                    tran_sample.error_msg = item.transaction.error_message

                extras = copy.deepcopy(item.transaction.extras())
                extras.update({
                    'responseCode': item.transaction.response_code(),
                    'responseMessage': "",
                    'responseTime': item.transaction.duration(),
                    'connectTime': 0,
                    'latency': 0,
                    'responseSize': len(item.transaction.response() or ""),
                    'requestSize': len(item.transaction.request() or ""),
                    'requestMethod': "",
                    'requestURI': "",
                    'assertions': [],  # will be filled later
                    'responseBody': item.transaction.response() or "",
                    'requestBody': item.transaction.request() or "",
                    'requestCookies': {},
                    'requestHeaders': {},
                    'responseHeaders': {},
                    "requestCookiesRaw": "",
                    "requestCookiesSize": 0,
                    "requestHeadersSize": 0,
                    "responseHeadersSize": 0,
                })
                extras["responseBodySize"] = len(extras["responseBody"])
                extras["requestBodySize"] = len(extras["requestBody"])
                tran_sample.extras = extras

                active_transactions[-1].add_subsample(tran_sample)
            elif isinstance(item, apiritif.Assertion):
                sample = response_map.get(item.response, None)
                if sample is None:
                    raise ValueError("Found assertion for unknown response")
                if "assertions" not in sample.extras:
                    sample.extras["assertions"] = []
                sample.extras["assertions"].append({
                    "name": item.name,
                    "isFailed": False,
                    "failureMessage": "",
                })
            elif isinstance(item, apiritif.AssertionFailure):
                sample = response_map.get(item.response, None)
                if sample is None:
                    raise ValueError("Found assertion failure for unknown response")
                for ass in sample.extras.get("assertions", []):
                    if ass["name"] == item.name:
                        ass["isFailed"] = True
                        ass["failureMessage"] = item.failure_message
                        sample.status = "FAILED"
                        sample.error_msg = item.failure_message
            else:
                raise ValueError("Unknown kind of event in apiritif recording: %s" % item)

        if len(active_transactions) != 1:
            # TODO: shouldn't we auto-balance them?
            raise ValueError("Can't parse apiritif recordings: unbalanced transactions")

        toplevel_sample = active_transactions.pop()

        # do not capture toplevel sample if transactions were used
        if transactions_present:
            return toplevel_sample.subsamples
        else:
            return [toplevel_sample]

    @staticmethod
    def _headers_from_dict(headers):
        return "\n".join(key + ": " + value for key, value in headers.items())

    @staticmethod
    def _cookies_from_dict(cookies):
        return "; ".join(key + "=" + value for key, value in cookies.items())

    @staticmethod
    def _extract_extras(request_event):
        response = request_event.response
        baked_request = request_event.request

        record = {
            'responseCode': response.status_code,
            'responseMessage': response.reason,
            'responseTime': response.elapsed.total_seconds(),
            'connectTime': 0,
            'latency': 0,
            'responseSize': len(response.content),
            'requestSize': 0,
            'requestMethod': baked_request.method,
            'requestURI': baked_request.url,
            'assertions': [],  # will be filled later
            'responseBody': response.text,
            'requestBody': baked_request.body or "",
            'requestCookies': dict(request_event.session.cookies),
            'requestHeaders': dict(response._request.headers),
            'responseHeaders': dict(response.headers),
        }

        record["requestCookiesRaw"] = ApiritifExtractor._cookies_from_dict(record["requestCookies"])
        record["responseBodySize"] = len(record["responseBody"])
        record["requestBodySize"] = len(record["requestBody"])
        record["requestCookiesSize"] = len(record["requestCookiesRaw"])
        record["requestHeadersSize"] = len(ApiritifExtractor._headers_from_dict(record["requestHeaders"]))
        record["responseHeadersSize"] = len(ApiritifExtractor._headers_from_dict(record["responseHeaders"]))

        return record


def run_nose(report_file, files, iteration_limit, hold):
    argv = [__file__, '-v']
    argv.extend(files)
    argv.extend(['--with-bzt_plugin', '--nocapture', '--exe', '--nologcapture'])

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
