import json
import sys
import time
import traceback
from collections import OrderedDict
from optparse import OptionParser

import nose
from nose.plugins import Plugin

if sys.version_info[0] == 2:
    irange = xrange
else:
    irange = range

REPORT_ITEM_KEYS = [
    "label",  # test label (test method name)
    "file",  # file location of test
    "full_name",  # full test name (package.module.class.method)
    "status",  # test status (PASSED/FAILED/BROKEN/SKIPPED)
    "description",  # test description (from a docstring)
    "start_time",  # test start time
    "duration",  # test duration
    "error_msg",  # short error message
    "error_trace"  # traceback of a failure
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
        test_method = test_fqn.split('.')[-1]

        self.test_dict["file"] = test_file
        self.test_dict["label"] = test_method
        self.test_dict["full_name"] = test_fqn
        self.test_dict["description"] = test.shortDescription()
        self.test_dict["start_time"] = time.time()

    def addError(self, test, error):  # pylint: disable=invalid-name
        """
        when a test raises an uncaught exception
        :param test:
        :param error:
        :return:
        """
        exc_type, exc, trace = error
        self.test_dict["status"] = "BROKEN"
        self.test_dict["error_msg"] = ''.join(traceback.format_exception_only(exc_type, exc)).rstrip()
        self.test_dict["error_trace"] = ''.join(traceback.format_exception(exc_type, exc, trace)).rstrip()

    def addFailure(self, test, error):  # pylint: disable=invalid-name
        """
        when a test fails
        :param test:
        :param error:

        :return:
        """
        exc_type, exc, trace = error
        self.test_dict["status"] = "FAILED"
        self.test_dict["error_msg"] = ''.join(traceback.format_exception_only(exc_type, exc)).rstrip()
        self.test_dict["error_trace"] = ''.join(traceback.format_exception(exc_type, exc, trace)).rstrip()

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

    def stopTest(self, test):  # pylint: disable=invalid-name
        """
        after the test has been run
        :param test:
        :return:
        """
        self.test_count += 1
        self.test_dict["duration"] = time.time() - self.test_dict["start_time"]
        self.out_stream.write("%s\n" % json.dumps(self.test_dict))
        self.out_stream.flush()

        report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        sys.stdout.write(report_pattern % (
            self.test_dict["label"], self.test_count, self.success_count, self.test_count - self.success_count))


def run_nose(report_file, files, iterations, hold):
    argv = [__file__, '-v']
    argv.extend(files)
    argv.extend(['--with-bzt_plugin', '--nocapture'])

    if iterations == 0:
        if hold > 0:
            iterations = sys.maxsize
        else:
            iterations = 1

    start_time = int(time.time())
    with BZTPlugin(report_file) as plugin:
        for _ in irange(0, iterations):
            nose.run(addplugins=[plugin], argv=argv)
            if 0 < hold < int(time.time()) - start_time:
                break


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('-r', '--report-file', action='store', default='nose_report.ldjson')
    parser.add_option('-i', '--iterations', action='store', default=0)
    parser.add_option('-d', '--duration', action='store', default=0)
    parser.add_option('-w', '--with-nose_plugin', action='store', default=0)
    opts, args = parser.parse_args()

    run_nose(opts.report_file, args, int(opts.iterations), float(opts.duration))
