import json
import sys
import time
import traceback
from collections import OrderedDict
from optparse import OptionParser

import nose
from nose.plugins import Plugin


REPORT_ITEM_KEYS = ["label", "status", "description", "start_time", "duration", "error_msg", "error_trace"]


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
        self._module_name = ""

    def finalize(self, result):
        """
        After all tests
        :param result:
        :return:
        """
        del result
        if not self.test_count:
            raise RuntimeError("Nothing to test.")

    def startTest(self, test):
        """
        before test run
        :param test:
        :return:
        """
        self.last_err = None
        self.test_dict = OrderedDict((key, None) for key in REPORT_ITEM_KEYS)
        self.test_dict["label"] = ".".join(test.id().split('.')[-2:])
        self.test_dict["description"] = test.shortDescription()
        self.test_dict["start_time"] = time.time()

    def addError(self, test, error):
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

    def addFailure(self, test, error):
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

    def addSkip(self, test):
        """
        when a test is skipped
        :param test:
        :return:
        """
        self.test_dict["status"] = "SKIPPED"

    def addSuccess(self, test):
        """
        when a test passes
        :param test:
        :return:
        """
        self.test_dict["status"] = "PASSED"
        self.success_count += 1

    def stopTest(self, test):
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



def run_nose(report_file, files):
    argv = [__file__, '-q']
    argv.extend(files)
    argv.extend(['--with-bzt_plugin', '--nocapture'])
    with BZTPlugin(report_file) as plugin:
        nose.run(addplugins=[plugin], argv=argv)


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('--report-file', action='store')
    opts, args = parser.parse_args()
    run_nose(opts.report_file, args)
