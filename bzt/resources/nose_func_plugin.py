import csv
import sys
import traceback
from optparse import OptionParser
from time import time

import nose
from nose.plugins import Plugin


CSV_HEADER = ["id", "description", "start_time", "duration", "status", "error_msg", "error_trace"]


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
        self.csv_writer = None
        self.test_dict = None
        self.out_stream = None
        self._time = None

    def __enter__(self):
        self.out_stream = open(self.output_file, "wt", buffering=1)
        self.csv_writer = csv.DictWriter(self.out_stream, delimiter=',', fieldnames=CSV_HEADER)
        self.csv_writer.writeheader()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.out_stream.close()

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
        del test
        self.test_dict["status"] = "SKIPPED"

    def addSuccess(self, test):
        """
        when a test passes
        :param test:
        :return:
        """
        del test
        self.test_dict["status"] = "PASSED"
        self.success_count += 1

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
        test_name = test.id()

        self.last_err = None
        self._time = time()
        self.test_dict = {}.fromkeys(CSV_HEADER, None)
        self.test_dict["id"] = test_name
        self.test_dict["description"] = test.shortDescription()
        self.test_dict["start_time"] = "%0.6f" % time()

    def stopTest(self, test):
        """
        after the test has been run
        :param test:
        :return:
        """
        del test
        self.test_count += 1
        self.test_dict["duration"] = "%0.6f" % (time() - float(self.test_dict["start_time"]))
        self.csv_writer.writerow(self.test_dict)

        report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        sys.stdout.write(report_pattern % (
            self.test_dict["id"], self.test_count, self.success_count, self.test_count - self.success_count))

        self.out_stream.flush()


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
