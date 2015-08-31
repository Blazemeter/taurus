from time import time

from nose.plugins import Plugin
from nose import run
import traceback
import sys
import csv
import re

JTL_HEADER = ["timeStamp", "elapsed", "label", "responseCode", "responseMessage", "threadName", "success",
                           "grpThreads", "allThreads", "Latency", "Connect"]

SEARCH_PATTERNS = {"file": re.compile(r'\((.*?)\.'), "class": re.compile(r'\.(.*?)\)'),
                                "method": re.compile(r'(.*?)\ ')}

class TaurusNosePlugin(Plugin):
    """
    Output test results in a format suitable for Taurus report.
    """

    name = 'nose_plugin'
    enabled = True

    def __init__(self, output_file):
        super(TaurusNosePlugin, self).__init__()
        self._trace = None
        self._module_name = None
        self.output_file = output_file
        self.test_count = 0
        self.success = 0
        self.csv_writer = None
        self.jtl_dict = None

    def report_error(self, err):
        exc_type, value, tb = err
        trace = "--TRACE:" + "".join(traceback.format_tb(tb))
        errmsg = "--MESSAGE:" + "".join(traceback.format_exception_only(exc_type, value))
        return trace + "\n" + errmsg

    def addError(self, test, err, capt=None):
        """
        when a test raises an uncaught exception
        :param test:
        :param err:
        :return:
        """
        self.jtl_dict["responseCode"] = 500
        self._trace = self.report_error(err)

    def addFailure(self, test, err, capt=None, tbinfo=None):
        """
        when a test fails
        :param test:
        :param err:

        :return:
        """
        self.jtl_dict["responseCode"] = 404
        self._trace = self.report_error(err)

    def addSkip(self, test):
        """
        when a test is skipped
        :param test:
        :return:
        """
        self.jtl_dict["responseCode"] = 300

    def addSuccess(self, test, capt=None):
        """
        when a test passes
        :param test:
        :return:
        """
        self.jtl_dict["responseCode"] = 200
        self.jtl_dict["success"] = "true"
        self.jtl_dict["responseMessage"] = "OK"
        self.success += 1

    def begin(self):
        """
        Before any test runs
        open descriptor here
        :return:
        """
        self.out_stream = open(self.output_file, "wt")
        self.csv_writer = csv.DictWriter(self.out_stream, delimiter=',', fieldnames=JTL_HEADER)
        self.csv_writer.writeheader()
        self._module_name = ""

    def finalize(self, result):
        """
        After all tests
        :param result:
        :return:
        """
        self.out_stream.close()
        if not self.test_count:
            raise RuntimeError("Nothing to test.")

    def startTest(self, test):
        """
        before test run
        :param test:
        :return:
        """
        full_test_name = str(test)

        file_name = SEARCH_PATTERNS["file"].findall(full_test_name)
        file_name = file_name[0] if file_name else ""

        class_name = SEARCH_PATTERNS["file"].findall(full_test_name)
        class_name = class_name[0] if class_name else ""

        method_name = SEARCH_PATTERNS["method"].findall(full_test_name)
        method_name = method_name[0] if method_name else ""

        if self._module_name != file_name + "." + class_name:
            self._module_name = file_name + "." + class_name
        self._trace = ""
        self._time = time()
        self.jtl_dict = {}.fromkeys(JTL_HEADER, 0)
        self.jtl_dict["timeStamp"] = int(1000 * self._time)
        self.jtl_dict["label"] = method_name
        self.jtl_dict["threadName"] = self._module_name
        self.jtl_dict["grpThreads"] = 1
        self.jtl_dict["allThreads"] = 1
        self.jtl_dict["success"] = "false"

        report_pattern = "%s.%s,Total:%d Pass:%d Failed:%d\n"
        sys.stdout.write(report_pattern % (
            class_name, method_name, self.test_count + 1, self.success, self.test_count - self.success))

    def stopTest(self, test):
        """
        after the test has been run
        :param test:
        :return:
        """
        self.test_count += 1
        if self._trace:
            self.jtl_dict["responseMessage"] = self._trace

        self.jtl_dict["elapsed"] = int(1000 * (time() - self._time))
        self.csv_writer.writerow(self.jtl_dict)
        self.out_stream.flush()


if __name__ == "__main__":
    _output_file = sys.argv[1]
    test_path = sys.argv[2:]
    argv = [__file__, '-v']
    argv.extend(test_path)
    run(addplugins=[TaurusNosePlugin(_output_file)], argv=argv + ['--with-nose_plugin'] + ['--nocapture'])
