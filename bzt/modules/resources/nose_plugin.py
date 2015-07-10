from time import time

from nose.plugins import Plugin
from nose import run
import traceback
import sys
import csv


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
        self.csv_writer = None
        self.jtl_dict = None
        self.jtl_header = ["timeStamp", "elapsed", "label", "responseCode", "responseMessage", "threadName", "success",
                         "grpThreads", "allThreads", "Latency", "Connect"]

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
        # self.stream.write("--RESULT: ERROR\n")
        # self.stream.flush()
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
        # self.stream.write("--RESULT: FAILED\n")
        # self.stream.flush()
        self._trace = self.report_error(err)

    def addSkip(self, test):
        """
        when a test is skipped
        :param test:
        :return:
        """
        # self.stream.write("--RESULT: SKIPPED\n")
        # self.stream.flush()
        self.jtl_dict["responseCode"] = 300

    def addSuccess(self, test, capt=None):
        """
        when a test passes
        :param test:
        :return:
        """
        # self.stream.write("--RESULT: OK\n")
        # self.stream.flush()
        self.jtl_dict["responseCode"] = 200
        self.jtl_dict["success"] = "true"
        self.jtl_dict["responseMessage"] = "OK"


    def begin(self):
        """
        Before any test runs
        open descriptor here
        :return:
        """
        self.stream = open(self.output_file, "wt")
        self.csv_writer = csv.DictWriter(self.stream, delimiter=',', fieldnames=self.jtl_header)
        self.csv_writer.writeheader()
        self._module_name = ""


    def finalize(self, result):
        """
        After all tests
        :param result:
        :return:
        """
        self.stream.close()
        if not self.test_count:
            sys.stderr.write("Nothing to test.")
            sys.exit(1)

    def startTest(self, test):
        """
        before test run
        :param test:
        :return:
        """
        if self._module_name != str(test.__module__):
            self._module_name = str(test.__module__)
        self._trace = ""
        self._time = time()
        self.jtl_dict = {}.fromkeys(self.jtl_header, 0)
        self.jtl_dict["timeStamp"] = int(1000 * self._time)
        self.jtl_dict["label"] = self._module_name
        self.jtl_dict["threadName"] = test
        self.jtl_dict["grpThreads"] = 1
        self.jtl_dict["allThreads"] = 1
        self.jtl_dict["Latency"] = 0
        self.jtl_dict["Connect"] = 0
        self.jtl_dict["success"] = "false"

        # self.stream.write("--TIMESTAMP: %d\n" % (1000 * self._time))
        # self.stream.write("--MODULE: %s\n" % self._module_name)
        # self.stream.write("--RUN: %s\n" % test)
        # self.stream.flush()
        self.test_count += 1
        # sys.stdout.write("test")
        # sys.stderr.write("test")

    def stopTest(self, test):
        """
        after the test has been run
        :param test:
        :return:
        """

        if self._trace:
            self.jtl_dict["responseMessage"] = self._trace
            # self.stream.write(self._trace)
            # self.stream.flush()

        self.jtl_dict["elapsed"] = int(1000 * (time() - self._time))
        self.csv_writer.writerow(self.jtl_dict)
        self.stream.flush()

        #self.stream.write("--TIME: %d\n" % (1000 * (time() - self._time)))
        # self.stream.flush()

    def setOutputStream(self, stream):

        class dummy:
            def write(self, *arg):
                pass

            def writeln(self, *arg):
                pass

            def flush(self, *arg):
                pass

        return dummy()


if __name__ == "__main__":
    output_file = sys.argv[1]
    test_path = sys.argv[2:]
    argv = [__file__, '-v']
    argv.extend(test_path)
    run(addplugins=[TaurusNosePlugin(output_file)], argv=argv + ['--with-nose_plugin'])
