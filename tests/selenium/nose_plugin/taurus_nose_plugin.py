from time import time

from nose.plugins import Plugin
import nose
import traceback
import sys

class TaurusNosePlugin(Plugin):
    """
    Output test results in a format suitable for Taurus report.
    """

    name = 'taurus_nose_plugin'
    enabled = True

    def report_error(self, err):
        exc_type, value, tb = err
        return "--TRACE:" + "".join(traceback.format_exception(exc_type, value, tb))

    def addError(self, test, err, capt=None):
        """
        when a test raises an uncaught exception
        :param test:
        :param err:
        :return:
        """
        self.stream.write("--RESULT: ERROR\n")
        self.stream.flush()
        self._trace = self.report_error(err)

    def addFailure(self, test, err, capt=None, tbinfo=None):
        """
        when a test fails
        :param test:
        :param err:

        :return:
        """
        self.stream.write("--RESULT: FAILED\n")
        self.stream.flush()
        self._trace = self.report_error(err)

    def addSkip(self, test):
        """
        when a test is skipped
        :param test:
        :return:
        """
        self.stream.write("--RESULT: SKIPPED\n")
        self.stream.flush()

    def addSuccess(self, test, capt=None):
        """
        when a test passes
        :param test:
        :return:
        """
        self.stream.write("--RESULT: OK\n")
        self.stream.flush()

    def begin(self):
        """
        Before any test runs
        open descriptor here
        :return:
        """
        self._module_name = ""
        self.stream = open("report.txt", "wt")

    def finalize(self, result):
        """
        After all tests
        :param result:
        :return:
        """
        self.stream.close()

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
        self.stream.write("--TIMESTAMP: %d\n" % (1000*self._time))
        self.stream.write("--MODULE: %s\n" % self._module_name)
        self.stream.write("--RUN: %s\n" % test)
        self.stream.flush()

    def stopTest(self, test):
        """
        after the test has been run
        :param test:
        :return:
        """
        if self._trace:
            self.stream.write(self._trace)
            self.stream.flush()
        self.stream.write("--TIME: %d\n" % (1000*(time() - self._time)))
        self.stream.flush()

    def setOutputStream(self, stream):

        class dummy:
            def write(self, *arg):
                pass

            def writeln(self, *arg):
                pass

            def flush(self, *arg):
                pass

        d = dummy()
        return d

if __name__ == "__main__":
    nose.main(addplugins=[TaurusNosePlugin()], argv=sys.argv)