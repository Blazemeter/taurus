import csv
import re
import sys
import traceback
from optparse import OptionParser
from time import time

import nose
from nose.plugins import Plugin

try:
    from lxml import etree
except ImportError:
    try:
        # noinspection PyPackageRequirements
        import cElementTree as etree
    except ImportError:
        # noinspection PyPackageRequirements,PyPep8Naming
        import elementtree.ElementTree as etree

if sys.version_info[0] == 2:
    irange = xrange
else:
    irange = range

JTL_ERR_ATRS = ["t", "lt", "ct", "ts", "s", "lb", "rc", "rm", "tn", "dt", "de", "by", "ng", "na"]

JTL_HEADER = ["timeStamp", "elapsed", "label", "responseCode", "responseMessage", "threadName", "success",
              "grpThreads", "allThreads", "Latency", "Connect"]

SEARCH_PATTERNS = {"file": re.compile(r'\((.*?)\.'), "class": re.compile(r'\.(.*?)\)'),
                   "method": re.compile(r'(.*?) ')}


class TaurusNosePlugin(Plugin):
    """
    Output test results in a format suitable for Taurus report.
    """

    name = 'nose_plugin'
    enabled = True

    def __init__(self, output_file, err_file):
        super(TaurusNosePlugin, self).__init__()
        self._module_name = None
        self._method_name = None
        self.output_file = output_file
        self.err_file = err_file
        self.test_count = 0
        self.success = 0
        self.csv_writer = None
        self.jtl_dict = None
        self.error_writer = None
        self.last_err = None
        self.out_stream = None
        self.err_stream = None
        self._time = None

    def __enter__(self):
        self.out_stream = open(self.output_file, "wt")
        self.csv_writer = csv.DictWriter(self.out_stream, delimiter=',', fieldnames=JTL_HEADER)
        self.csv_writer.writeheader()

        self.err_stream = open(self.err_file, "wb")
        self.error_writer = JTLErrorWriter(self.err_stream)
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.error_writer.close()
        self.out_stream.close()
        self.err_stream.close()

    def addError(self, test, err, capt=None):  # pylint: disable=invalid-name
        """
        when a test raises an uncaught exception
        :param test:
        :param err:
        :return:
        """
        del test, capt
        self.jtl_dict["responseCode"] = "500"
        self.last_err = err

    def addFailure(self, test, err, capt=None, tbinfo=None):  # pylint: disable=invalid-name
        """
        when a test fails
        :param test:
        :param err:

        :return:
        """
        del test, capt, tbinfo
        self.jtl_dict["responseCode"] = "404"
        self.last_err = err

    def addSkip(self, test):  # pylint: disable=invalid-name
        """
        when a test is skipped
        :param test:
        :return:
        """
        del test
        self.jtl_dict["responseCode"] = "300"

    def addSuccess(self, test, capt=None):  # pylint: disable=invalid-name
        """
        when a test passes
        :param test:
        :return:
        """
        del test, capt
        self.jtl_dict["responseCode"] = "200"
        self.jtl_dict["success"] = "true"
        self.jtl_dict["responseMessage"] = "OK"
        self.success += 1

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

    def startTest(self, test):  # pylint: disable=invalid-name
        """
        before test run
        :param test:
        :return:
        """
        full_test_name = str(test)

        file_name = SEARCH_PATTERNS["file"].findall(full_test_name)
        file_name = file_name[0] if file_name else ""

        class_name = SEARCH_PATTERNS["class"].findall(full_test_name)
        class_name = class_name[0] if class_name else ""

        method_name = SEARCH_PATTERNS["method"].findall(full_test_name)
        method_name = method_name[0] if method_name else ""

        if self._module_name != file_name + "." + class_name:
            self._module_name = file_name + "." + class_name

        self._method_name = method_name
        self.last_err = None
        self._time = time()
        self.jtl_dict = {}.fromkeys(JTL_HEADER, 0)
        self.jtl_dict["timeStamp"] = int(1000 * self._time)
        self.jtl_dict["label"] = self._method_name
        self.jtl_dict["threadName"] = self._module_name
        self.jtl_dict["grpThreads"] = 1
        self.jtl_dict["allThreads"] = 1
        self.jtl_dict["success"] = "false"

    def stopTest(self, test):  # pylint: disable=invalid-name
        """
        after the test has been run
        :param test:
        :return:
        """
        del test
        self.test_count += 1
        self.jtl_dict["elapsed"] = str(int(1000 * (time() - self._time)))

        if self.last_err is not None:
            exc_type_name = self.last_err[0].__name__
            trace = "".join(traceback.format_tb(self.last_err[2])) + ("\n%s" % self.last_err[1])
            self.jtl_dict["responseMessage"] = exc_type_name

            sample = {}.fromkeys(JTL_ERR_ATRS)
            sample["t"] = self.jtl_dict["elapsed"]
            sample["lt"] = str(self.jtl_dict["Latency"])
            sample["ct"] = str(self.jtl_dict["Connect"])
            sample["ts"] = str(self.jtl_dict["timeStamp"])
            sample["s"] = self.jtl_dict["success"]
            sample["lb"] = self.jtl_dict["label"]
            sample["rc"] = self.jtl_dict["responseCode"]
            sample["rm"] = exc_type_name
            sample["tn"] = self.jtl_dict["threadName"]
            sample["dt"] = "text"
            sample["de"] = ""
            sample["by"] = str(len(trace))
            sample["ng"] = "1"
            sample["na"] = "1"

            self.error_writer.add_sample(sample, self.jtl_dict["label"], trace)

        self.csv_writer.writerow(self.jtl_dict)

        report_pattern = "%s.%s,Total:%d Pass:%d Failed:%d\n"
        sys.stdout.write(report_pattern % (
            self._module_name, self._method_name, self.test_count, self.success, self.test_count - self.success))

        self.out_stream.flush()


class JTLErrorWriter(object):
    def __init__(self, fds):
        self.out_file_fds = fds
        self.xml_writer = write_element(self.out_file_fds)
        next(self.xml_writer)

    def add_sample(self, sample, url, resp_data):
        new_sample = self.gen_http_sample(sample, url, resp_data)
        self.xml_writer.send(new_sample)

    @staticmethod
    def gen_http_sample(sample, url, resp_data):
        sample_element = etree.Element("httpSample", **sample)
        sample_element.append(JTLErrorWriter.gen_resp_header())
        sample_element.append(JTLErrorWriter.gen_req_header())
        sample_element.append(JTLErrorWriter.gen_resp_data(resp_data))
        sample_element.append(JTLErrorWriter.gen_cookies())
        sample_element.append(JTLErrorWriter.gen_method())
        sample_element.append(JTLErrorWriter.gen_query_string())
        sample_element.append(JTLErrorWriter.gen_url(url))
        return sample_element

    @staticmethod
    def gen_resp_header():
        resp_header = etree.Element("responseHeader")
        resp_header.set("class", "java.lang.String")
        return resp_header

    @staticmethod
    def gen_req_header():
        resp_header = etree.Element("requestHeader")
        resp_header.set("class", "java.lang.String")
        return resp_header

    @staticmethod
    def gen_resp_data(data):
        resp_data = etree.Element("responseData")
        resp_data.set("class", "java.lang.String")
        resp_data.text = data
        return resp_data

    @staticmethod
    def gen_cookies():
        cookies = etree.Element("cookies")
        cookies.set("class", "java.lang.String")
        return cookies

    @staticmethod
    def gen_method():
        method = etree.Element("method")
        method.set("class", "java.lang.String")
        return method

    @staticmethod
    def gen_query_string():
        qstring = etree.Element("queryString")
        qstring.set("class", "java.lang.String")
        return qstring

    @staticmethod
    def gen_url(url):
        url_element = etree.Element("java.net.URL")
        url_element.text = url
        return url_element

    def close(self):
        self.xml_writer.close()


def write_element(fds):
    with etree.xmlfile(fds, encoding="UTF-8") as xfds:
        xfds.write_declaration()
        with xfds.element('testResults', version="1.2"):
            try:
                while True:
                    elem = (yield)
                    xfds.write(elem)
                    xfds.flush()
            except GeneratorExit:
                pass


def run_nose(_output_file, _err_file, files, iterations, hold):
    argv = [__file__, '-v']
    argv.extend(files)
    argv += ['--with-nose_plugin'] + ['--nocapture']

    if iterations == 0:
        if hold > 0:
            iterations = sys.maxsize
        else:
            iterations = 1

    start_time = int(time())
    with TaurusNosePlugin(_output_file, _err_file) as plugin:
        for _ in irange(0, iterations):
            nose.run(addplugins=[plugin], argv=argv)
            if 0 < hold < int(time()) - start_time:
                break


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('-k', '--kpi-file', action='store')
    parser.add_option('-e', '--errors-file', action='store')
    parser.add_option('-i', '--iterations', action='store', default=0)
    parser.add_option('-d', '--duration', action='store', default=0)
    parser.add_option('-w', '--with-nose_plugin', action='store', default=0)

    opts, args = parser.parse_args()

    run_nose(opts.kpi_file, opts.errors_file, args, int(opts.iterations), float(opts.duration))
