# coding=utf-8
import logging
import os
import sys
import unittest

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.jmeter import JTLErrorsReader, JTLReader, FuncJTLReader
from tests import BZTestCase, RESOURCES_DIR, close_reader_file
from tests.mocks import EngineEmul


class TestFuncJTLReader(BZTestCase):
    def setUp(self):
        super(TestFuncJTLReader, self).setUp()
        self.obj = None

    def configure(self, jtl_file):
        engine = EngineEmul()
        self.obj = FuncJTLReader(jtl_file, engine, logging.getLogger(''))

    def tearDown(self):
        close_reader_file(self.obj)
        super(TestFuncJTLReader, self).tearDown()

    def test_functional_reader_pass(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-errors-no-fail.jtl")
        samples = list(self.obj.read())
        self.assertEqual(2, len(samples))
        first = samples[0]
        self.assertEqual(first.test_case, "HTTP Request")
        self.assertEqual(first.test_suite, "JMeter")
        self.assertEqual(first.status, "PASSED")
        self.assertEqual(first.start_time, 1440764640)
        self.assertEqual(first.duration, 0.419)
        self.assertEqual(first.error_msg, "")
        self.assertEqual(first.error_trace, "")

    def test_functional_reader_failed(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl")
        samples = list(self.obj.read())
        self.assertEqual(185, len(samples))
        first = samples[0]
        self.assertEqual(first.test_case, "http://blazedemo.com/some-more-or-less-long-label")
        self.assertEqual(first.test_suite, "JMeter")
        self.assertEqual(first.status, "FAILED")
        self.assertEqual(first.start_time, 1430825787)
        self.assertEqual(first.duration, 0.011)
        self.assertEqual(first.error_msg, "The operation lasted too long")

    def test_functional_reader_broken(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl")
        samples = list(self.obj.read())
        self.assertEqual(185, len(samples))
        sample = samples[8]
        self.assertEqual(sample.test_case, "http://blazedemo.com/some-more-or-less-long-label")
        self.assertEqual(sample.test_suite, "JMeter")
        self.assertEqual(sample.status, "BROKEN")
        self.assertEqual(sample.start_time, 1430825788)
        self.assertEqual(sample.duration, 0.01)
        self.assertEqual(sample.error_msg, "Non HTTP response message: Read timed out")
        self.assertTrue(sample.error_trace.startswith("java.net.SocketTimeoutException: Read timed out"))

    def test_functional_reader_extras(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/trace.jtl")
        samples = list(self.obj.read())
        self.assertEqual(1, len(samples))
        sample = samples[0]
        self.assertIsNotNone(sample.extras)
        fields = [
            'assertions', 'connectTime', 'latency', 'responseTime',
            'requestBody', 'requestBodySize', 'requestCookies', 'requestCookiesRaw', 'requestCookiesSize',
            'requestHeaders', 'requestMethod', 'requestSize', 'requestURI',
            'responseBody', 'responseBodySize', 'responseCode', 'responseHeaders',
            'responseMessage', 'responseSize',
            "threadId", "threadGroup"]
        for field in set(fields) - set(FuncJTLReader.FILE_EXTRACTED_FIELDS):
            self.assertIn(field, sample.extras)
        self.assertEqual(sample.extras["requestURI"], "http://blazedemo.com/")
        self.assertEqual(sample.extras["requestMethod"], "GET")

    def test_functional_reader_artifact_files(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/trace.jtl")
        samples = list(self.obj.read())
        self.assertEqual(1, len(samples))
        sample_path = os.path.join(self.obj.engine.artifacts_dir, "sample-responseBody.bin")
        self.assertTrue(os.path.exists(sample_path))

    def test_functional_reader_extras_assertions(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/trace.jtl")
        samples = list(self.obj.read())
        self.assertEqual(1, len(samples))
        sample = samples[0]
        self.assertIsNotNone(sample.extras)
        self.assertEqual(len(sample.extras["assertions"]), 2)
        first, second = sample.extras["assertions"]
        self.assertEqual(first, {"name": 'Passing Assertion',
                                 "isFailed": False,
                                 "errorMessage": ""})
        self.assertEqual(second, {"name": 'Failing Assertion',
                                  "isFailed": True,
                                  "errorMessage": "Test failed: text expected to contain /something/"})

    def test_functional_reader_extras_empty_body(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/cookies.jtl")
        samples = list(self.obj.read())
        self.assertEqual(2, len(samples))
        sample = samples[1]
        self.assertIsNotNone(sample.extras)
        self.assertEqual(sample.extras["requestCookies"], {'hello': 'world', 'visited': 'yes'})

    def test_functional_reader_extract(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/crash_trace.jtl")
        samples = list(self.obj.read())
        self.assertNotEqual(len(samples), 0)

    def test_unicode_errors(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/unicode-reqs.jtl")
        samples = list(self.obj.read())

        origin_string = u'чсмтчомтчжом'
        for sample in samples:
            self.assertEqual(sample.test_case, origin_string)


class TestJTLErrorsReader(BZTestCase):
    def setUp(self):
        super(TestJTLErrorsReader, self).setUp()
        self.obj = None

    def configure(self, jtl_file):
        self.obj = JTLErrorsReader(jtl_file, logging.getLogger(''))

    def tearDown(self):
        close_reader_file(self.obj)
        super(TestJTLErrorsReader, self).tearDown()

    def test_nonstandard_errors_unicode(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/nonstandard-unicode.jtl")
        self.obj.read_file(final_pass=True)
        values = self.obj.get_data(sys.maxsize)
        self.assertNotEquals(values[''][0]['msg'].find(u'Cannot find function error in object Файфокс'), -1)
        self.assertNotEquals(values[''][1]['msg'].find('Cannot find function error in object FirefoxDriver'), -1)

    def test_standard_errors_format(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl")
        self.obj.read_file(final_pass=True)
        values = self.obj.get_data(sys.maxsize)
        self.assertEquals(3, len(values))

    def test_embedded_errors(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-error-embedded.jtl")
        self.obj.read_file(final_pass=True)
        values = self.obj.get_data(sys.maxsize)
        self.assertEquals(2, len(values))
        self.assertEquals(KPISet.ERRTYPE_SUBSAMPLE, values[''][0]['type'])

    @unittest.skipUnless(sys.platform == "darwin" and sys.version_info >= (3, 0), "MacOS- and Python3-only")
    def test_macos_unicode_parsing_is_not_supported(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl")
        self.obj.read_file(final_pass=True)  # shouldn't fail with "ParserError: Unicode parsing is not supported"


class TestJTLReader(BZTestCase):
    def setUp(self):
        super(TestJTLReader, self).setUp()
        self.obj = None

    def configure(self, jtl_file):
        self.obj = JTLReader(jtl_file, logging.getLogger(''))

    def tearDown(self):
        if self.obj:
            close_reader_file(self.obj.csvreader)
            close_reader_file(self.obj.errors_reader)
        super(TestJTLReader, self).tearDown()

    def test_tranctl_jtl(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/tranctl.jtl")
        values = [x for x in self.obj.datapoints(final_pass=True)]
        self.assertEquals(1, len(values))

    def test_tabs_jtl(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/tabs.jtl")
        values = [x for x in self.obj.datapoints(final_pass=True)]
        self.assertEquals(4, len(values))

    def test_reader_unicode(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/unicode.jtl")
        self.obj.ignored_labels = [u"Тест.Эхо"]
        for point in self.obj.datapoints(final_pass=True):
            cumulative = point[DataPoint.CUMULATIVE]
            self.assertIn(u"САП.АутентифицироватьРасш", cumulative)
            self.assertNotIn(u"Тест.Эхо", cumulative)

    def test_jtl_doublequoting(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/doublequoting.jtl")
        list(self.obj.datapoints(final_pass=True))

    def test_jtl_csv_sniffer_unicode_crash(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/quote-guessing-crash.jtl")
        list(self.obj.datapoints(final_pass=True))
