# coding=utf-8
import os
import sys

import logging

from bzt.modules.jmeter import JTLErrorsReader, JTLReader, FuncJTLReader
from bzt.six import u
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul
from bzt.modules.aggregator import DataPoint


class TestJTLReader(BZTestCase):
    def test_functional_reader_pass(self):
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/resource-errors-no-fail.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
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
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
        self.assertEqual(185, len(samples))
        first = samples[0]
        self.assertEqual(first.test_case, "http://blazedemo.com/some-more-or-less-long-label")
        self.assertEqual(first.test_suite, "JMeter")
        self.assertEqual(first.status, "FAILED")
        self.assertEqual(first.start_time, 1430825787)
        self.assertEqual(first.duration, 0.011)
        self.assertEqual(first.error_msg, "The operation lasted too long")

    def test_functional_reader_broken(self):
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
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
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/trace.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
        self.assertEqual(1, len(samples))
        sample = samples[0]
        self.assertIsNotNone(sample.extras)
        fields = [
            'assertions', 'connectTime', 'latency', 'responseTime',
            'requestBody', 'requestBodySize', 'requestCookies', 'requestCookiesRaw', 'requestCookiesSize',
            'requestHeaders', 'requestMethod', 'requestSize', 'requestURI',
            'responseBody', 'responseBodySize', 'responseCode', 'responseHeaders',
            'responseMessage', 'responseSize',
            "threadId", "threadGroup",
        ]
        for field in set(fields) - set(FuncJTLReader.FILE_EXTRACTED_FIELDS):
            self.assertIn(field, sample.extras)
        self.assertEqual(sample.extras["requestURI"], "http://blazedemo.com/")
        self.assertEqual(sample.extras["requestMethod"], "GET")

    def test_functional_reader_artifact_files(self):
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/trace.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
        self.assertEqual(1, len(samples))
        self.assertTrue(os.path.exists(os.path.join(engine_obj.artifacts_dir, "sample-responseBody.bin")))

    def test_functional_reader_extras_assertions(self):
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/trace.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
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
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/cookies.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
        self.assertEqual(2, len(samples))
        sample = samples[1]
        self.assertIsNotNone(sample.extras)
        self.assertEqual(sample.extras["requestCookies"], {'hello': 'world', 'visited': 'yes'})

    def test_functional_reader_extract(self):
        engine_obj = EngineEmul()
        obj = FuncJTLReader(RESOURCES_DIR + "/jmeter/jtl/crash_trace.jtl",
                            engine_obj,
                            logging.getLogger(''))
        samples = list(obj.read(last_pass=True))
        self.assertNotEqual(len(samples), 0)

    def test_nonstandard_errors_format(self):
        obj = JTLErrorsReader(RESOURCES_DIR + "/jmeter/jtl/nonstandard-errors.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertNotEquals(values[''][0]['msg'].find('Cannot find function error in object FirefoxDriver'), -1)

    def test_standard_errors_format(self):
        obj = JTLErrorsReader(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEquals(3, len(values))

    def test_tranctl_jtl(self):
        obj = JTLReader(RESOURCES_DIR + "/jmeter/jtl/tranctl.jtl", logging.getLogger(''), None)
        values = [x for x in obj.datapoints(True)]
        self.assertEquals(1, len(values))

    def test_tabs_jtl(self):
        obj = JTLReader(RESOURCES_DIR + "/jmeter/jtl/tabs.jtl", logging.getLogger(''), None)
        values = [x for x in obj.datapoints(True)]
        self.assertEquals(4, len(values))

    def test_reader_unicode(self):
        reader = JTLReader(RESOURCES_DIR + "/jmeter/jtl/unicode.jtl", logging.getLogger(''), None)
        reader.ignored_labels = [u("Тест.Эхо")]
        for point in reader.datapoints():
            cumulative = point[DataPoint.CUMULATIVE]
            self.assertNotIn("Тест.Эхо", cumulative)

    def test_jtl_doublequoting(self):
        obj = JTLReader(RESOURCES_DIR + "/jmeter/jtl/doublequoting.jtl", logging.getLogger(), None)
        list(obj.datapoints(True))

