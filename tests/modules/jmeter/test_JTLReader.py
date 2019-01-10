# coding=utf-8
import json
import os
import sys
import time
import unittest

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.jmeter import JTLErrorsReader, JTLReader, FuncJTLReader
from bzt.six import PY2
from bzt.utils import to_json
from tests import BZTestCase, RESOURCES_DIR, close_reader_file, ROOT_LOGGER
from tests.mocks import EngineEmul


class TestFuncJTLReader(BZTestCase):
    def setUp(self):
        super(TestFuncJTLReader, self).setUp()
        self.obj = None

    def configure(self, jtl_file):
        engine = EngineEmul()
        self.obj = FuncJTLReader(jtl_file, engine, ROOT_LOGGER)

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

    def configure(self, jtl_file, err_msg_sep=None):
        self.obj = JTLErrorsReader(jtl_file, ROOT_LOGGER, err_msg_separator=err_msg_sep)

    def tearDown(self):
        close_reader_file(self.obj)
        super(TestJTLErrorsReader, self).tearDown()

    def test_embedded_resources_no_fail(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-errors-no-fail.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(len(values.get('HTTP Request')), 1)
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "failed_resource_message")

    def test_embedded_resources_main_sample_fail_assert(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-errors-main-assert.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Test failed")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "Test failed")

    def test_embedded_resources_fail_child_no_assert(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-errors-child-no-assert.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "NOT FOUND")

    def test_embedded_resources_fail_child_assert(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-errors-child-assert.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(1, len(values.get("")))
        self.assertEqual(values.get('')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "NOT FOUND")

    def test_bug1(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-bug1.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(9, len(values.get("")))
        self.assertEqual(values.get('')[0].get("msg"), "Non HTTP response message: Connection reset")

    def test_short_err_message(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-mix.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Not Found")

    def test_full_err_message(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-mix.jtl", err_msg_sep=" *OMG!* ")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Not Found *OMG!* ")

    def test_puzzle_jtl(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-com.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Test failed: text expected not to contain /understanding/")

    def test_resource_tc(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource_tc.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(4, len(values.get("")))
        self.assertEqual(values.get('')[0].get("msg"), "message")
        self.assertEqual(values.get('')[1].get("msg"), "FOUND")
        self.assertEqual(values.get('')[2].get("msg"), "second message")
        self.assertEqual(values.get('')[3].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('')[3].get("cnt"), 2)

        self.assertEqual(values.get('tc1')[0].get("msg"), "FOUND")
        self.assertEqual(values.get("tc1")[0].get("type"), KPISet.ERRTYPE_SUBSAMPLE)
        self.assertEqual(values.get('tc3')[0].get("msg"), "message")
        self.assertEqual(values.get("tc3")[0].get("type"), KPISet.ERRTYPE_ERROR)
        self.assertEqual(values.get("tc3")[1].get("type"), KPISet.ERRTYPE_ERROR)
        self.assertEqual(values.get('tc3')[1].get("msg"), "second message")
        self.assertEqual(values.get('tc4')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get("tc4")[0].get("type"), KPISet.ERRTYPE_SUBSAMPLE)
        self.assertEqual(values.get('tc5')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get("tc5")[0].get("type"), KPISet.ERRTYPE_SUBSAMPLE)

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
        self.assertEquals(KPISet.ERRTYPE_ERROR, values[''][0]['type'])
        self.assertEqual(KPISet.ERRTYPE_ASSERT, values[""][1]["type"])
        self.assertIn("text expected to contain", values[""][1]["msg"])
        self.assertIn("Assert", values[""][1]["tag"])

    def test_embedded_errors(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource-error-embedded.jtl")
        self.obj.read_file(final_pass=True)
        values = self.obj.get_data(sys.maxsize)
        self.assertEquals(2, len(values))
        self.assertEquals(KPISet.ERRTYPE_SUBSAMPLE, values[''][0]['type'])
        self.assertEquals('404', values[''][0]['rc'])

    def test_error_parsing(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-parsing.jtl")
        self.obj.read_file(final_pass=True)
        values = self.obj.get_data(sys.maxsize)
        self.assertEquals(2, len(values))
        self.assertEquals(KPISet.ERRTYPE_ERROR, values[''][0]['type'])
        self.assertEquals('200', values[''][0]['rc'])

    @unittest.skipUnless(sys.platform == "darwin" and sys.version_info >= (3, 0), "MacOS- and Python3-only")
    def test_macos_unicode_parsing_is_not_supported(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl")
        self.obj.read_file(final_pass=True)  # shouldn't fail with "ParserError: Unicode parsing is not supported"


class TestJTLReader(BZTestCase):
    def setUp(self):
        super(TestJTLReader, self).setUp()
        self.obj = None

    def configure(self, jtl_file):
        self.obj = JTLReader(jtl_file, ROOT_LOGGER)

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

    def test_stdev_performance(self):
        start = time.time()
        self.configure(RESOURCES_DIR + "/jmeter/jtl/slow-stdev.jtl")
        res = list(self.obj.datapoints(final_pass=True))
        lst_json = to_json(res)

        self.assertNotIn('"perc": {},', lst_json)

        elapsed = time.time() - start
        ROOT_LOGGER.debug("Elapsed/per datapoint: %s / %s", elapsed, elapsed / len(res))
        # self.assertLess(elapsed, len(res))  # less than 1 datapoint per sec is a no-go
        exp = [2.2144798867972773,
               0.7207704268609725,
               0.606834452578833,
               0.8284089170237546,
               0.5858142211763572,
               0.622922628329711,
               0.5529488620851849,
               0.6933748292117727,
               0.4876162181858197,
               0.42471180222446503,
               0.2512251128133865]
        self.assertEqual(exp, [x[DataPoint.CURRENT][''][KPISet.STDEV_RESP_TIME] for x in res])

    def test_kpiset_trapped_getitem(self):
        def new():
            subj = KPISet(perc_levels=(100.0,))
            subj[KPISet.RESP_TIMES].add(0.1)
            subj[KPISet.RESP_TIMES].add(0.01)
            subj[KPISet.RESP_TIMES].add(0.001)
            subj.recalculate()
            return subj

        def enc_dec_iter(vals):
            vals = list(vals)
            dct = {x[0]: x[1] for x in vals}
            jsoned = to_json(dct)
            return json.loads(jsoned)

        exp = {u'avg_ct': 0,
               u'avg_lt': 0,
               u'avg_rt': 0,
               u'bytes': 0,
               u'concurrency': 0,
               u'errors': [],
               u'fail': 0,
               u'perc': {u'100.0': 0.1},
               u'rc': {},
               u'rt': {u'0.001': 1, u'0.01': 1, u'0.1': 1},
               u'stdev_rt': 0.058 if PY2 else 0.05802585630561603,
               u'succ': 0,
               u'throughput': 0}

        self.assertEqual(exp, enc_dec_iter(new().items()))
        if PY2:
            self.assertEqual(exp, enc_dec_iter(new().viewitems()))
            self.assertEqual(exp, enc_dec_iter(new().iteritems()))
        self.assertEqual('{"100.0": 0.1}', to_json(new().get(KPISet.PERCENTILES), indent=None))
