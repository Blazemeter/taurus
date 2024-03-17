# coding=utf-8
import json
import os
import sys
import time
import unittest

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.jmeter import JTLErrorsReader, JTLReader, FuncJTLReader
from bzt.utils import to_json
from tests.unit import BZTestCase, RESOURCES_DIR, close_reader_file, ROOT_LOGGER, EngineEmul


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


TEST_ERROR_RESPONSE_BODIES_SIZE_LIMIT: int = 256 * 1024
TEST_ERROR_RESPONSE_BODIES_LIMIT: int = 10


class TestJTLErrorsReader(BZTestCase):
    def setUp(self):
        super(TestJTLErrorsReader, self).setUp()
        self.obj = None

    def configure(self, jtl_file, err_msg_sep=None):
        self.obj = JTLErrorsReader(jtl_file, ROOT_LOGGER, err_msg_separator=err_msg_sep)

    def tearDown(self):
        close_reader_file(self.obj)
        super(TestJTLErrorsReader, self).tearDown()

    def test_error_responses_collection_settings(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/simple.error.jtl")
        self.assertFalse(self.obj.collect_error_response_bodies)
        self.assertEqual(self.obj.error_response_bodies_limit, TEST_ERROR_RESPONSE_BODIES_LIMIT)
        self.assertEqual(self.obj.error_response_bodies_size_limit, TEST_ERROR_RESPONSE_BODIES_SIZE_LIMIT)

    def test_error_responses_collection_disabled(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/huge.error.response.jtl")
        self.obj.collect_error_response_bodies = False

        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)

        label_data = values.get('http://blazedemo.com/not-found')
        self.assertEqual(len(label_data), 1)

        response_data = label_data[0]
        self.assertEqual(3, response_data['cnt'])
        self.assertEqual('404', response_data['rc'])
        self.assertEqual(0, len(response_data['responseBodies']))

    def test_error_responses_collection(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/huge.error.response.jtl")
        self.obj.collect_error_response_bodies = True

        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)

        label_data = values.get('http://blazedemo.com/not-found')
        self.assertEqual(len(label_data), 1)

        response_data = label_data[0]
        self.assertEqual(3, response_data['cnt'])
        self.assertEqual('404', response_data['rc'])
        self.assertEqual(1, len(response_data['responseBodies']))

        error_response_data = response_data['responseBodies'][0]
        self.assertEqual(3, error_response_data['cnt'])
        self.assertEqual(333866, error_response_data['original_size'])

        # content is expected to be trimmed to max supported size
        self.assertEqual(TEST_ERROR_RESPONSE_BODIES_SIZE_LIMIT, len(error_response_data['content']))

    def test_error_responses_limits_unique(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/many-errors-unique-responses.jtl")
        self.obj.collect_error_response_bodies = True

        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)

        for label in ['find', 'edit', 'submit']:
            label_data = values.get(label)
            self.assertEqual(len(label_data), 1)

            response_data = label_data[0]
            self.assertEqual(63, response_data['cnt'])
            self.assertEqual('401', response_data['rc'])
            self.assertEqual(TEST_ERROR_RESPONSE_BODIES_LIMIT, len(response_data['responseBodies']))

            for error_response_data in response_data['responseBodies']:
                self.assertEqual(1, error_response_data['cnt'])
                self.assertEqual(156, error_response_data['original_size'])

        label_data = values.get('')
        self.assertEqual(len(label_data), 1)
        response_data = label_data[0]
        self.assertEqual(189, response_data['cnt'])
        self.assertEqual('401', response_data['rc'])
        self.assertEqual(30, len(response_data['responseBodies']))

    def test_error_responses_empty(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/many-errors-empty-responses.jtl")
        self.obj.collect_error_response_bodies = True

        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)

        label_data = values.get('edit')
        self.assertEqual(len(label_data), 1)
        response_data = label_data[0]
        self.assertEqual(14, response_data['cnt'])
        self.assertEqual('401', response_data['rc'])
        self.assertEqual(1, len(response_data['responseBodies']))
        self.assertEqual(14, response_data['responseBodies'][0]['cnt'])

        label_data = values.get('')
        self.assertEqual(len(label_data), 1)
        response_data = label_data[0]
        self.assertEqual(42, response_data['cnt'])
        self.assertEqual('401', response_data['rc'])
        self.assertEqual(1, len(response_data['responseBodies']))
        self.assertEqual(42, response_data['responseBodies'][0]['cnt'])

    def test_error_responses_limits_duplicates(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/many-errors-duplicated-responses.jtl")
        self.obj.collect_error_response_bodies = True

        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)

        label_data = values.get('')
        self.assertEqual(len(label_data), 5)

        test_data = [
            {'filter': lambda ld: ld['rc'] == '401' and ld['msg'] == 'Unauthorized', 'cnt': 139, 'resp': 21},
            {'filter': lambda ld: ld['rc'] == '402' and ld['msg'] == 'Unauthorized 402', 'cnt': 7, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '403' and ld['msg'] == 'Unauthorized 403', 'cnt': 15, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '403' and ld['msg'] == 'NF', 'cnt': 20, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '403' and ld['msg'] == 'SU', 'cnt': 8, 'resp': 1},
        ]
        for td in test_data:
            response_data = next(filter(td['filter'], label_data), None)
            self.assertEqual(td['cnt'], response_data['cnt'])
            self.assertEqual(td['resp'], len(response_data['responseBodies']))

        label_data = values.get('find')
        self.assertEqual(len(label_data), 5)

        test_data = [
            {'filter': lambda ld: ld['rc'] == '401' and ld['msg'] == 'Unauthorized', 'cnt': 13, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '402' and ld['msg'] == 'Unauthorized 402', 'cnt': 7, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '403' and ld['msg'] == 'Unauthorized 403', 'cnt': 15, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '403' and ld['msg'] == 'NF', 'cnt': 20, 'resp': 1},
            {'filter': lambda ld: ld['rc'] == '403' and ld['msg'] == 'SU', 'cnt': 8, 'resp': 1},
        ]
        for td in test_data:
            response_data = next(filter(td['filter'], label_data), None)
            self.assertEqual(td['cnt'], response_data['cnt'])
            self.assertEqual(td['resp'], len(response_data['responseBodies']))

        for label in ['edit', 'submit']:
            label_data = values.get(label)
            self.assertEqual(len(label_data), 1)
            response_data = label_data[0]
            self.assertEqual(TEST_ERROR_RESPONSE_BODIES_LIMIT, len(response_data['responseBodies']))

    def test_error_response_bodies_in_assertions(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-assertions.jtl")
        self.obj.collect_error_response_bodies = True
        self.obj.read_file()

        values = self.obj.get_data(sys.maxsize)

        self.assertEqual(10, len(values))
        aggregated_label = values.get('')
        self.assertEqual(1, len(aggregated_label))
        self.assertEqual(aggregated_label[0].get("msg"), "Test failed: text expected to contain /.*Robocop.*/")
        self.assertEqual(90, len(aggregated_label[0].get("responseBodies")))

    def test_smart_aggregation_assert(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/smart-aggregation/errors.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        # todo: process rule according to assertion

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

    def test_assertion_ts(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-bug1.jtl")
        self.obj.read_file()
        first_ts = int(list(self.obj.buffer.keys())[0])
        values = self.obj.get_data(first_ts)
        self.assertTrue(values)

    def test_short_err_message(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-mix.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Not Found")

    def test_full_err_message(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-mix.jtl", err_msg_sep=" *OMG!* ")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Test failed: code expected to contain /777/ *OMG!* Not Found")

    def test_puzzle_jtl(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-puzzle.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Test failed: text expected not to contain /understanding/")

    def test_resource_tc(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/resource_tc.jtl")
        self.obj.read_file()
        values = self.obj.get_data(sys.maxsize)
        self.assertEqual(3, len(values.get("")))
        self.assertEqual(values.get('')[0].get("msg"), "message")
        self.assertEqual(values.get('')[1].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('')[1].get("cnt"), 3)
        self.assertEqual(values.get('')[2].get("msg"), "second message")

        self.assertEqual(values.get('tc1')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get("tc1")[0].get("type"), KPISet.ERRTYPE_SUBSAMPLE)
        self.assertEqual(values.get('tc3')[0].get("msg"), "message")
        self.assertEqual(values.get("tc3")[0].get("type"), KPISet.ERRTYPE_ERROR)
        self.assertEqual(values.get('tc3')[1].get("msg"), "second message")
        self.assertEqual(values.get("tc3")[1].get("type"), KPISet.ERRTYPE_ERROR)
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

    @unittest.skipUnless(sys.platform == "darwin", "MacOS-only")
    def test_macos_unicode_parsing_is_not_supported(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/standard-errors.jtl")
        self.obj.read_file(final_pass=True)  # shouldn't fail with "ParserError: Unicode parsing is not supported"

    def test_302(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/error-302.jtl")
        self.obj.read_file(final_pass=True)
        values = self.obj.get_data(sys.maxsize)

        assert_msg = "Test failed: text expected to contain /Dummy Sampler/"

        self.assertEqual(1, len(values.get("")))
        self.assertEqual(values.get('')[0].get("msg"), assert_msg)
        self.assertEqual(values.get('')[0].get("type"), KPISet.ERRTYPE_ASSERT)
        self.assertEqual(values.get('')[0].get("cnt"), 2)

        self.assertEqual(1, len(values.get("Transaction Controller 1")))
        self.assertEqual(values.get("Transaction Controller 1")[0].get("msg"), assert_msg)
        self.assertEqual(values.get("Transaction Controller 1")[0].get("type"), KPISet.ERRTYPE_ASSERT)
        self.assertEqual(values.get("Transaction Controller 1")[0].get("cnt"), 1)

        self.assertEqual(1, len(values.get("Transaction Controller 3")))
        self.assertEqual(values.get("Transaction Controller 3")[0].get("msg"), assert_msg)
        self.assertEqual(values.get("Transaction Controller 3")[0].get("type"), KPISet.ERRTYPE_ASSERT)
        self.assertEqual(values.get("Transaction Controller 3")[0].get("cnt"), 1)


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

    def test_smart_aggr(self):
        self.configure(RESOURCES_DIR + "/jmeter/jtl/smart-aggregation/kpi.jtl")
        values = [x for x in self.obj.datapoints(final_pass=True)]
        # todo: process rule according to assertion

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
        self.configure(RESOURCES_DIR + "jmeter/jtl/slow-stdev.jtl")
        res = list(self.obj.datapoints(final_pass=True))
        lst_json = to_json(res)

        self.assertNotIn('"perc": {},', lst_json)

        elapsed = time.time() - start
        ROOT_LOGGER.debug("Elapsed/per datapoint: %s / %s", elapsed, elapsed / len(res))
        # self.assertLess(elapsed, len(res))  # less than 1 datapoint per sec is a no-go
        exp = [0.53060066889723,
               0.39251356581014,
               0.388405157629,
               0.38927586980868,
               0.30511697736531,
               0.21160424043633,
               0.07339064994943]
        self.assertEqual(exp, [round(x[DataPoint.CURRENT][''][KPISet.STDEV_RESP_TIME], 14) for x in res])

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
               u'stdev_rt': 0.05802585630561603,
               u'succ': 0,
               u'throughput': 0}

        subj = new()
        items = list(subj.items()) + [('concurrency', subj['concurrency'])]
        self.assertEqual(exp, enc_dec_iter(items))
        self.assertEqual('{"100.0": 0.1}', to_json(new().get(KPISet.PERCENTILES), indent=None))
