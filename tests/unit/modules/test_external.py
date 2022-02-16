from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator, SAMPLE_STATES
from bzt.modules.external import ExternalResultsLoader
from bzt.modules.jmeter import FuncJTLReader, JTLReader
from bzt.modules.ab import TSVDataReader
from bzt.modules.gatling import DataLogReader as GatlingLogReader
from bzt.modules.vegeta import VegetaLogReader

from tests.unit import RESOURCES_DIR, close_reader_file, ExecutorTestCase
from tests.unit.mocks import MockReader, MockListener


class TestExternalResultsLoader(ExecutorTestCase):
    EXECUTOR = ExternalResultsLoader

    def tearDown(self):
        if self.obj.reader:
            if isinstance(self.obj.reader, FuncJTLReader):
                close_reader_file(self.obj.reader)
            if isinstance(self.obj.reader, JTLReader):
                close_reader_file(self.obj.reader.csvreader)
                close_reader_file(self.obj.reader.errors_reader)
        super(TestExternalResultsLoader, self).tearDown()

    def configure(self, config):
        super(TestExternalResultsLoader, self).configure(config)
        self.results_listener = MockReader()
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.engine.aggregator.engine = self.obj.engine
        self.obj.engine.aggregator.add_listener(self.results_listener)

    def test_ext_agg_concurrency(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/kpi1.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/error1.jtl",
            }]
        })
        self.obj.engine.aggregator.set_aggregation(True)
        self.obj.prepare()
        self.obj.reader.set_aggregation(True)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results

        cons1 = {}
        for dp in results:
            current = dp[DataPoint.CURRENT]
            cons1[dp['ts']] = {state: current[state].concurrency for state in current if state != ''}

        sample_cons1 = {
            1637589158:
                {'good-success': 1, 'bad-http_errors': 1},
            1637589159:
                {'good-success': 3, 'bad-http_errors': 3},
            1637589160:
                {'good-success': 4, 'bad-http_errors': 4},
            1637589161:
                {'good-success': 3, 'bad-http_errors': 1}}

        self.assertEqual(cons1, sample_cons1)

        cons2 = dict()

        converted_results = [self.obj.engine.aggregator.converter(dp) for dp in results]
        for dp in converted_results:
            cons2[dp['ts']] = dict()
            for label in dp[DataPoint.CURRENT]:
                label_dst = cons2[dp[DataPoint.TIMESTAMP]][label] = dict()
                label_src = dp[DataPoint.CURRENT][label]
                for state in label_src:
                    label_dst[state] = label_src[state].concurrency

        sample_cons2 = {
            1637589158: {
                '': {
                    'all_transactions_aggregated': 1,
                    'http_errors': 1,
                    'success_http_errors': 1,
                    'http_errors_jmeter_errors': 1,
                    'success': 1,
                    'success_jmeter_errors': 1},
                'bad': {
                    'all_transactions_aggregated': 1,
                    'http_errors': 1,
                    'success_http_errors': 1,
                    'http_errors_jmeter_errors': 1},
                'good': {
                    'all_transactions_aggregated': 1,
                    'success': 1,
                    'success_jmeter_errors': 1,
                    'success_http_errors': 1}},
            1637589159: {
                '': {
                    'all_transactions_aggregated': 3,
                    'http_errors': 3,
                    'success_http_errors': 3,
                    'http_errors_jmeter_errors': 3,
                    'success': 3,
                    'success_jmeter_errors': 3},
                'bad': {
                    'all_transactions_aggregated': 3,
                    'http_errors': 3,
                    'success_http_errors': 3,
                    'http_errors_jmeter_errors': 3},
                'good': {
                    'all_transactions_aggregated': 3,
                    'success': 3,
                    'success_jmeter_errors': 3,
                    'success_http_errors': 3}},
            1637589160: {
                '': {
                    'all_transactions_aggregated': 4,
                    'http_errors': 4,
                    'success_http_errors': 4,
                    'http_errors_jmeter_errors': 4,
                    'success': 4,
                    'success_jmeter_errors': 4},
                'bad': {
                    'all_transactions_aggregated': 4,
                    'http_errors': 4,
                    'success_http_errors': 4,
                    'http_errors_jmeter_errors': 4},
                'good': {
                    'all_transactions_aggregated': 4,
                    'success': 4,
                    'success_jmeter_errors': 4,
                    'success_http_errors': 4}},
            1637589161: {
                '': {
                    'all_transactions_aggregated': 4,
                    'http_errors': 1,
                    'success_http_errors': 4,
                    'http_errors_jmeter_errors': 1,
                    'success': 3,
                    'success_jmeter_errors': 3},
                'bad': {
                    'all_transactions_aggregated': 1,
                    'http_errors': 1,
                    'success_http_errors': 1,
                    'http_errors_jmeter_errors': 1},
                'good': {
                    'all_transactions_aggregated': 3,
                    'success': 3,
                    'success_jmeter_errors': 3,
                    'success_http_errors': 3}}}

        # the same as self.assertEqual(cons2, sample_cons2) but much better for debug purpose
        for ts in cons2:
            for label in cons2[ts]:
                for state in cons2[ts][label]:
                    self.assertEqual(cons2[ts][label][state], sample_cons2[ts][label][state])

    def test_ext_agg_embedded(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/embedded_resources/kpi_ed.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/embedded_resources/error_ed.jtl",
            }]
        })
        ext_mode = True
        self.obj.engine.aggregator.set_aggregation(ext_mode)
        self.obj.prepare()
        self.obj.reader.set_aggregation(ext_mode)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results

        cons1 = {}
        for dp in results:
            current = dp[DataPoint.CURRENT]
            cons1[dp['ts']] = {state: current[state].concurrency for state in current if state != ''}

        if ext_mode:
            converted_results = [self.obj.engine.aggregator.converter(dp) for dp in results]
            first_current = converted_results[0][DataPoint.CURRENT]
            error_type = first_current['Request 001']['all_transactions_aggregated'][KPISet.ERRORS][0]['type']
            self.assertEqual(2, error_type)
            self.assertIn(SAMPLE_STATES[2], first_current[''])  # http_errors
            self.assertNotIn(SAMPLE_STATES[1], first_current[''])  # not jmeter_errors

    def test_non_http_exception(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/non-http/kpi.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/non-http/error.jtl",
            }]
        })
        ext_mode = True
        self.obj.engine.aggregator.set_aggregation(ext_mode)
        self.obj.prepare()
        self.obj.reader.set_aggregation(ext_mode)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results

        cons1 = {}
        for dp in results:
            current = dp[DataPoint.CURRENT]
            cons1[dp['ts']] = {state: current[state].concurrency for state in current if state != ''}

        if ext_mode:
            converted_results = [self.obj.engine.aggregator.converter(dp) for dp in results]
            currents = [converted_results[i][DataPoint.CURRENT] for i in range(len(converted_results))]
            overall = currents[0]['']
            self.assertNotIn(SAMPLE_STATES[1], overall)     # there are no jmeter_errors

            http_errors = [err for err in overall[SAMPLE_STATES[2]][KPISet.ERRORS] if SAMPLE_STATES[2] in overall]
            non_http_line = "Non HTTP response message"
            non_http_in_http_errors = any([j_err['msg'].startswith(non_http_line) for j_err in http_errors])
            self.assertTrue(non_http_in_http_errors, f"'{non_http_line}' not found in http_errors")

    def test_no_data_file(self):
        self.configure({
            "execution": [{
                # empty execution
            }]
        })
        self.assertRaises(AssertionError, self.obj.prepare)

    def test_plain(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/simple.kpi.jtl"
            }]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.reader, JTLReader)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertIn('200', cumulative_kpis[KPISet.RESP_CODES])

    def test_errors_jtl(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/simple.kpi.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/simple.error.jtl",
            }]
        })
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertIn('200', cumulative_kpis[KPISet.RESP_CODES])
        self.assertIn('404', cumulative_kpis[KPISet.RESP_CODES])

    def test_500_success(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/500-success/kpi.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/500-success/error.jtl",
            }]
        })
        self.obj.engine.aggregator.set_aggregation(True)
        self.obj.prepare()
        self.obj.reader.set_aggregation(True)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        converted = self.obj.engine.aggregator.converter(results[-1])[DataPoint.CURRENT]
        self.assertIn(SAMPLE_STATES[0], converted[''])  # success
        self.assertNotIn(SAMPLE_STATES[2], converted[''])  # http_errors

    def test_errors_jtl_ext(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/simple.kpi.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/simple.error.jtl",
            }]
        })
        self.obj.engine.aggregator.set_aggregation(True)
        self.obj.prepare()
        self.obj.reader.set_aggregation(True)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertIn('200', cumulative_kpis[KPISet.RESP_CODES])
        self.assertIn('404', cumulative_kpis[KPISet.RESP_CODES])

    def test_extend_data_and_assertion(self):
        # check aggregated results for error details in the corresponding state record:
        #   'current': {
        #     <label>:
        #       {'jmeter_errors': {..
        #           'errors': [{'msg':..., 'tag':..., ...}, ...]..}, # there must be real jmeter errors and nothing more
        #        'http_errors': {..}, 'success': {..}, ...}}, <other_labels>...}
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/kpi-pair1.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/error-pair1.jtl",
            }]
        })
        self.engine.aggregator.settings['extend-aggregation'] = True
        watcher = MockListener()
        watcher.engine = self.obj.engine

        self.engine.aggregator.prepare()
        self.obj.prepare()
        self.engine.aggregator.add_listener(watcher)
        self.engine.aggregator.startup()
        self.obj.startup()

        while not self.obj.check():
            self.engine.aggregator.check()

        self.obj.shutdown()
        self.engine.aggregator.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        dp = watcher.results[0]['current']

        success_label = 'Response Code 200'
        self.assertEqual([], dp[success_label][SAMPLE_STATES[0]][KPISet.ERRORS])

        jmeter_error_label = 'Invalid Assert (No results for path)'
        sample_jmeter_error = ('No results for path: $[\'error\']', 'JSON Path Assertion - No results for path')
        jmeter_errors_err = [(e['msg'], e['tag']) for e in dp[jmeter_error_label][SAMPLE_STATES[1]][KPISet.ERRORS]]
        self.assertEqual({sample_jmeter_error}, set(jmeter_errors_err))

        http_error_label = 'Response Code 400'
        sample_http_error = ('Bad Request', None)
        http_errors_err = [(e['msg'], e['tag']) for e in dp[http_error_label][SAMPLE_STATES[2]][KPISet.ERRORS]]
        self.assertEqual({sample_http_error}, set(http_errors_err))

    def test_sum_of_errors(self):
        # get_mixed_label was called even in not extended_aggregation case
        # it caused lack of error info in label' kpi sets (but overall ('') errors list was correct).
        # DE520333
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/kpi-pair1.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/error-pair1.jtl",
            }]
        })
        self.engine.aggregator.settings['extend-aggregation'] = False
        watcher = MockListener()
        watcher.engine = self.obj.engine

        self.engine.aggregator.prepare()
        self.obj.prepare()
        self.engine.aggregator.add_listener(watcher)
        self.engine.aggregator.startup()
        self.obj.startup()

        while not self.obj.check():
            self.engine.aggregator.check()

        self.obj.shutdown()
        self.engine.aggregator.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        dp = watcher.results[0]['current']

        assertions = {label: 0 for label in dp}
        http_errors = {label: 0 for label in dp}
        for label in dp:
            for error in dp[label]['errors']:
                if error['type'] == 1:
                    assertions[label] += error['cnt']
                elif error['type'] == 2:
                    http_errors[label] += error['cnt']

        sum_of_assertions = sum([assertions[label] for label in assertions if label != ''])
        sum_of_http_errors = sum([http_errors[label] for label in assertions if label != ''])
        self.assertEqual(sum_of_assertions, assertions[''])
        self.assertEqual(sum_of_http_errors, http_errors[''])

    def test_errors_jtl2(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/jmeter/jtl/kpi-pair1.jtl",
                "errors-file": RESOURCES_DIR + "/jmeter/jtl/error-pair1.jtl",
            }]
        })
        self.obj.prepare()
        self.obj.startup()
        cnt = 0
        while not self.obj.check():
            self.engine.aggregator.check()
            cnt += 1
            self.assertLess(cnt, 10)
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertEqual(8, cumulative_kpis[KPISet.SAMPLE_COUNT])

    def test_execution_file_parsing(self):
        datafile_pair = RESOURCES_DIR + "jmeter/jtl/kpi-pair2.jtl"
        errorfile_pair = RESOURCES_DIR + "jmeter/jtl/error-pair2.jtl"
        self.configure({
            "execution": [{
                "data-file": datafile_pair,
                "errors-file": errorfile_pair,
            }]
        })
        self.obj.prepare()
        self.assertEqual(datafile_pair.replace('\\', '/'), self.obj.data_file.replace('\\', '/'))
        self.assertEqual(errorfile_pair.replace('\\', '/'), self.obj.errors_file.replace('\\', '/'))

    def test_execution_parsing_priority(self):
        datafile_pair = RESOURCES_DIR + "jmeter/jtl/kpi-pair1.jtl"
        errorfile_pair = RESOURCES_DIR + "jmeter/jtl/error-pair1.jtl"
        another_datafile_pair = RESOURCES_DIR + "jmeter/jtl/kpi-pair2.jtl"
        another_errorfile_pair = RESOURCES_DIR + "jmeter/jtl/error-pair2.jtl"
        self.configure({
            "execution": [{
                "data-file": datafile_pair,
                "errors-file": errorfile_pair,
                "scenario": "sample"
            }],
            "scenarios": {
                "sample": {
                    "data-file": another_datafile_pair,
                    "errors-file": another_errorfile_pair,
                }
            }
        })
        self.obj.prepare()
        self.assertEqual(datafile_pair.replace('\\', '/'), self.obj.data_file.replace('\\', '/'))
        self.assertEqual(errorfile_pair.replace('\\', '/'), self.obj.errors_file.replace('\\', '/'))

    def test_ab(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/ab/ab.tsv"
            }]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.reader, TSVDataReader)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertEqual(10, cumulative_kpis[KPISet.SUCCESSES])

    def test_gatling(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/gatling/gatling-3-000/simulation.log",
            }]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.reader, GatlingLogReader)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertEqual(243, cumulative_kpis[KPISet.SUCCESSES])

    def test_vegeta(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/vegeta/vegeta_kpi.csv",
            }]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.reader, VegetaLogReader)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertEqual(4, cumulative_kpis[KPISet.SUCCESSES] + cumulative_kpis[KPISet.FAILURES])
