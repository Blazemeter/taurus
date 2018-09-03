from bzt import TaurusConfigError
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator
from bzt.modules.external import ExternalResultsLoader
from bzt.modules.jmeter import FuncJTLReader, JTLReader
from bzt.modules.ab import TSVDataReader
from bzt.modules.pbench import PBenchKPIReader
from bzt.modules.gatling import DataLogReader as GatlingLogReader
from bzt.modules.grinder import DataLogReader as GrinderLogReader

from tests import BZTestCase, RESOURCES_DIR, close_reader_file
from tests.mocks import EngineEmul, MockReader


class TestExternalResultsLoader(BZTestCase):
    def setUp(self):
        super(TestExternalResultsLoader, self).setUp()
        self.obj = ExternalResultsLoader()
        self.obj.engine = EngineEmul()

    def tearDown(self):
        if self.obj.reader:
            if isinstance(self.obj.reader, FuncJTLReader):
                close_reader_file(self.obj.reader)
            if isinstance(self.obj.reader, JTLReader):
                close_reader_file(self.obj.reader.csvreader)
                close_reader_file(self.obj.reader.errors_reader)
        super(TestExternalResultsLoader, self).tearDown()

    def configure(self, config):
        self.obj.engine.config.merge(config)
        execution = self.obj.engine.config['execution']
        if isinstance(execution, list):
            self.obj.execution = execution[0]
        else:
            self.obj.execution = execution

        self.results_listener = MockReader()
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.engine.aggregator.add_listener(self.results_listener)

    def test_no_data_file(self):
        self.configure({
            "execution": [{
                # empty execution
            }]
        })
        self.assertRaises(TaurusConfigError, self.obj.prepare)

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
                "errors-jtl": RESOURCES_DIR + "/jmeter/jtl/simple.error.jtl",
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

    def test_pbench(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/pbench/pbench-kpi.txt",
                "errors-jtl": RESOURCES_DIR + "/pbench/pbench-additional.ldjson"
            }]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.reader, PBenchKPIReader)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertEqual(8, cumulative_kpis[KPISet.SUCCESSES])

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

    def test_grinder(self):
        self.configure({
            "execution": [{
                "data-file": RESOURCES_DIR + "/grinder/grinder-bzt-1-kpi.log",
            }]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.reader, GrinderLogReader)
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        self.assertGreater(len(results), 0)
        last_dp = results[-1]
        cumulative_kpis = last_dp[DataPoint.CUMULATIVE]['']
        self.assertEqual(50, cumulative_kpis[KPISet.SUCCESSES])
