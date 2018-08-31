from bzt import TaurusConfigError
from bzt.modules.jmeter import JTLLoaderExecutor, FuncJTLReader, JTLReader
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.functional import FunctionalAggregator

from tests import BZTestCase, close_reader_file, RESOURCES_DIR
from tests.mocks import EngineEmul, MockReader, MockFunctionalListener


class TestJTLLoader(BZTestCase):
    def setUp(self):
        super(TestJTLLoader, self).setUp()
        self.obj = JTLLoaderExecutor()
        self.obj.engine = EngineEmul()

    def tearDown(self):
        if self.obj.reader:
            if isinstance(self.obj.reader, FuncJTLReader):
                close_reader_file(self.obj.reader)
            if isinstance(self.obj.reader, JTLReader):
                close_reader_file(self.obj.reader.csvreader)
                close_reader_file(self.obj.reader.errors_reader)
        super(TestJTLLoader, self).tearDown()

    def configure(self, config, functional_mode=False):
        self.obj.engine.config.merge(config)
        execution = self.obj.engine.config['execution']
        if isinstance(execution, list):
            self.obj.execution = execution[0]
        else:
            self.obj.execution = execution

        if functional_mode:
            self.results_listener = MockFunctionalListener()
            self.obj.engine.aggregator = FunctionalAggregator()
            self.obj.engine.aggregator.add_listener(self.results_listener)
        else:
            self.results_listener = MockReader()
            self.obj.engine.aggregator = ConsolidatingAggregator()
            self.obj.engine.aggregator.add_listener(self.results_listener)

    def test_plain(self):
        self.configure({
            "execution": [{
                "kpi-jtl": RESOURCES_DIR + "/jmeter/jtl/simple.kpi.jtl"
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

    def test_no_kpi_jtl(self):
        self.configure({
            "execution": [{
                # empty execution
            }]
        })
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_errors_jtl(self):
        self.configure({
            "execution": [{
                "kpi-jtl": RESOURCES_DIR + "/jmeter/jtl/simple.kpi.jtl",
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

    def test_functional(self):
        self.configure({
            "execution": [{
                "errors-jtl": RESOURCES_DIR + "/jmeter/jtl/trace.jtl",
            }]
        }, functional_mode=True)
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        results = self.results_listener.results
        last = results[-1]
        samples = last['JMeter']
        self.assertEqual('FAILED', samples[0].status)
        self.assertEqual('Test failed: text expected to contain /something/', samples[0].error_msg)
        self.assertEqual('blazedemo-index', samples[0].test_case)
        self.assertEqual('JMeter', samples[0].test_suite)
