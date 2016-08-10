import logging
import shutil

from bzt.modules.chrome import ChromeProfiler, Metrics, CPUProfileReader
from bzt.modules.monitoring import MonitoringListener
from bzt.six import iteritems
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestChromeProfiler(BZTestCase):
    def test_extraction(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "trace-file": "trace.json",
        })
        listener = RecordingListener()
        obj.add_listener(listener)

        shutil.copy(__dir__() + "/../chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        obj.check()

    def test_aggr_metrics(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "trace-file": "trace.json",
        })

        shutil.copy(__dir__() + "/../chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        obj.check()

        metrics = obj.get_aggr_metrics()
        self.assertAlmostEqual(metrics[Metrics.PAGE_LOAD_TIME], 5.27, delta=0.01)
        self.assertAlmostEqual(metrics[Metrics.DOM_CONTENT_LOADED_TIME], 3.00, delta=0.01)
        self.assertAlmostEqual(metrics[Metrics.FULL_LOAD_TIME], 8.25, delta=0.01)
        self.assertAlmostEqual(metrics[Metrics.FIRST_PAINT_TIME], 2.83, delta=0.01)
        self.assertAlmostEqual(metrics[Metrics.NETWORK_FOOTPRINT], 2.952, delta=0.001)
        self.assertAlmostEqual(metrics[Metrics.NETWORK_TTFB], 1.45, delta=0.001)
        self.assertEqual(metrics[Metrics.NETWORK_REQUESTS], 200)
        self.assertEqual(metrics[Metrics.NETWORK_XHR_REQUESTS], 21)
        self.assertAlmostEqual(metrics[Metrics.JS_GC_TIME], 0.0464, delta=0.0001)

    def test_calc_metrics(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "trace-file": "trace.json",
        })
        listener = RecordingListener()
        obj.add_listener(listener)

        shutil.copy(__dir__() + "/../chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        obj.check()

        dom_docs = listener.metrics_of_type(Metrics.DOM_DOCUMENTS)
        self.assertEqual(len(dom_docs), 16)
        self.assertEqual(dom_docs[0][Metrics.DOM_DOCUMENTS], 1)
        self.assertEqual(dom_docs[-1][Metrics.DOM_DOCUMENTS], 15)

        dom_nodes = listener.metrics_of_type(Metrics.DOM_NODES)
        self.assertEqual(len(dom_nodes), 16)
        self.assertEqual(dom_nodes[0][Metrics.DOM_NODES], 4)
        self.assertEqual(dom_nodes[-1][Metrics.DOM_NODES], 1989)

        listeners = listener.metrics_of_type(Metrics.JS_EVENT_LISTENERS)
        self.assertEqual(len(listeners), 16)
        self.assertEqual(listeners[0][Metrics.JS_EVENT_LISTENERS], 0)
        self.assertEqual(listeners[-1][Metrics.JS_EVENT_LISTENERS], 470)

        browser = listener.metrics_of_type(Metrics.MEMORY_BROWSER)
        self.assertEqual(len(browser), 1)
        self.assertAlmostEqual(browser[0][Metrics.MEMORY_BROWSER], 97.25, delta=0.1)

        per_tab = listener.metrics_of_type(Metrics.MEMORY_TAB)
        self.assertEqual(len(per_tab), 1)
        self.assertAlmostEqual(per_tab[0][Metrics.MEMORY_TAB], 97.25, delta=0.1)

        heap_size = listener.metrics_of_type(Metrics.MEMORY_JS_HEAP)
        self.assertEqual(len(heap_size), 16)
        self.assertAlmostEqual(heap_size[0][Metrics.MEMORY_JS_HEAP], 0.89, delta=0.1)
        self.assertAlmostEqual(heap_size[-1][Metrics.MEMORY_JS_HEAP], 50.76, delta=0.1)

        js_cpu = listener.metrics_of_type(Metrics.JS_CPU_UTILIZATION)
        self.assertEqual(len(js_cpu), 15)
        self.assertAlmostEqual(js_cpu[0][Metrics.JS_CPU_UTILIZATION], 26.0, delta=0.1)
        self.assertAlmostEqual(js_cpu[1][Metrics.JS_CPU_UTILIZATION], 98.4, delta=0.1)
        self.assertAlmostEqual(js_cpu[2][Metrics.JS_CPU_UTILIZATION], 132.4, delta=0.1)
        self.assertAlmostEqual(js_cpu[-1][Metrics.JS_CPU_UTILIZATION], 1.6, delta=0.1)

    def test_cpuprofile_reader(self):
        obj = CPUProfileReader(__dir__() + "/../chrome/js.cpuprofile", logging.getLogger())
        stats, totals = obj.extract_js_calls()
        self.assertEqual(len(stats), 5)
        snowflake = next(stat for func, stat in iteritems(stats) if func.name == "drawSnowflake")
        self.assertEqual(snowflake["ncalls"], 1116)
        self.assertEqual(snowflake["perc_calls"], "22.48%")


class RecordingListener(MonitoringListener):
    def __init__(self):
        self.data = []

    def metrics_of_type(self, m_type):
        return [
            point for point in self.data
            if m_type in point
        ]

    def monitoring_data(self, data):
        self.data.extend(data)
