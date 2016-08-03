import shutil

from bzt.modules.monitoring import MonitoringListener
from bzt.modules.chrome import ChromeProfiler, MetricExtractor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestChromeProfiler(BZTestCase):
    def test_metric_extraction(self):
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

    def test_load_metrics(self):
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

        page_load = listener.metrics_of_type(MetricExtractor.METRIC_PAGE_LOAD_TIME)
        self.assertEqual(len(page_load), 1)
        self.assertAlmostEqual(page_load[0][MetricExtractor.METRIC_PAGE_LOAD_TIME], 6.68, delta=0.01)

        full_load = listener.metrics_of_type(MetricExtractor.METRIC_FULL_LOAD_TIME)
        self.assertEqual(len(full_load), 1)
        self.assertAlmostEqual(full_load[0][MetricExtractor.METRIC_FULL_LOAD_TIME], 8.25, delta=0.01)

        first_paint = listener.metrics_of_type(MetricExtractor.METRIC_FIRST_PAINT_TIME)
        self.assertEqual(len(first_paint), 1)
        self.assertAlmostEqual(first_paint[0][MetricExtractor.METRIC_FIRST_PAINT_TIME], 2.83, delta=0.01)

    def test_dom_metrics(self):
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

        dom_docs = listener.metrics_of_type(MetricExtractor.METRIC_DOM_DOCUMENTS)
        self.assertEqual(len(dom_docs), 16)
        self.assertEqual(dom_docs[0][MetricExtractor.METRIC_DOM_DOCUMENTS], 1)
        self.assertEqual(dom_docs[-1][MetricExtractor.METRIC_DOM_DOCUMENTS], 15)

        dom_nodes = listener.metrics_of_type(MetricExtractor.METRIC_DOM_NODES)
        self.assertEqual(len(dom_nodes), 16)
        self.assertEqual(dom_nodes[0][MetricExtractor.METRIC_DOM_NODES], 4)
        self.assertEqual(dom_nodes[-1][MetricExtractor.METRIC_DOM_NODES], 1989)

    def test_js_metrics(self):
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

        listeners = listener.metrics_of_type(MetricExtractor.METRIC_JS_EVENT_LISTENERS)
        self.assertEqual(len(listeners), 16)
        self.assertEqual(listeners[0][MetricExtractor.METRIC_JS_EVENT_LISTENERS], 0)
        self.assertEqual(listeners[-1][MetricExtractor.METRIC_JS_EVENT_LISTENERS], 470)

        gc_time = listener.metrics_of_type(MetricExtractor.METRIC_JS_GC_TIME)
        self.assertEqual(len(gc_time), 1)
        self.assertAlmostEqual(gc_time[0][MetricExtractor.METRIC_JS_GC_TIME], 0.0464, delta=0.0001)

        heap_size = listener.metrics_of_type(MetricExtractor.METRIC_JS_HEAP_SIZE)
        self.assertEqual(len(heap_size), 16)
        self.assertAlmostEqual(heap_size[0][MetricExtractor.METRIC_JS_HEAP_SIZE], 0.89, delta=0.1)
        self.assertAlmostEqual(heap_size[-1][MetricExtractor.METRIC_JS_HEAP_SIZE], 50.76, delta=0.1)

    def test_network_metrics(self):
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

        footprint = listener.metrics_of_type(MetricExtractor.METRIC_NETWORK_FOOTPRINT)
        self.assertEqual(len(footprint), 1)
        self.assertAlmostEqual(footprint[0][MetricExtractor.METRIC_NETWORK_FOOTPRINT], 2.952, delta=0.001)

        ttfb = listener.metrics_of_type(MetricExtractor.METRIC_NETWORK_TTFB)
        self.assertEqual(len(ttfb), 1)
        self.assertAlmostEqual(ttfb[0][MetricExtractor.METRIC_NETWORK_TTFB], 0.010, delta=0.001)

        reqs_count = listener.metrics_of_type(MetricExtractor.METRIC_NETWORK_REQUESTS)
        self.assertEqual(len(reqs_count), 1)
        self.assertEqual(reqs_count[0][MetricExtractor.METRIC_NETWORK_REQUESTS], 202)

        xhr_reqs = listener.metrics_of_type(MetricExtractor.METRIC_NETWORK_XHR_REQUESTS)
        self.assertEqual(len(xhr_reqs), 1)
        self.assertEqual(xhr_reqs[0][MetricExtractor.METRIC_NETWORK_XHR_REQUESTS], 21)


    def test_memory_metrics(self):
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

        browser = listener.metrics_of_type(MetricExtractor.METRIC_MEMORY_BROWSER)
        per_tab = listener.metrics_of_type(MetricExtractor.METRIC_MEMORY_TAB)

        self.assertEqual(len(browser), 1)
        self.assertAlmostEqual(browser[0][MetricExtractor.METRIC_MEMORY_BROWSER], 97.25, delta=0.1)

        self.assertEqual(len(per_tab), 1)
        self.assertAlmostEqual(per_tab[0][MetricExtractor.METRIC_MEMORY_TAB], 97.25, delta=0.1)


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
