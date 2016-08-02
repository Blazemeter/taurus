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

        load_metrics = listener.metrics_of_type(MetricExtractor.METRIC_PAGE_LOAD_TIME)
        self.assertEqual(len(load_metrics), 1)
        self.assertEqual(load_metrics[0][MetricExtractor.METRIC_PAGE_LOAD_TIME], 0.3)

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
        self.assertEqual(len(dom_docs), 22)
        self.assertEqual(dom_docs[0][MetricExtractor.METRIC_DOM_DOCUMENTS], 1)
        self.assertEqual(dom_docs[-1][MetricExtractor.METRIC_DOM_DOCUMENTS], 2)

        dom_docs = listener.metrics_of_type(MetricExtractor.METRIC_DOM_NODES)
        self.assertEqual(len(dom_docs), 22)
        self.assertEqual(dom_docs[0][MetricExtractor.METRIC_DOM_NODES], 4)
        self.assertEqual(dom_docs[-1][MetricExtractor.METRIC_DOM_NODES], 121)

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
        self.assertEqual(len(listeners), 5)
        self.assertEqual(listeners[0][MetricExtractor.METRIC_JS_EVENT_LISTENERS], 2)
        self.assertEqual(listeners[-1][MetricExtractor.METRIC_JS_EVENT_LISTENERS], 6)

        gc_time = listener.metrics_of_type(MetricExtractor.METRIC_JS_GC_TIME)
        self.assertEqual(len(gc_time), 1)
        self.assertEqual(gc_time[0][MetricExtractor.METRIC_JS_GC_TIME], 0.0)

        heap_size = listener.metrics_of_type(MetricExtractor.METRIC_JS_HEAP_SIZE)
        self.assertEqual(len(heap_size), 5)
        self.assertAlmostEqual(heap_size[0][MetricExtractor.METRIC_JS_HEAP_SIZE], 2, delta=0.1)
        self.assertAlmostEqual(heap_size[-1][MetricExtractor.METRIC_JS_HEAP_SIZE], 2.8, delta=0.1)

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
        self.assertAlmostEqual(footprint[0][MetricExtractor.METRIC_NETWORK_FOOTPRINT], 0.012, delta=0.001)

        ttfb = listener.metrics_of_type(MetricExtractor.METRIC_NETWORK_TTFB)
        self.assertEqual(len(ttfb), 1)
        self.assertAlmostEqual(ttfb[0][MetricExtractor.METRIC_NETWORK_TTFB], 0.0)

        reqs_count = listener.metrics_of_type(MetricExtractor.METRIC_NETWORK_REQUESTS)
        self.assertEqual(len(reqs_count), 1)
        self.assertEqual(reqs_count[0][MetricExtractor.METRIC_NETWORK_REQUESTS], 6)

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
        self.assertAlmostEqual(browser[0][MetricExtractor.METRIC_MEMORY_BROWSER], 96.8, delta=0.1)

        self.assertEqual(len(per_tab), 1)
        self.assertAlmostEqual(per_tab[0][MetricExtractor.METRIC_MEMORY_TAB], 96.8, delta=0.1)


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
