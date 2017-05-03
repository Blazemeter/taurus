import shutil
import time

from bzt.modules.chrome import ChromeProfiler, MetricReporter
from bzt.modules.monitoring import MonitoringListener
from bzt.six import iteritems
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, RecordingHandler


class TestMetricExtraction(BZTestCase):
    def test_extraction(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "processors": {
                "trace": {
                    "class": "bzt.modules.chrome.TraceProcessor",
                    "extractors": ["bzt.modules.chrome.MemoryMetricsExtractor"]
                }
            }
        })
        listener = RecordingListener()
        obj.add_listener(listener)

        shutil.copy(__dir__() + "/../resources/chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_reread(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "processors": {
                "trace": {
                    "class": "bzt.modules.chrome.TraceProcessor",
                    "extractors": ["bzt.modules.chrome.MemoryMetricsExtractor"]
                }
            }
        })
        listener = RecordingListener()
        obj.add_listener(listener)

        shutil.copy(__dir__() + "/../resources/chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        for _ in range(3):
            obj.check()
            time.sleep(1)

        shutil.copy(__dir__() + "/../resources/chrome/trace.json", obj.engine.artifacts_dir)
        for _ in range(3):
            obj.check()
            time.sleep(1)

        obj.shutdown()
        obj.post_process()

    def test_aggr_metrics(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "processors": {
                "trace": {
                    "class": "bzt.modules.chrome.TraceProcessor",
                    "extractors": ["bzt.modules.chrome.MemoryMetricsExtractor"]
                }
            }
        })

        shutil.copy(__dir__() + "/../resources/chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        obj.check()

        metrics = obj.get_aggr_metrics()

        for metric, _ in iteritems(metrics):
            self.assertIsNotNone(obj.get_metric_label(metric))

        self.assertAlmostEqual(metrics["memory-average-tab"], 97.25, delta=0.01)
        self.assertAlmostEqual(metrics["memory-average-browser"], 97.25, delta=0.01)

    def test_calc_metrics(self):
        obj = ChromeProfiler()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "processors": {
                "trace": {
                    "class": "bzt.modules.chrome.TraceProcessor",
                    "extractors": ["bzt.modules.chrome.MemoryMetricsExtractor"]
                }
            }
        })
        listener = RecordingListener()
        obj.add_listener(listener)

        shutil.copy(__dir__() + "/../resources/chrome/trace.json", obj.engine.artifacts_dir)

        obj.prepare()
        obj.startup()
        obj.check()

        browser = listener.metrics_of_type("memory-browser")
        self.assertEqual(len(browser), 1)
        self.assertAlmostEqual(browser[0]["memory-browser"], 97.25, delta=0.1)

        per_tab = listener.metrics_of_type("memory-tab")
        self.assertEqual(len(per_tab), 1)
        self.assertAlmostEqual(per_tab[0]["memory-tab"], 97.25, delta=0.1)


class TestMetricReporter(BZTestCase):
    def test_metrics_reporting(self):
        engine = EngineEmul()
        profiler = ChromeProfiler()
        profiler.engine = engine
        profiler.settings.merge({
            "processors": {
                "trace": {
                    "class": "bzt.modules.chrome.TraceProcessor",
                    "extractors": [
                        "bzt.modules.chrome.TabNameExtractor",
                        "bzt.modules.chrome.MemoryMetricsExtractor",
                    ]
                }
            }
        })
        shutil.copy(__dir__() + "/../resources/chrome/trace.json", engine.artifacts_dir)

        log_recorder = RecordingHandler()
        reporter = MetricReporter()
        reporter.log.addHandler(log_recorder)
        reporter.engine = engine

        engine.services.append(profiler)
        engine.reporters.append(reporter)

        reporter.prepare()
        profiler.prepare()
        reporter.startup()
        profiler.startup()
        profiler.check()
        reporter.check()

        reporter.shutdown()
        reporter.post_process()

        info_buff = log_recorder.info_buff.getvalue()

        self.assertIn("Chrome metrics for tab 'JMeter and Performance Testing for DevOps I BlazeMeter'", info_buff)
        self.assertIn("Memory metrics:", info_buff)

        profiler.log.removeHandler(log_recorder)


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
