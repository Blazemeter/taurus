import logging
import shutil
import time
import unittest

from bzt.modules.chrome import ChromeProfiler, CPUProfileProcessor, MetricReporter
from bzt.modules.monitoring import MonitoringListener
from bzt.six import iteritems
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, RecordingHandler
from tests.modules.test_SeleniumExecutor import SeleniumTestCase


class TestMetricExtraction(BZTestCase):
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

        self.assertAlmostEqual(metrics["time-load-time"], 5.27, delta=0.01)
        self.assertAlmostEqual(metrics["time-dom-content-load-time"], 3.00, delta=0.01)
        self.assertAlmostEqual(metrics["time-first-paint-time"], 2.83, delta=0.01)
        self.assertAlmostEqual(metrics["time-full-load-time"], 8.25, delta=0.01)
        self.assertAlmostEqual(metrics["network-footprint"], 2.952, delta=0.001)
        self.assertAlmostEqual(metrics["network-time-to-first-byte"], 1.45, delta=0.001)
        self.assertEqual(metrics["network-http-requests"], 200)
        self.assertEqual(metrics["network-xhr-requests"], 21)
        self.assertAlmostEqual(metrics["js-total-gc-time"], 0.0464, delta=0.0001)
        self.assertAlmostEqual(metrics["js-average-heap"], 34.8, delta=0.1)
        self.assertEqual(metrics["dom-final-documents"], 15)
        self.assertEqual(metrics["dom-final-nodes"], 1989)
        self.assertEqual(metrics["dom-final-event-listeners"], 469)

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

        dom_docs = listener.metrics_of_type("dom-documents")
        self.assertEqual(len(dom_docs), 16)
        self.assertEqual(dom_docs[0]["dom-documents"], 1)
        self.assertEqual(dom_docs[-1]["dom-documents"], 15)

        dom_nodes = listener.metrics_of_type("dom-nodes")
        self.assertEqual(len(dom_nodes), 16)
        self.assertEqual(dom_nodes[0]["dom-nodes"], 4)
        self.assertEqual(dom_nodes[-1]["dom-nodes"], 1989)

        listeners = listener.metrics_of_type("dom-event-listeners")
        self.assertEqual(len(listeners), 16)
        self.assertEqual(listeners[0]["dom-event-listeners"], 0)
        self.assertEqual(listeners[-1]["dom-event-listeners"], 469)

        browser = listener.metrics_of_type("memory-browser")
        self.assertEqual(len(browser), 1)
        self.assertAlmostEqual(browser[0]["memory-browser"], 97.25, delta=0.1)

        per_tab = listener.metrics_of_type("memory-tab")
        self.assertEqual(len(per_tab), 1)
        self.assertAlmostEqual(per_tab[0]["memory-tab"], 97.25, delta=0.1)

        heap_size = listener.metrics_of_type("js-heap-usage")
        self.assertEqual(len(heap_size), 16)
        self.assertAlmostEqual(heap_size[0]["js-heap-usage"], 0.89, delta=0.1)
        self.assertAlmostEqual(heap_size[-1]["js-heap-usage"], 50.76, delta=0.1)

        js_cpu = listener.metrics_of_type("js-cpu-usage")
        self.assertEqual(len(js_cpu), 15)
        self.assertAlmostEqual(js_cpu[0]["js-cpu-usage"], 26.0, delta=0.1)
        self.assertAlmostEqual(js_cpu[1]["js-cpu-usage"], 98.4, delta=0.1)
        self.assertAlmostEqual(js_cpu[2]["js-cpu-usage"], 132.4, delta=0.1)
        self.assertAlmostEqual(js_cpu[-1]["js-cpu-usage"], 1.6, delta=0.1)


class TestCPUPRofileReader(BZTestCase):
    def test_cpuprofile_stats(self):
        obj = CPUProfileProcessor(__dir__() + "/../chrome/js.cpuprofile", logging.getLogger())
        obj.process_file()
        stats = obj.extract_js_call_stats()
        self.assertEqual(len(stats), 29)
        snowflake = next(stat for func, stat in iteritems(stats) if func.name == "drawSnowflake")
        self.assertEqual(snowflake["ncalls"], 1116)
        self.assertEqual(snowflake["perc_calls"], "22.48%")
        self.assertAlmostEqual(snowflake["total_time"], 1.84, delta=0.01)
        self.assertAlmostEqual(snowflake["self_time"], 1.20, delta=0.01)
        self.assertEqual(snowflake["perc_self"], "65.29%")


class TestMetricReporter(BZTestCase):
    def test_metrics_reporting(self):
        engine = EngineEmul()
        profiler = ChromeProfiler()
        profiler.engine = engine
        profiler.parameters.merge({
            "processors": [{
                "class": "bzt.modules.chrome.TraceProcessor",
                "file": "trace.json",
            }, {
                "file": "js.cpuprofile",
                "class": "bzt.modules.chrome.CPUProfileProcessor",
            }],
        })
        shutil.copy(__dir__() + "/../chrome/trace.json", engine.artifacts_dir)
        shutil.copy(__dir__() + "/../chrome/js.cpuprofile", engine.artifacts_dir)

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

        self.assertIn("Page load times:", info_buff)
        self.assertIn("Network metrics:", info_buff)
        self.assertIn("JavaScript metrics:", info_buff)
        self.assertIn("HTTP requests:", info_buff)
        self.assertIn("AJAX requests:", info_buff)

        self.assertIn("JavaScript function calls", info_buff)
        for fun in ("drawSnowflake", "getSegmentAngle", "submitSnowflakeResults"):
            self.assertIn(fun, info_buff)

        profiler.log.removeHandler(log_recorder)


class TestChromeProfiler(SeleniumTestCase):
    @unittest.skip("CI has no chromedriver, pity")
    def test_full(self):
        self.obj.engine.config.merge({
            "execution": {
                "executor": "selenium",
                "iterations": 3,
                "scenario": {"script": __dir__() + "/../chrome/test_trace.py"}
            },
        })
        self.obj.execution = self.obj.engine.config['execution']
        profiler = ChromeProfiler()
        profiler.engine = self.engine_obj
        profiler.parameters.merge({
            "processors": [{
                "class": "bzt.modules.chrome.TraceProcessor",
                "file": "trace.json",
            }, {
                "file": "js.cpuprofile",
                "class": "bzt.modules.chrome.CPUProfileProcessor",
            }],
        })

        profiler.prepare()
        self.obj.prepare()
        profiler.startup()
        self.obj.startup()
        while not self.obj.check():
            profiler.check()
            time.sleep(1)
        self.obj.shutdown()
        profiler.shutdown()
        profiler.post_process()

        self.assertIsNotNone(profiler.get_tab_label())
        self.assertIsNotNone(profiler.get_aggr_metrics())


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
