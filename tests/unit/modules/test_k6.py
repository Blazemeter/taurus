from os.path import join

import bzt
from bzt.engine.names import EXEC

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.k6 import K6Executor, K6LogReader
from bzt.utils import EXE_SUFFIX
from tests.unit import BZTestCase, ExecutorTestCase, RESOURCES_DIR, ROOT_LOGGER

TOOL_NAME = join(RESOURCES_DIR, "k6", "k6_mock" + EXE_SUFFIX)
K6_SCRIPT = join(RESOURCES_DIR, "k6", "k6_script.js")


class TestK6Executor(ExecutorTestCase):
    EXECUTOR = K6Executor
    CMD_LINE = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = " ".join(args)

    def test_full(self):
        self.configure({"execution": {
            "concurrency": 5,
            "hold-for": "30",
            "iterations": 50,
            "scenario": {"script": K6_SCRIPT}}})

        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.get_widget()
        self.obj.k6.tool_name = TOOL_NAME
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def simple_run(self, config):
        self.configure(config)

        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

    def test_kpi_file(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertNotIn(f"--out csv={self.obj.kpi_file}", self.CMD_LINE)

    def test_concurrency(self):
        self.simple_run({
            "execution": {
                "concurrency": "5",
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertIn("--vus 5", self.CMD_LINE)

    def test_hold_for(self):
        self.simple_run({
            "execution": {
                "hold-for": "30",
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertIn("--duration 30s", self.CMD_LINE)

    def test_iterations(self):
        self.simple_run({
            "execution": {
                "iterations": "100",
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertIn("--iterations 100", self.CMD_LINE)

    def test_ramp_up(self):
        self.simple_run({
            "execution": {
                "ramp-up": "60s",
                "hold-for": "180s",
                "concurrency": "10",
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertIn("--stage 60s:10", self.CMD_LINE)
        self.assertIn("--stage 120s:10", self.CMD_LINE)
        self.assertNotIn("--iterations", self.CMD_LINE)
        self.assertNotIn("--duration", self.CMD_LINE)
        self.assertNotIn("--vu", self.CMD_LINE)

    def test_iterations_multiplied(self):
        self.simple_run({
            "execution": {
                "iterations": "10",
                "concurrency": "10",
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertIn("--iterations 100", self.CMD_LINE)

    def test_get_load(self):
        self.configure({EXEC: {
            "ramp-up": "1", "concurrency": "2", "throughput": "3"}})
        self.assertEqual(self.obj.execution.get("ramp-up"), "1")
        self.assertEqual(self.obj.execution.get("concurrency"), "2")
        self.assertEqual(self.obj.execution.get("throughput"), "3")
        load = self.obj.get_load()
        self.assertEqual(load.ramp_up, 1)
        self.assertEqual(load.concurrency, 2)
        self.assertEqual(load.throughput, 3)
        self.assertEqual(load.iterations, 0)

        self.assertEqual(self.obj.execution.get("ramp-up"), "1")
        self.assertEqual(self.obj.execution.get("concurrency"), {"local": "2"})
        self.assertEqual(self.obj.execution.get("throughput"), {"local": "3"})

        self.obj.engine.config.get("execution")[0]["concurrency"] = "22"
        load = self.obj.get_load()
        self.assertEqual(load.concurrency, 22)
        self.assertEqual(self.obj.execution.get("concurrency"), {"local": "22"})

    def test_get_load_defaults(self):
        self.configure({EXEC: {}})
        self.assertFalse(self.obj.execution.get("ramp-up"))
        self.assertFalse(self.obj.execution.get("concurrency"))
        self.assertFalse(self.obj.execution.get("throughput"))
        self.assertFalse(self.obj.execution.get("iterations"))
        load = self.obj.get_load()
        self.assertEqual(load.ramp_up, None)
        self.assertEqual(load.concurrency, 0)
        self.assertEqual(load.throughput, 0)
        self.assertEqual(load.iterations, 0)

    def test_get_load_str(self):
        self.configure({EXEC: {
            "concurrency": "2",
            "hold-for": "3",
            "ramp-up": "4",
            "iterations": "5",
            "throughput": "6",
            "steps": "7",
        }})
        load = self.obj.get_load()
        self.assertEquals(2, load.concurrency)
        self.assertEquals(3, load.hold)
        self.assertEquals(4, load.ramp_up)
        self.assertEquals(5, load.iterations)
        self.assertEquals(6, load.throughput)
        self.assertEquals(7, load.steps)

    def test_get_load_str_fail(self):
        self.configure({EXEC: {
            "concurrency": "2VU",
            "throughput": "1PS",
            "steps": "3ST",
            "iterations": "4IT"
        }})
        self.assertRaises(bzt.TaurusConfigError, self.obj.get_load)

class TestK6Reader(BZTestCase):
    def test_read(self):
        log_path = join(RESOURCES_DIR, "k6", "k6_kpi.csv")
        obj = K6LogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        self.assertEqual(len(points), 4)

        for datapoint in points:
            self.assertTrue(datapoint['ts'] > 1500000000)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.SUCCESSES], 2)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.FAILURES], 2)
