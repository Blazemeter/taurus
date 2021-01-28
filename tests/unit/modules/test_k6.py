from os.path import join

import bzt

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

    def exec_and_communicate(self, *args, **kwargs):
        return "v0.30.0", ""

    def test_full(self):
        self.configure({"execution": {
            "concurrency": 5,
            "hold-for": "30",
            "iterations": 50,
            "scenario": {"script": K6_SCRIPT}}})
        self.obj.prepare()
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
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_kpi_file(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertIn(f"--out csv={self.obj.kpi_file}", self.CMD_LINE)

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
