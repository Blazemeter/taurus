from os.path import join

import bzt

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.vegeta import VegetaExecutor, VegetaLogReader
from bzt.utils import EXE_SUFFIX
from tests.unit import BZTestCase, ExecutorTestCase, RESOURCES_DIR, ROOT_LOGGER

from blazemeter.taurus.bzt.modules.vegeta import VegetaLogReader

TOOL_NAME = join(RESOURCES_DIR, "vegeta", "vegeta_mock" + EXE_SUFFIX)
VEGETA_SCRIPT = join(RESOURCES_DIR, "vegeta", "/tmp/vegeta.in")


class TestVegetaExecutor(ExecutorTestCase):
    EXECUTOR = VegetaExecutor
    CMD_LINE = None

    def test_no_bin_file(self):
        assert false

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = " ".join(args)

    def exec_and_communicate(self, *args, **kwargs):
        return "v0.30.0", ""

    def test_full(self):
        self.configure({"execution": {
            "concurrency": 5,
            "hold-for": "30",
            "iterations": 50,
            "scenario": {"script": VEGETA_SCRIPT}}})

        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.get_widget()
        self.obj.vegeta.tool_name = TOOL_NAME
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

    def test_no_bin_file(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": VEGETA_SCRIPT},
                "executor": "vegeta"
            },
        })
        self.assertNotIn(f"-output", self.CMD_LINE)

    def test_concurrency(self):
        self.simple_run({
            "execution": {
                "concurrency": "5",
                "scenario": {"script": VEGETA_SCRIPT},
                "executor": "vegeta"
            },
        })
        self.assertIn("-rate=5", self.CMD_LINE)

    def test_hold_for(self):
        self.simple_run({
            "execution": {
                "hold-for": "30",
                "scenario": {"script": VEGETA_SCRIPT},
                "executor": "vegeta"
            },
        })
        self.assertIn("-duration 30s", self.CMD_LINE)

    def test_script(self):
        self.simple_run({
            "execution": {
                "iterations": "100",
                "scenario": {"script": VEGETA_SCRIPT},
                "executor": "vegeta"
            },
        })
        self.assertIn("-targets=" + VEGETA_SCRIPT, self.CMD_LINE)

    def test_requests(self):
        self.simple_run({
            "execution": {
                "iterations": "100",
                "scenario": "some_scenario",
                "executor": "vegeta"
            },
            "scenarios": {
                "vegeta-test": {
                     "requests": [{
                         "url": "http://localhost:8000",
                         "method": "HEAD",
                         "headers": {"X-Account-ID": 8675309}
                     }]
                }
            }
        })
        self.assertIn("-targets=" + VEGETA_SCRIPT, self.CMD_LINE)


class TestVegetaReader(BZTestCase):
    def test_read(self):
        log_path = join(RESOURCES_DIR, "Vegeta", "vegeta_kpi.csv")
        obj = VegetaLogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        self.assertEqual(len(points), 4)

        for datapoint in points:
            self.assertTrue(datapoint['ts'] > 1500000000)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.SUCCESSES], 2)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.FAILURES], 2)
