from os.path import join

import bzt

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.k6 import K6, K6Executor, K6LogReader
from bzt.utils import BetterDict, EXE_SUFFIX
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
        self.obj.k6.tool_path = TOOL_NAME
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def simple_run(self, config, settings=None):
        self.configure(config)
        if settings:
            self.obj.settings.merge(settings)

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
        self.assertIn(f"--out csv={self.obj.kpi_file}", self.CMD_LINE)

    def test_no_load_configured_skips_load_flags(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        })
        self.assertNotIn("--vus", self.CMD_LINE)
        self.assertNotIn("--duration", self.CMD_LINE)
        self.assertNotIn("--iterations", self.CMD_LINE)
        self.assertNotIn("--stage", self.CMD_LINE)

    def test_no_load_configured_lets_env_drive_script(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        }, settings={"env": {"DCP_ANON_USERS": "1", "DURATION": "5s", "RAMPUP": "1s"}})
        self.assertNotIn("--vus", self.CMD_LINE)
        self.assertNotIn("--duration", self.CMD_LINE)
        self.assertNotIn("--iterations", self.CMD_LINE)
        self.assertIn("-e DCP_ANON_USERS=1", self.CMD_LINE)

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

    def test_default_tool_path(self):
        self.assertEqual(K6().tool_path, "k6")

    def test_custom_tool_path(self):
        config = BetterDict.from_dict({"path": "/custom/k6"})
        self.assertEqual(K6(config=config).tool_path, "/custom/k6")

    def test_configured_path_used_in_cmdline(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        }, settings={"path": "/custom/k6"})
        self.assertTrue(self.CMD_LINE.startswith("/custom/k6 run"))

    def test_env_settings(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        }, settings={"env": {"BASE_URL": "http://x"}})
        self.assertIn("-e BASE_URL=http://x", self.CMD_LINE)

    def test_outputs_settings(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        }, settings={"outputs": ["xk6-output-influxdb=http://localhost:8086"]})
        self.assertIn(f"--out csv={self.obj.kpi_file}", self.CMD_LINE)
        self.assertIn("--out xk6-output-influxdb=http://localhost:8086", self.CMD_LINE)
        csv_pos = self.CMD_LINE.index(f"--out csv={self.obj.kpi_file}")
        influx_pos = self.CMD_LINE.index("--out xk6-output-influxdb=http://localhost:8086")
        self.assertLess(csv_pos, influx_pos)

    def test_outputs_and_env_before_trailing_cmdline_and_script(self):
        self.simple_run({
            "execution": {
                "scenario": {"script": K6_SCRIPT},
                "executor": "k6"
            },
        }, settings={
            "outputs": ["xk6-output-influxdb=http://localhost:8086"],
            "env": {"BASE_URL": "http://x"},
            "cmdline": "--verbose",
        })
        outputs_pos = self.CMD_LINE.index("--out xk6-output-influxdb=http://localhost:8086")
        env_pos = self.CMD_LINE.index("-e BASE_URL=http://x")
        cmdline_pos = self.CMD_LINE.index("--verbose")
        script_pos = self.CMD_LINE.index(K6_SCRIPT)
        self.assertLess(outputs_pos, cmdline_pos)
        self.assertLess(env_pos, cmdline_pos)
        self.assertLess(cmdline_pos, script_pos)


class TestK6Reader(BZTestCase):
    def test_read(self):
        log_path = join(RESOURCES_DIR, "k6", "k6_kpi.csv")
        obj = K6LogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        for datapoint in points:
            self.assertTrue(datapoint['ts'] > 1500000000)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.SUCCESSES], 3)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.FAILURES], 3)

    def test_checks_failure_becomes_own_label(self):
        log_path = join(RESOURCES_DIR, "k6", "k6_kpi.csv")
        obj = K6LogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        # k6's `checks` metric carries no request-level `name` tag, so a failed check
        # can't be attributed to the http_reqs sample it came from - it surfaces as its
        # own label instead (see K6LogReader._read's trailing checks-scan loop).
        check_label = "check: status is 200"
        cumulative = points[-1][DataPoint.CUMULATIVE]
        self.assertIn(check_label, cumulative)
        self.assertEqual(cumulative[check_label][KPISet.FAILURES], 1)
        self.assertEqual(cumulative[check_label][KPISet.SUCCESSES], 0)
        errors = cumulative[check_label][KPISet.ERRORS]
        self.assertTrue(any("Check failed: status is 200" in item["msg"] for item in errors))

        # the http_reqs sample itself succeeded at the HTTP layer and is unaffected
        http_label = "https://blazedemo.com/checks-fail"
        self.assertEqual(cumulative[http_label][KPISet.SUCCESSES], 1)
        self.assertEqual(cumulative[http_label][KPISet.FAILURES], 0)
