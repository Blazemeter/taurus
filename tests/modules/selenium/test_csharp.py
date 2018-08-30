import json
import time

from bzt.modules.dotnet import NUnitExecutor, XUnitExecutor
from bzt.utils import is_windows
from tests import RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


RUNNER_EXECUTABLE = "runner-mock" + (".bat" if is_windows() else ".sh")


class TestNUnitExecutor(SeleniumTestCase):
    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, NUnitExecutor)
        self.obj.runner.mono.tool_path = None
        self.obj.runner.runner_dir = RESOURCES_DIR + "selenium/nunit/bin/"
        self.obj.runner.runner_executable = RESOURCES_DIR + "selenium/nunit/bin/" + RUNNER_EXECUTABLE

    def test_startup(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/nunit/assemblies/NUnitSuite.dll"
            }
        })
        self.obj.prepare()
        self.setup_mock()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        samples = [json.loads(line) for line in open(self.obj.runner.report_file).readlines()]
        statuses = [sample["status"] for sample in samples]
        self.assertEqual(statuses, ["FAILED", "FAILED", "PASSED", "SKIPPED"])


class TestXUnitExecutor(SeleniumTestCase):
    def test_startup(self):
        self.obj.execution.merge({
            "runner": "xunit",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/xunit/XUnitExample.dll"
            }
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, XUnitExecutor)
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        with open(self.obj.runner.report_file) as fds:
            samples = [json.loads(line) for line in fds.readlines()]
        statuses = [sample["status"] for sample in samples]
        self.assertEqual(statuses, ["PASSED", "FAILED"])

    def test_iterations(self):
        self.obj.execution.merge({
            "runner": "xunit",
            "iterations": 2,
            "scenario": {
                "script": RESOURCES_DIR + "selenium/xunit/XUnitExample.dll"
            }
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, XUnitExecutor)
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        with open(self.obj.runner.report_file) as fds:
            samples = [json.loads(line) for line in fds.readlines()]
        statuses = [sample["status"] for sample in samples]
        self.assertEqual(statuses, ["PASSED", "FAILED", "PASSED", "FAILED"])

    def test_hold_for(self):
        self.obj.execution.merge({
            "runner": "xunit",
            "hold-for": "5s",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/xunit/XUnitExample.dll"
            }
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, XUnitExecutor)
        start_time = time.time()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        end_time = time.time()
        self.obj.post_process()
        self.assertGreaterEqual(end_time - start_time, 5)
