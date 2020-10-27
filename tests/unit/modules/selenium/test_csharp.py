import json
import time

from bzt.modules.csharp import NUnitExecutor, XUnitExecutor
from bzt.utils import is_windows
from tests.unit import RESOURCES_DIR
from tests.unit.modules.selenium import SeleniumTestCase


class TestNUnitExecutor(SeleniumTestCase):
    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, NUnitExecutor)
        runner_executable = "nunit-runner-mock" + (".bat" if is_windows() else ".sh")

        self.obj.runner.runner_dir = RESOURCES_DIR + "selenium/dotnet/bin/"
        self.obj.runner.runner_executable = RESOURCES_DIR + "selenium/dotnet/bin/" + runner_executable

        if self.obj.runner.dotnet:
            self.obj.runner.dotnet.tool_path = None

    def test_startup(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/dotnet/assemblies/Empty.dll"
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
        self.assertEqual(statuses, ['FAILED', 'FAILED', 'PASSED', 'SKIPPED'])


class TestXUnitExecutor(SeleniumTestCase):
    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, XUnitExecutor)
        runner_executable = "xunit-runner-mock" + (".bat" if is_windows() else ".sh")

        self.obj.runner.runner_dir = RESOURCES_DIR + "selenium/dotnet/bin/"
        self.obj.runner.runner_executable = RESOURCES_DIR + "selenium/dotnet/bin/" + runner_executable

        if self.obj.runner.dotnet:
            self.obj.runner.dotnet.tool_path = None

    def test_startup(self):
        self.obj.execution.merge({
            "runner": "xunit",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/dotnet/assemblies/Empty.dll"
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
        self.assertEqual(statuses, ['FAILED', 'FAILED', 'PASSED', 'SKIPPED'])
