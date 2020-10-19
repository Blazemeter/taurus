import json
import time

from bzt.modules.csharp import NUnitExecutor, XUnitExecutor
from tests import RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


class TestNUnitExecutor(SeleniumTestCase):
    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, NUnitExecutor)

        if self.obj.runner.dotnet:
            self.obj.runner.dotnet.tool_path = None

    def test_startup(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/dotnet/NUnitTests.dll"
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
        self.assertEqual(statuses, ["PASSED"])


class TestXUnitExecutor(SeleniumTestCase):
    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, XUnitExecutor)

        if self.obj.runner.dotnet:
            self.obj.runner.dotnet.tool_path = None

    def test_startup(self):
        self.obj.execution.merge({
            "runner": "xunit",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/dotnet/XUnitTests.dll"
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
        self.assertEqual(statuses, ["PASSED"])
