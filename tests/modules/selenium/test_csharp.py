import json
import time

from bzt.modules.csharp import NUnitExecutor
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
        self.obj.runner.env.update({"ARTIFACTS_DIR": self.obj.engine.artifacts_dir})

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
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        samples = [json.loads(line) for line in open(self.obj.runner.report_file).readlines()]
        statuses = [sample["status"] for sample in samples]
        self.assertEqual(statuses, ["FAILED", "FAILED", "PASSED", "SKIPPED"])
