import json
import time

from bzt.modules.csharp import NUnitExecutor, XUnitExecutor, Dotnet
from bzt.utils import is_windows
from tests.unit import RESOURCES_DIR
from tests.unit.modules._selenium import SeleniumTestCase


def check_if_installed_mock(obj):
    return True


class TestNUnitExecutor(SeleniumTestCase):
    def setUp(self):
        super(TestNUnitExecutor, self).setUp()
        self.dotnet_check_if_installed = Dotnet.check_if_installed
        Dotnet.check_if_installed = check_if_installed_mock

    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, NUnitExecutor)
        runner_executable = "nunit-runner-mock" + (".bat" if is_windows() else ".sh")

        self.obj.runner.runner_dir = RESOURCES_DIR + "selenium/dotnet/bin/"
        self.obj.runner.runner_executable = RESOURCES_DIR + "selenium/dotnet/bin/" + runner_executable

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

    def tearDown(self):
        Dotnet.check_if_installed = self.dotnet_check_if_installed
        super(TestNUnitExecutor, self).tearDown()


class TestXUnitExecutor(SeleniumTestCase):
    def setUp(self):
        super(TestXUnitExecutor, self).setUp()
        self.dotnet_check_if_installed = Dotnet.check_if_installed
        Dotnet.check_if_installed = check_if_installed_mock

    def setup_mock(self):
        self.assertIsInstance(self.obj.runner, XUnitExecutor)
        runner_executable = "xunit-runner-mock" + (".bat" if is_windows() else ".sh")

        self.obj.runner.runner_dir = RESOURCES_DIR + "selenium/dotnet/bin/"
        self.obj.runner.runner_executable = RESOURCES_DIR + "selenium/dotnet/bin/" + runner_executable

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

    def tearDown(self):
        Dotnet.check_if_installed = self.dotnet_check_if_installed
        super(TestXUnitExecutor, self).tearDown()
