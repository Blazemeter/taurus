import json
import subprocess
import time

from tests import __dir__
from tests.modules.selenium import SeleniumTestCase


class TestNUnitExecutor(SeleniumTestCase):
    @classmethod
    def setUpClass(cls):
        # TODO: build NUnit runner
        subprocess.call([__dir__() + "/../../../dotnet/NUnitRunner/rebuild.sh"])

    def test_single_iteration(self):
        self.obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../../resources/selenium/nunit/assemblies/NUnitSuite.dll"
            }
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        samples = [json.loads(line) for line in open(self.obj.runner.report_file).readlines()]
        statuses = [sample["status"] for sample in samples]
        self.assertEqual(statuses, ["FAILED", "FAILED", "PASSED", "SKIPPED"])

    def test_iterations(self):
        self.obj.execution.merge({
            "iterations": 2,
            "scenario": {
                "script": __dir__() + "/../../resources/selenium/nunit/assemblies/NUnitSuite.dll"
            }
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        samples = [json.loads(line) for line in open(self.obj.runner.report_file).readlines()]
        self.assertEqual(len(samples), 8)

    def test_duration(self):
        self.obj.execution.merge({
            "hold-for": "5s",
            "scenario": {
                "script": __dir__() + "/../../resources/selenium/nunit/assemblies/NUnitSuite.dll"
            }
        })
        self.obj.prepare()
        started_at = time.time()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        ended_at = time.time()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertGreaterEqual(ended_at - started_at, 5)

    def test_selenium_run(self):
        self.obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../../resources/selenium/nunit/assemblies/SeleniumSuite.dll"
            }
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        samples = [json.loads(line) for line in open(self.obj.runner.report_file).readlines()]
        self.assertEqual(len(samples), 2)
        statuses = [sample["status"] for sample in samples]
        self.assertEqual(statuses, ["FAILED", "PASSED"])
