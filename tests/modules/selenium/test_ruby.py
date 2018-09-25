import json
import os
import time

from bzt.utils import is_windows
from tests import RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumRSpecRunner(SeleniumTestCase):
    def test_selenium_prepare_rspec(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/ruby/example_spec.rb"
                }}})
        self.obj.prepare()

    def test_rspec_full(self):
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 3)
        first, second, third = lines
        self.assertEqual(json.loads(first)["status"], "PASSED")
        self.assertEqual(json.loads(second)["status"], "FAILED")
        self.assertEqual(json.loads(third)["status"], "FAILED")

    def test_rspec_hold(self):
        self.configure({
            'execution': {
                'hold-for': '10s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.report_file))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 10)

    def test_rspec_iterations(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 9)

    def test_interpreter(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        dummy = RESOURCES_DIR + 'selenium/ruby/ruby-dummy'
        dummy += '.bat' if is_windows() else ''
        self.obj.settings.merge({"interpreter": dummy})
        self.obj.prepare()

    def test_rspec_report_extras(self):
        # NOTE: cloud functional tests rely on sample['extras'] being a dict
        # this test verifies it
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 3)
        for line in lines:
            sample = json.loads(line)
            self.assertIsNotNone(sample['extras'])
            self.assertIsInstance(sample['extras'], dict)
