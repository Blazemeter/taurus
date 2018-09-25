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

    def full_run(self, config):
        self.configure(config)
        dummy = RESOURCES_DIR + 'selenium/ruby/ruby' + ('.bat' if is_windows() else '.sh')
        self.obj.prepare()
        self.obj.runner.settings.merge({"interpreter": dummy})
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()

    def test_rspec_full(self):
        script = RESOURCES_DIR + 'selenium/ruby/example_spec.rb'
        self.full_run({
            'execution': {
                'scenario': {'script': script},
            },
        })
        self.assertTrue(os.path.exists(self.obj.runner.report_file))
        with open(self.obj.runner.stdout_file) as fds:
            stdout = fds.read()
        self.assertIn('--test-suite ' + script, stdout)

    def test_rspec_hold(self):
        self.full_run({
            'execution': {
                'hold-for': '10s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        with open(self.obj.runner.stdout_file) as fds:
            stdout = fds.read()
        self.assertIn('--hold-for 10', stdout)

    def test_rspec_iterations(self):
        self.full_run({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        with open(self.obj.runner.stdout_file) as fds:
            stdout = fds.read()
        self.assertIn('--iterations 3', stdout)

    def test_interpreter(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
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
