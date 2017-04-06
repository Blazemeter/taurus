import json
import time

import os
from tests import __dir__

from bzt.utils import is_windows
from tests.modules.test_SeleniumExecutor import SeleniumTestCase


class TestSeleniumRSpecRunner(SeleniumTestCase):
    def test_selenium_prepare_rspec(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": __dir__() + "/../selenium/ruby/example_spec.rb"
                }}})
        self.obj.prepare()

    def test_rspec_full(self):
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        lines = open(self.obj.runner.execution.get("report-file")).readlines()
        self.assertEqual(len(lines), 3)
        first, second, third = lines
        self.assertEqual(json.loads(first)["status"], "PASSED")
        self.assertEqual(json.loads(second)["status"], "FAILED")
        self.assertEqual(json.loads(third)["status"], "FAILED")

    def test_rspec_hold(self):
        self.configure({
            'execution': {
                'hold-for': '10s',
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 10)

    def test_rspec_iterations(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        lines = open(self.obj.runner.execution.get("report-file")).readlines()
        self.assertEqual(len(lines), 9)

    def test_interpreter(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        dummy = __dir__() + '/../selenium/ruby/ruby-dummy'
        dummy += '.bat' if is_windows() else ''
        self.obj.settings.merge({
            "selenium-tools": {"rspec": {"interpreter": dummy}}
        })
        self.obj.prepare()
