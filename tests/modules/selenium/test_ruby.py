import os
import time

from bzt.utils import is_windows
from bzt.modules.selenium import SeleniumExecutor
from tests import RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumRSpecRunner(SeleniumTestCase):
    def prepare_runner(self, config):
        self.configure(config)
        super(SeleniumExecutor, self.obj).prepare()
        self.obj.install_required_tools()
        for driver in self.obj.webdrivers:
            self.obj.env.add_path({"PATH": driver.get_driver_dir()})
        self.obj.create_runner()

        dummy = RESOURCES_DIR + 'selenium/ruby/ruby' + ('.bat' if is_windows() else '.sh')
        self.obj.runner.settings.merge({"interpreter": dummy})
        self.obj.runner.settings.merge({"path": dummy})
        self.obj.runner.prepare()
        self.obj.script = self.obj.runner.script

        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()

    def test_rspec_full(self):
        self.prepare_runner({
            'execution': {
                'hold-for': '10s',
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
            },
        })
        with open(self.obj.runner.stdout.name) as fds:
            stdout = fds.read()
        self.assertIn('--hold-for 10', stdout)
        self.assertIn('--iterations 3', stdout)

    def test_selenium_prepare_rspec(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/ruby/example_spec.rb"
                }}})

        dummy = RESOURCES_DIR + 'selenium/ruby/ruby' + ('.bat' if is_windows() else '.sh')

        self.obj.settings.merge({"interpreter": dummy})
        self.obj.settings.merge({"path": dummy})
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))

        self.obj.prepare()
        self.assertEqual(self.obj.script, os.path.normpath(RESOURCES_DIR + "selenium/ruby/example_spec.rb"))
