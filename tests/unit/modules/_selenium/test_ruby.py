import json
import os

import bzt
from bzt.utils import EXE_SUFFIX
from tests.unit import RESOURCES_DIR
from tests.unit.modules._selenium import SeleniumTestCase


class TestSeleniumRSpecRunner(SeleniumTestCase):
    CMD_LINE = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = ' '.join(args)

    def full_run(self, config):

        self.configure(config)
        dummy = RESOURCES_DIR + 'selenium/ruby/ruby' + EXE_SUFFIX

        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.runner.settings.merge({"interpreter": dummy})
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

    def test_rspec_full(self):
        self.full_run({
            'execution': {
                'hold-for': '10s',
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
            },
        })
        self.assertIn('--hold-for 10', self.CMD_LINE)
        self.assertIn('--iterations 3', self.CMD_LINE)

    def test_rspec_hold(self):
        self.full_run({
            'execution': {
                'hold-for': '10s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.assertIn('--hold-for 10', self.CMD_LINE)

    def test_rspec_iterations(self):
        self.full_run({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.assertIn('--iterations 3', self.CMD_LINE)

    def test_interpreter(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))

        dummy = RESOURCES_DIR + 'selenium/ruby/ruby' + EXE_SUFFIX
        self.obj.settings.merge({"interpreter": dummy})
        self.obj.settings.merge({"path": dummy})

        self.obj.prepare()

    def test_selenium_prepare_rspec(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/ruby/example_spec.rb"
                }}})

        dummy = RESOURCES_DIR + 'selenium/ruby/ruby' + EXE_SUFFIX

        self.obj.settings.merge({"interpreter": dummy})
        self.obj.settings.merge({"path": dummy})
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))

        self.obj.prepare()
        self.assertEqual(self.obj.script, os.path.normpath(RESOURCES_DIR + "selenium/ruby/example_spec.rb"))
