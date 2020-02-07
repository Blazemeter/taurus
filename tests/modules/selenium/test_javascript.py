import json
import os
import shutil
import time
from os.path import join, exists, dirname

import bzt

from bzt.modules.javascript import WebdriverIOExecutor, JavaScriptExecutor, NewmanExecutor, Mocha, JSSeleniumWebdriver
from bzt.modules.selenium import SeleniumExecutor
from bzt.utils import get_full_path, is_windows

from tests import BUILD_DIR, RESOURCES_DIR, BZTestCase
from tests.mocks import EngineEmul
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumMochaRunner(SeleniumTestCase):
    RUNNER_STUB = RESOURCES_DIR + "selenium/js-mocha/mocha" + (".bat" if is_windows() else ".sh")

    @staticmethod
    def check_mocha_cmd(runner):
        check_mocha = [runner.node.tool_path, "-e", "require('mocha'); console.log('mocha is installed');"]
        return check_mocha

    @staticmethod
    def install_mocha_cmd(runner):
        package_name = runner.mocha.package_name
        if runner.mocha.version:
            package_name += "@" + runner.mocha.version
        install_mocha = [runner.npm.tool_path, "install", package_name, "--prefix", runner.mocha.tools_dir]
        return install_mocha

    def test_mocha_not_found(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.func_results = "not found", None
        call_back = bzt.utils.RequiredTool.call
        bzt.utils.RequiredTool.call = self.func_mock
        try:
            self.obj.prepare()
        finally:
            bzt.utils.RequiredTool.call = call_back

        self.assertEqual(6, len(self.func_args))

        runner = self.obj.runner
        args = [args["args"][0] for args in self.func_args]

        self.assertIn(self.check_mocha_cmd(runner), args)
        self.assertIn(self.install_mocha_cmd(runner), args)

    def test_mocha_installed(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.func_results = "mocha is installed", None
        call_back = bzt.utils.RequiredTool.call
        bzt.utils.RequiredTool.call = self.func_mock
        try:
            self.obj.prepare()
        finally:
            bzt.utils.RequiredTool.call = call_back

        self.assertEqual(5, len(self.func_args))
        runner = self.obj.runner
        args = [args["args"][0] for args in self.func_args]

        self.assertIn(self.check_mocha_cmd(runner), args)
        self.assertNotIn(self.install_mocha_cmd(runner), args)

    def full_run(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config['execution']

        super(SeleniumExecutor, self.obj).prepare()
        self.obj.install_required_tools()
        for driver in self.obj.webdrivers:
            self.obj.env.add_path({"PATH": driver.get_driver_dir()})

        self.obj.create_runner()
        self.obj.runner.install_required_tools = lambda: None
        self.obj.runner.prepare()
        self.obj.script = self.obj.runner.script
        self.obj.runner.get_launch_cmdline = lambda *args: [TestSeleniumMochaRunner.RUNNER_STUB] + list(args)

        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()

    def test_mocha_full(self):
        self.full_run({
            'execution': {
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
                }
            }
        })
        self.assertTrue(exists(self.obj.runner.report_file))

    def test_mocha_hold(self):
        self.full_run({
            'execution': {
                'hold-for': '5s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-mocha/'},
                'executor': 'selenium'
            },
        })

        with open(self.obj.runner.stdout.name) as fds:
            stdout = fds.read()
        self.assertIn("--hold-for 5", stdout)

    def test_mocha_iterations(self):
        self.full_run({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-mocha'},
                'executor': 'selenium'
            },
        })
        with open(self.obj.runner.stdout.name) as fds:
            stdout = fds.read()
        self.assertIn("--iterations 3", stdout)

    def test_install_mocha(self):
        dummy_installation_path = get_full_path(BUILD_DIR + "selenium-taurus/nodejs")
        mocha_link = get_full_path(RESOURCES_DIR + "selenium/mocha-7.0.0.tgz")
        wd_link = get_full_path(RESOURCES_DIR + "selenium/selenium-webdriver-1.0.0.tgz")

        shutil.rmtree(dirname(dummy_installation_path), ignore_errors=True)
        self.assertFalse(exists(dummy_installation_path))

        old_node_path = os.environ.get("NODE_PATH")
        if old_node_path:
            os.environ.pop("NODE_PATH")

        orig_mocha_package = Mocha.PACKAGE_NAME
        orig_wd_package = JSSeleniumWebdriver.PACKAGE_NAME
        try:
            Mocha.PACKAGE_NAME = mocha_link
            JSSeleniumWebdriver.PACKAGE_NAME = wd_link

            self.obj.engine.config.merge({
                "modules": {
                    "mocha": {
                        "tools-dir": dummy_installation_path}}})

            self.obj.execution.merge({
                "runner": "mocha",
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"}})
            self.obj.prepare()

            self.assertTrue(exists(join(dummy_installation_path, "node_modules")))
            self.assertTrue(exists(join(dummy_installation_path, "node_modules", "mocha")))
            self.assertTrue(exists(join(dummy_installation_path, "node_modules", "mocha", "index.js")))
            self.assertTrue(exists(join(dummy_installation_path, "node_modules", "selenium-webdriver")))
            self.assertTrue(exists(join(dummy_installation_path, "node_modules", "selenium-webdriver", "index.js")))
        finally:
            Mocha.PACKAGE_NAME = orig_mocha_package
            JSSeleniumWebdriver.PACKAGE_NAME = orig_wd_package
            if old_node_path:
                os.environ["NODE_PATH"] = old_node_path


class TestWebdriverIOExecutor(SeleniumTestCase):
    RUNNER_STUB = RESOURCES_DIR + "selenium/js-wdio/wdio" + (".bat" if is_windows() else ".sh")
    CMD_LINE = None

    def start_subprocess(self, args, env, cwd=None, **kwargs):
        self.CMD_LINE = args

    def full_run(self, config):
        self.configure(config)
        self.obj.install_required_tools = lambda: None
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, WebdriverIOExecutor)
        self.obj.engine.start_subprocess = self.start_subprocess
        self.assertIsInstance(self.obj.runner, JavaScriptExecutor)
        self.obj.startup()
        self.obj.shutdown()

    def test_simple(self):
        self.full_run({
            'execution': {
                'hold-for': '5s',
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-wdio/wdio.conf.js'},
                'runner': 'wdio',
            },
        })

        self.assertTrue('--hold-for' in self.CMD_LINE)
        hold_val = self.CMD_LINE[self.CMD_LINE.index('--hold-for')+1]
        self.assertEqual(hold_val, '5.0')

        self.assertTrue('--iterations' in self.CMD_LINE)
        iters_val = self.CMD_LINE[self.CMD_LINE.index('--iterations')+1]
        self.assertEqual(iters_val, '3')


class TestNewmanExecutor(BZTestCase):
    RUNNER_STUB = RESOURCES_DIR + "newman/newman" + (".bat" if is_windows() else ".sh")

    def full_run(self, config):
        self.obj = NewmanExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.config.merge(config)
        execution = config["execution"][0] if isinstance(config["execution"], list) else config["execution"]
        self.obj.execution.merge(execution)

    def test_flow(self):
        self.full_run({"execution": {"scenario": {
            "script": RESOURCES_DIR + 'functional/postman.json',
            "globals": {"a": 123},
        }}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None

        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()
