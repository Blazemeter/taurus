import json
import os
import shutil
import time
from os.path import join, exists, dirname

import bzt

from bzt.modules.javascript import WebdriverIOExecutor, JavaScriptExecutor, NewmanExecutor, Mocha, JSSeleniumWebdriver
from bzt.utils import get_full_path, is_windows

from tests import BUILD_DIR, RESOURCES_DIR, BZTestCase
from tests.mocks import EngineEmul
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumMochaRunner(SeleniumTestCase):
    RUNNER_STUB = RESOURCES_DIR + "selenium/js-mocha/mocha" + (".bat" if is_windows() else ".sh")

    def test_selenium_prepare_mocha(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.obj.prepare()

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

        self.obj.prepare()

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
        mocha_link = get_full_path(RESOURCES_DIR + "selenium/mocha-3.1.0.tgz")
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

    def test_prepare(self):
        self.obj.execution.merge({
            "runner": "wdio",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/js-wdio/wdio.conf.js"
            }
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, WebdriverIOExecutor)

    def full_run(self, config):
        self.configure(config)

        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaScriptExecutor)

        self.obj.runner.get_launch_cmdline = lambda *args: [TestWebdriverIOExecutor.RUNNER_STUB] + list(args)

        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()

    def test_simple(self):
        self.full_run({
            'execution': {
                "runner": "wdio",
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/js-wdio/wdio.conf.js",
                }
            },
        })
        self.assertTrue(exists(self.obj.runner.report_file))
        with open(self.obj.runner.report_file) as fds:
            lines = fds.readlines()
        self.assertEqual(len(lines), 1)

    def test_hold(self):
        self.full_run({
            'execution': {
                'hold-for': '5s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-wdio/wdio.conf.js'},
                'runner': 'wdio',
            },
        })

        with open(self.obj.runner.stdout.name) as fds:
            stdout = fds.read()
        self.assertIn("--hold-for 5", stdout)

    def test_iterations(self):
        self.full_run({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-wdio/wdio.conf.js'},
                'runner': 'wdio'
            },
        })

        with open(self.obj.runner.stdout.name) as fds:
            stdout = fds.read()
        self.assertIn("--iterations 3", stdout)


class TestNewmanExecutor(BZTestCase):
    RUNNER_STUB = RESOURCES_DIR + "newman/newman" + (".bat" if is_windows() else ".sh")

    def full_run(self, config):
        self.obj = NewmanExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.config.merge(config)
        execution = config["execution"][0] if isinstance(config["execution"], list) else config["execution"]
        self.obj.execution.merge(execution)
        self.obj.prepare()

        self.obj.get_launch_cmdline = lambda *args: [TestNewmanExecutor.RUNNER_STUB] + list(args)

        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()

    def test_flow(self):
        self.full_run({"execution": {"scenario": {
            "script": RESOURCES_DIR + 'functional/postman.json',
            "globals": {"a": 123},
        }}})
        self.assertTrue(os.path.exists(self.obj.report_file))
        with open(self.obj.report_file) as fds:
            samples = [json.loads(line) for line in fds.readlines()]
        self.assertEqual(1, len(samples))
        sample = samples[0]
        self.assertEqual(sample["status"], "PASSED")
        self.assertEqual(sample["test_case"], "should load")
