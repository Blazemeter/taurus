import json
import os
import shutil
import time
from os.path import join, exists, dirname

import bzt
from bzt.modules import javascript
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.javascript import WebdriverIOExecutor, NewmanExecutor
from bzt.utils import get_full_path, shell_exec, is_windows
from tests import BUILD_DIR, RESOURCES_DIR, BZTestCase
from tests.mocks import EngineEmul
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumMochaRunner(SeleniumTestCase):
    def test_selenium_prepare_mocha(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.obj.prepare()

    @staticmethod
    def check_mocha_cmd(runner):
        check_mocha = [runner.node_tool.executable, "-e", "require('mocha'); console.log('mocha is installed');"]
        return check_mocha

    @staticmethod
    def install_mocha_cmd(runner):
        package_name = runner.mocha_tool.package_name
        if runner.mocha_tool.version:
            package_name += "@" + runner.mocha_tool.version
        install_mocha = [runner.npm_tool.executable, "install", package_name, "--prefix", runner.mocha_tool.tools_dir,
                         '--no-save']
        return install_mocha

    def test_mocha_not_found(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.func_results = "not found"
        sync_run_back = bzt.modules.javascript.sync_run
        bzt.modules.javascript.sync_run = self.func_mock
        try:
            self.obj.prepare()
        finally:
            bzt.modules.javascript.sync_run = sync_run_back

        self.assertEqual(5, len(self.func_args))

        runner = self.obj.runner
        args = [args["args"][0] for args in self.func_args]

        self.assertIn(self.check_mocha_cmd(runner), args)
        self.assertIn(self.install_mocha_cmd(runner), args)

    def test_mocha_installed(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.func_results = "mocha is installed"
        sync_run_back = bzt.utils.sync_run
        bzt.modules.javascript.sync_run = self.func_mock
        try:
            self.obj.prepare()
        finally:
            bzt.modules.javascript.sync_run = sync_run_back

        self.assertEqual(4, len(self.func_args))
        runner = self.obj.runner
        args = [args["args"][0] for args in self.func_args]

        self.assertIn(self.check_mocha_cmd(runner), args)
        self.assertNotIn(self.install_mocha_cmd(runner), args)

    def test_mocha_full(self):
        self.obj.engine.config.merge({
            'execution': {
                "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"}})

        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']

        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"}})

        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 3)

    def test_mocha_hold(self):
        self.obj.engine.config.merge({
            'execution': {
                'hold-for': '5s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-mocha/'},
                'executor': 'selenium'
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']

        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/"
        }})

        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 5)

    def test_mocha_iterations(self):
        self.obj.engine.config.merge({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-mocha'},
                'executor': 'selenium'
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']

        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha"
        }})

        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 9)

    def test_install_mocha(self):
        dummy_installation_path = get_full_path(BUILD_DIR + "selenium-taurus/nodejs")
        mocha_link = get_full_path(RESOURCES_DIR + "selenium/mocha-3.1.0.tgz")
        wd_link = get_full_path(RESOURCES_DIR + "selenium/selenium-webdriver-1.0.0.tgz")

        shutil.rmtree(dirname(dummy_installation_path), ignore_errors=True)
        self.assertFalse(exists(dummy_installation_path))

        old_node_path = os.environ.get("NODE_PATH")
        if old_node_path:
            os.environ.pop("NODE_PATH")

        orig_mocha_package = javascript.MOCHA_NPM_PACKAGE_NAME
        orig_wd_package = javascript.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME
        try:
            javascript.MOCHA_NPM_PACKAGE_NAME = mocha_link
            javascript.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = wd_link

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
            javascript.MOCHA_NPM_PACKAGE_NAME = orig_mocha_package
            javascript.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = orig_wd_package
            if old_node_path:
                os.environ["NODE_PATH"] = old_node_path


class TestWebdriverIOExecutor(SeleniumTestCase):
    def test_prepare(self):
        self.obj.execution.merge({
            "runner": "wdio",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
            }
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, WebdriverIOExecutor)

    def run_command(self, cmdline, stream_name, cwd):
        out = open(self.obj.engine.create_artifact(stream_name, ".out"), "wt")
        err = open(self.obj.engine.create_artifact(stream_name, ".err"), "wt")
        process = shell_exec(args=cmdline, stdout=out, stderr=err, cwd=cwd)
        while process.poll() is None:
            time.sleep(0.5)
        out.close()
        err.close()
        self.obj.log.debug("%s out: %s", cmdline, open(out.name, 'r').read())
        self.obj.log.debug("%s err: %s", cmdline, open(err.name, 'r').read())

    def full_run(self, config, script_dir):
        self.configure(config)

        self.run_command(["npm.cmd" if is_windows() else "npm", "install"], "npm-install", script_dir)

        self.obj.prepare()
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
        }, script_dir=RESOURCES_DIR + "selenium/js-wdio")
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 1)

    def test_hold(self):
        self.full_run({
            'execution': {
                'hold-for': '5s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-wdio/wdio.conf.js'},
                'runner': 'wdio',
            },
        }, script_dir=RESOURCES_DIR + "selenium/js-wdio")
        self.assertTrue(exists(self.obj.runner.report_file))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 5)

    def test_iterations(self):
        self.full_run({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-wdio/wdio.conf.js'},
                'runner': 'wdio'
            },
        }, script_dir=RESOURCES_DIR + "selenium/js-wdio")

        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 3)


class TestNewmanExecutor(BZTestCase):
    def test_flow(self):
        obj = NewmanExecutor()
        obj.engine = EngineEmul()
        obj.env = obj.engine.env
        obj.engine.aggregator = ConsolidatingAggregator()
        obj.engine.config.merge({"scenarios": {"newman": {
            "script": RESOURCES_DIR + 'functional/postman.json',
            "globals": {"a": 123},
        }}})
        obj.execution.merge({"scenario": "newman"})
        obj.engine.aggregator.prepare()
        obj.prepare()
        obj.startup()
        obj.engine.aggregator.startup()
        while not obj.check():
            obj.engine.aggregator.check()
            time.sleep(obj.engine.check_interval)
        obj.shutdown()
        obj.engine.aggregator.shutdown()
        obj.post_process()
        obj.engine.aggregator.post_process()
        self.assertTrue(obj.has_results())

    def test_broken(self):
        obj = NewmanExecutor()
        obj.engine = EngineEmul()
        obj.env = obj.engine.env
        obj.engine.aggregator = ConsolidatingAggregator()
        obj.engine.config.merge({"scenarios": {"newman": {
            "script": RESOURCES_DIR + 'functional/postman.json',
            "globals": {"a": 123},
        }}})
        obj.execution.merge({"scenario": "newman"})
        obj.engine.aggregator.prepare()
        obj.prepare()
        obj.startup()
        obj.engine.aggregator.startup()
        while not obj.check():
            obj.engine.aggregator.check()
            time.sleep(obj.engine.check_interval)
        obj.shutdown()
        obj.engine.aggregator.shutdown()
        obj.post_process()
        obj.engine.aggregator.post_process()
        self.assertTrue(obj.has_results())
        with open(obj.report_file) as fds:
            samples = [json.loads(line) for line in fds.readlines()]
        self.assertEqual(1, len(samples))
        sample = samples[0]
        self.assertEqual(sample["status"], "FAILED")
        self.assertEqual(sample["error_msg"], "expect response be 200")
