import os
import shutil
import time

from os.path import join, exists, dirname
from bzt.modules import javascript
from bzt.modules.javascript import WebdriverIOExecutor
from bzt.utils import get_full_path, shell_exec, is_windows
from tests import BUILD_DIR, RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumMochaRunner(SeleniumTestCase):
    def test_selenium_prepare_mocha(self):
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"
        }})
        self.obj.prepare()

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

            self.obj.settings.merge({
                "selenium-tools": {
                    "mocha": {
                        "tools-dir": dummy_installation_path}}})

            self.obj.execution.merge({
                "scenario": {"script": RESOURCES_DIR + "selenium/js-mocha/bd_scenarios.js"}})
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
            time.sleep(1)
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
