import json
import os

import time
import yaml

import apiritif
from selenium.common.exceptions import NoSuchElementException

import bzt
from bzt.engine import EXEC
from bzt.modules import ConsolidatingAggregator
from bzt.modules.selenium import GeckoDriver
from bzt.modules.functional import FuncSamplesReader, LoadSamplesReader, FunctionalAggregator
from bzt.modules.apiritif import ApiritifNoseExecutor
from bzt.modules.pytest import PyTestExecutor
from bzt.modules.robot import RobotExecutor
from tests import RESOURCES_DIR, ExecutorTestCase, BZTestCase
from tests.modules.selenium import SeleniumTestCase
from bzt.resources.selenium_extras import get_locator, BYS
from bzt.utils import EXE_SUFFIX


class MockWebDriver(object):
    def __init__(self, content, timeout=60):
        self.content = []
        for element in content:
            key, val = list(element.items())[0]
            self.content.append((BYS[key.lower()], val))
        self.timeout = timeout
        self.waiting_time = 0

    def implicitly_wait(self, timeout):
        self.timeout = timeout

    def find_elements(self, *target):
        self.waiting_time += self.timeout
        return [element for element in self.content if element == target]


class TestLocatorsMagager(BZTestCase):
    def test_get_locator_timeout(self):
        content = [{'css': 'existed_css'}]
        timeout = 30
        driver = MockWebDriver(content=content, timeout=timeout)

        apiritif.put_into_thread_store(driver=driver, timeout=timeout, func_mode=False)

        # exception should be raised when raise_exception is True
        missing_locators = [{'css': 'missing_css'}, {'xpath': 'missing_xpath'}]
        self.assertRaises(NoSuchElementException, get_locator, missing_locators, False, True)
        self.assertEqual(30, driver.waiting_time)

        # exception should not be raised when raise_exception is False
        driver.waiting_time = 0
        locators = get_locator(missing_locators, ignore_implicit_wait=True, raise_exception=False)
        self.assertEqual(locators, (BYS['css'], 'missing_css'))
        # actual waiting time is 0 when setting ignore_implicit_wait to True
        self.assertEqual(0, driver.waiting_time)

        driver.waiting_time = 0
        existed_locators = [{'css': 'existed_css'}]
        get_locator(existed_locators)
        self.assertEqual(30, driver.waiting_time)


class TestSeleniumApiritifRunner(SeleniumTestCase):
    def test_selenium_prepare_python_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/python/test_blazemeter_fail.py"
        }})
        self.obj.prepare()

    def test_selenium_prepare_python_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/python/"}})
        self.obj.prepare()

    def test_selenium_startup_shutdown_python_single(self):
        """
        run tests from .py file
        :return:
        """
        self.configure({
            'execution': {
                "iterations": 1,
                'scenario': {'script': RESOURCES_DIR + 'selenium/python/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/python/test_blazemeter_fail.py"
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "apiritif.0.csv")))

    def test_selenium_startup_shutdown_python_folder(self):
        """
        run tests from .py files
        :return:
        """
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {'script': RESOURCES_DIR + 'selenium/python/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        api_log = os.path.join(self.obj.engine.artifacts_dir, "apiritif.0.csv")
        nose_log = os.path.join(self.obj.engine.artifacts_dir, "apiritif.out")
        self.assertTrue(os.path.exists(api_log))
        with open(nose_log) as fds:
            content = fds.read()
            self.assertIn("Transaction started::", content)
            self.assertIn("Transaction ended::", content)

    def test_runner_fail_no_test_found(self):
        """
        Check that Python Apiritif runner fails if no tests were found
        :return:
        """
        self.configure({
            EXEC: {
                "iterations": 1,
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/dummy.py"}
            }
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()

        diagnostics = "\n".join(self.obj.get_error_diagnostics())
        self.assertIn("Nothing to test.", diagnostics)

    def test_resource_files_collection_remote_apiritif(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/python/"}})
        self.assertEqual(len(self.obj.resource_files()), 1)

    def test_setup_exception(self):
        """
        Do not crash when test's setUp/setUpClass fails
        :return:
        """
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "selenium/python/test_setup_exception.py"
        }})
        self.obj.engine.aggregator = FunctionalAggregator()
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        diagnostics = "\n".join(self.obj.get_error_diagnostics())
        self.assertIn("Nothing to test", diagnostics)

    def test_long_iterations_value(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        self.obj.execution.merge({
            "iterations": 2 ** 64,
            "scenario": {
                "requests": [
                    "http://blazedemo.com/",
                ],
            }
        })
        self.obj.prepare()
        try:
            self.obj.startup()
            for _ in range(3):
                self.assertFalse(self.obj.check())
                self.engine.aggregator.check()
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()


class TestApiritifRunner(ExecutorTestCase):
    EXECUTOR = ApiritifNoseExecutor

    def test_new_flow(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "iterations": 1,
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                        {"set-variables": {"name1": "val1"}},
                        {
                            "transaction": "second",
                            "do": [
                                "/other.html",
                                "/reserve.php",
                                {
                                    "transaction": "third",
                                    "do": [
                                        "/${name1}"
                                    ]
                                }
                            ]}]}}]})

        self.obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "test_requests.py")))
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_apiritif_generated_requests(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "iterations": 1,
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                        "/reserve.php"]}}]})

        self.obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "test_requests.py")))
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_apiritif_transactions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "apiritif/test_transactions.py"
                }
            }]
        })
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_report_reading(self):
        reader = FuncSamplesReader(RESOURCES_DIR + "apiritif/transactions.ldjson", self.obj.engine, self.obj.log)
        items = list(reader.read(last_pass=True))
        self.assertEqual(9, len(items))
        self.assertEqual(items[0].get_short_name(), 'TestRequests.test_1_single_request')
        self.assertEqual(items[1].get_short_name(), 'TestRequests.test_2_multiple_requests')
        self.assertEqual(items[2].get_short_name(), 'test_3_toplevel_transaction.Transaction')
        self.assertEqual(items[3].get_short_name(), 'test_4_mixed_transaction.Transaction')
        self.assertEqual(items[4].get_short_name(), 'test_5_multiple_transactions.Transaction 1')
        self.assertEqual(items[5].get_short_name(), 'test_5_multiple_transactions.Transaction 2')
        self.assertEqual(items[6].get_short_name(), 'test_6_transaction_obj.Label')
        self.assertEqual(items[7].get_short_name(), 'test_7_transaction_fail.Label')
        self.assertEqual(items[8].get_short_name(), 'test_8_transaction_attach.Label')

    def test_report_transactions_as_failed(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "iterations": 1,
                "scenario": {
                    "default-address": "http://httpbin.org",
                    "requests": [{
                        "label": "failure by 404",
                        "url": "/status/404",
                    }]
                }
            }]
        })
        self.obj.engine.aggregator = FunctionalAggregator()
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)
        reader = LoadSamplesReader(os.path.join(self.obj.engine.artifacts_dir, "apiritif.0.ldjson"), self.obj.log)
        samples = list(reader._read(last_pass=True))
        self.assertEqual(len(samples), 1)
        tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count = samples[0]
        self.assertIsNotNone(error)

    def test_status_skipped(self):
        self.configure({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "functional/test_all.py"
                }
            }]
        })
        self.obj.engine.aggregator = FunctionalAggregator()
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        reader = FuncSamplesReader(os.path.join(self.obj.engine.artifacts_dir, "apiritif.0.ldjson"),
                                   self.obj.engine, self.obj.log)
        samples = list(reader.read(last_pass=True))
        self.assertEqual(len(samples), 4)
        self.assertIsNotNone(samples[-1].status)


class TestPyTestExecutor(ExecutorTestCase):
    EXECUTOR = PyTestExecutor
    CMD_LINE = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = args

    def exec_and_communicate(self, *args, **kwargs):
        return "", ""

    def full_run(self, config):
        self.obj.execution.merge(config)

        tmp_aec = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_aec

        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_full_single_script(self):
        self.obj.execution.merge({
            "iterations": 1,
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_statuses.py"
            }
        })
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)

    def test_statuses(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_statuses.py"
            }
        })
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        with open(self.obj.report_file) as fds:
            report = [json.loads(line) for line in fds.readlines() if line]
        self.assertEqual(4, len(report))
        self.assertEqual(["PASSED", "FAILED", "FAILED", "SKIPPED"], [item["status"] for item in report])

        failed_item = report[1]
        assertions = failed_item["assertions"]
        self.assertEqual(1, len(assertions))
        assertion = assertions[0]
        self.assertEqual('assert (2 + (2 * 2)) == 8', assertion['error_msg'])
        self.assertTrue(assertion['failed'])
        self.assertEqual('AssertionError: assert (2 + (2 * 2)) == 8', assertion['name'])
        self.assertIsNotNone(assertion.get('error_trace'))

    def test_report_file(self):
        self.full_run({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.assertTrue('--report-file' in self.CMD_LINE)
        val = self.CMD_LINE[self.CMD_LINE.index('--report-file') + 1]
        self.assertTrue(val.endswith("PyTestExecutor.ldjson"))

    def test_iterations(self):
        self.full_run({
            "iterations": 10,
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.assertTrue('-i 10' in ' '.join(self.CMD_LINE))

    def test_hold(self):
        self.full_run({
            "hold-for": "3s",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.assertTrue('-d 3.0' in ' '.join(self.CMD_LINE))

    def test_script(self):
        self.full_run({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.assertTrue(self.CMD_LINE[-1].endswith("test_single.py"))

    def test_blazedemo(self):
        self.obj.engine.check_interval = 0.1
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_blazedemo.py"
            }
        })

        tmp_aec = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_aec

        driver = self.obj._get_tool(GeckoDriver, config=self.obj.settings.get('geckodriver'))
        if not driver.check_if_installed():
            driver.install()
        self.obj.env.add_path({"PATH": driver.get_driver_dir()})

        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_package(self):
        self.obj.engine.check_interval = 0.1
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/"
            }
        })

        tmp_aec = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_aec

        driver = self.obj._get_tool(GeckoDriver, config=self.obj.settings.get('geckodriver'))
        if not driver.check_if_installed():
            driver.install()
        self.obj.env.add_path({"PATH": driver.get_driver_dir()})

        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_additional_args(self):
        additional_args = "--foo --bar"
        self.obj.runner_path = RESOURCES_DIR + "selenium/pytest/bin/runner.py"
        self.full_run({
            "scenario": {
                "additional-args": additional_args,
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.assertTrue(additional_args in " ".join(self.CMD_LINE))


class TestRobotExecutor(ExecutorTestCase):
    EXECUTOR = RobotExecutor
    CMD_LINE = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = args

    def exec_and_communicate(self, *args, **kwargs):
        return "", ""

    def test_full_single_script(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })

        tmp_aec = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
            self.obj.settings["interpreter"] = RESOURCES_DIR + "selenium/robot/robot-mock" + EXE_SUFFIX
            self.obj.startup()
        finally:
            bzt.utils.exec_and_communicate = tmp_aec
            self.obj.shutdown()
            self.obj.post_process()

        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)
        lines = open(self.obj.report_file).readlines()
        self.assertEqual(1, len(lines))

    def full_run(self, config):
        self.configure(config)
        tmp_aec = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = self.exec_and_communicate
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_aec
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_hold(self):
        self.full_run({
            "execution": [{
                "hold-for": "5s",
                "iterations": 3,
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })
        self.assertTrue('--duration' in self.CMD_LINE)
        dur_val = self.CMD_LINE[self.CMD_LINE.index('--duration')+1]
        self.assertEqual(dur_val, '5.0')

    def test_report_file(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })
        self.assertTrue('--report-file' in self.CMD_LINE)
        report_file = self.CMD_LINE[self.CMD_LINE.index('--report-file')+1]
        self.assertTrue(report_file.endswith("RobotExecutor.ldjson"))

    def test_iterations(self):
        self.full_run({
            "execution": [{
                "iterations": 3,
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })
        self.assertTrue('--iterations' in self.CMD_LINE)
        iters_val = self.CMD_LINE[self.CMD_LINE.index('--iterations')+1]
        self.assertEqual(iters_val, '3')

    def test_variables(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "variables": {
                        "USERNAME": "janedoe",
                    },
                    "script": RESOURCES_DIR + "selenium/robot/simple/test_novar.robot",
                }
            }]
        })
        self.assertTrue('--variablefile' in self.CMD_LINE)
        var_file = self.CMD_LINE[self.CMD_LINE.index('--variablefile')+1]
        self.assertTrue(var_file.endswith("robot-vars.yaml"))
        self.assertEqual('janedoe', yaml.full_load(open(var_file).read())['USERNAME'])

    def test_variables_file(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "variables": RESOURCES_DIR + "selenium/robot/simple/vars.yaml",
                    "script": RESOURCES_DIR + "selenium/robot/simple/test_novar.robot",
                }
            }]
        })
        self.assertTrue('--variablefile' in self.CMD_LINE)
        var_file = self.CMD_LINE[self.CMD_LINE.index('--variablefile')+1]
        self.assertEqual(var_file, os.path.normpath(RESOURCES_DIR + "selenium/robot/simple/vars.yaml"))

    def test_output_file(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })
        self.assertTrue('--outputfile' in self.CMD_LINE)
        out_file = self.CMD_LINE[self.CMD_LINE.index('--outputfile')+1]
        self.assertTrue(out_file.endswith("output.xml"))

    def test_log_file(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })
        self.assertTrue('--logfile' in self.CMD_LINE)
        log_file = self.CMD_LINE[self.CMD_LINE.index('--logfile')+1]
        self.assertTrue(log_file.endswith("log.html"))

    def test_single_tag(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "tags": "create",
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot",
                }
            }]
        })
        self.assertTrue('--include' in self.CMD_LINE)
        tags = self.CMD_LINE[self.CMD_LINE.index('--include')+1]
        self.assertEqual(tags, 'create')

    def test_multiple_tags(self):
        self.full_run({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "tags": "create,database",
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot",
                }
            }]
        })
        self.assertTrue('--include' in self.CMD_LINE)
        tags = self.CMD_LINE[self.CMD_LINE.index('--include')+1]
        self.assertEqual(tags, 'create,database')
