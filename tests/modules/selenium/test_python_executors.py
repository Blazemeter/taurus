import json
import os

import time

from bzt.engine import EXEC
from bzt.modules import ConsolidatingAggregator
from bzt.modules.functional import FuncSamplesReader, LoadSamplesReader, FunctionalAggregator
from bzt.modules.apiritif import ApiritifNoseExecutor
from bzt.modules.pytest import PyTestExecutor
from bzt.modules.robot import RobotExecutor
from tests import RESOURCES_DIR, ExecutorTestCase
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumNoseRunner(SeleniumTestCase):
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
        nose_log = os.path.join(self.obj.engine.artifacts_dir, "nose.out")
        self.assertTrue(os.path.exists(api_log))
        with open(nose_log) as fds:
            content = fds.read()
            self.assertIn("Transaction started::", content)
            self.assertIn("Transaction ended::", content)

    def test_runner_fail_no_test_found(self):
        """
        Check that Python Nose runner fails if no tests were found
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

    def test_resource_files_collection_remote_nose(self):
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


class TestNoseRunner(ExecutorTestCase):
    EXECUTOR = ApiritifNoseExecutor

    def test_full_single_script(self):
        self.obj.engine.check_interval = 0.1
        self.obj.execution.merge({
            "iterations": 1,
            "ramp-up": "10s",
            "hold-for": "10s",
            "steps": 5,
            "scenario": {
                "script": RESOURCES_DIR + "apiritif/test_codegen.py"}})

        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertFalse(self.obj.has_results())
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

    def test_iterations(self):
        self.obj.execution.merge({
            "iterations": 10,
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
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
        self.assertEqual(10, len(report))
        self.assertTrue(all(item["status"] == "PASSED" for item in report))

    def test_hold(self):
        self.obj.execution.merge({
            "hold-for": "3s",
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.obj.prepare()
        try:
            start_time = time.time()
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
            end_time = time.time()
        self.obj.post_process()
        duration = end_time - start_time
        self.assertGreaterEqual(duration, 3.0)

    def test_blazedemo(self):
        self.obj.engine.check_interval = 0.1
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/test_blazedemo.py"
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
        self.assertEqual(2, len(report))

    def test_package(self):
        self.obj.engine.check_interval = 0.1
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/pytest/"
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
        self.assertEqual(7, len(report))

    def test_additional_args(self):
        additional_args = "--foo --bar"
        self.obj.execution.merge({
            "scenario": {
                "additional-args": additional_args,
                "script": RESOURCES_DIR + "selenium/pytest/test_single.py"
            }
        })
        self.obj.runner_path = RESOURCES_DIR + "selenium/pytest/bin/runner.py"
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        with open(self.obj.stdout.name) as fds:
            stdout = fds.read()
            self.assertIn(additional_args, stdout)


class TestRobotExecutor(ExecutorTestCase):
    EXECUTOR = RobotExecutor

    def test_full_single_script(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
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
        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)
        lines = open(self.obj.report_file).readlines()
        self.assertEqual(5, len(lines))

    def test_hold(self):
        self.configure({
            "execution": [{
                "hold-for": "5s",
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
                }
            }]
        })
        self.obj.prepare()
        try:
            start_time = time.time()
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertTrue(os.path.exists(self.obj.report_file))
        duration = time.time() - start_time
        self.assertGreater(duration, 5)

    def test_iterations(self):
        self.configure({
            "execution": [{
                "iterations": 3,
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot"
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
        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)
        lines = open(self.obj.report_file).readlines()
        self.assertEqual(3 * 5, len(lines))

    def test_variables(self):
        self.configure({
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
        samples = [json.loads(line) for line in open(self.obj.report_file).readlines() if line]
        self.obj.log.info(samples)
        self.assertEqual(5, len(samples))
        self.assertTrue(all(sample["status"] == "PASSED" for sample in samples))

    def test_variables_file(self):
        self.configure({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "variables": RESOURCES_DIR + "selenium/robot/simple/vars.yaml",
                    "script": RESOURCES_DIR + "selenium/robot/simple/test_novar.robot",
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
        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)
        samples = [json.loads(line) for line in open(self.obj.report_file).readlines() if line]
        self.obj.log.info(samples)
        self.assertEqual(5, len(samples))
        self.assertTrue(all(sample["status"] == "PASSED" for sample in samples))

    def test_single_tag(self):
        self.configure({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "tags": "create",
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot",
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
        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)
        samples = [json.loads(line) for line in open(self.obj.report_file).readlines() if line]
        self.obj.log.info(samples)
        self.assertEqual(1, len(samples))
        self.assertTrue(all(sample["status"] == "PASSED" for sample in samples))

    def test_multiple_tags(self):
        self.configure({
            "execution": [{
                "iterations": 1,
                "scenario": {
                    "tags": "create,database",
                    "script": RESOURCES_DIR + "selenium/robot/simple/test.robot",
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
        self.assertFalse(self.obj.has_results())
        self.assertNotEquals(self.obj.process, None)
        samples = [json.loads(line) for line in open(self.obj.report_file).readlines() if line]
        self.obj.log.info(samples)
        self.assertEqual(2, len(samples))
        self.assertTrue(all(sample["status"] == "PASSED" for sample in samples))
