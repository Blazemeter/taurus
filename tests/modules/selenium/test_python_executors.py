import json
import os

import time

import apiritif
from selenium.common.exceptions import NoSuchElementException

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

