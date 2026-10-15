# coding=utf-8
from unittest import mock

import apiritif
from apiritif import thread
from apiritif.action_plugins import BaseActionHandler

from tests.unit import BZTestCase


class RecordingActionHandler(BaseActionHandler):
    def __init__(self, fail=False):
        super(RecordingActionHandler, self).__init__()
        self.calls = []
        self.fail = fail

    def startup(self):
        pass

    def handle(self, session_id, action_type, action):
        self.calls.append((session_id, action_type, dict(action)))
        if self.fail:
            raise KeyError(session_id)

    def finalize(self):
        pass


class TestReportSkippedSteps(BZTestCase):
    def setUp(self):
        super(TestReportSkippedSteps, self).setUp()
        self.handler = RecordingActionHandler()
        self.driver = mock.MagicMock(session_id="grid-session-1")
        apiritif.put_into_thread_store(
            action_handlers=[self.handler], driver=self.driver, scenario_name="sc1",
            test_suite="TestSc1", data_sources=None)

    def report(self, steps):
        from bzt.resources.skip_on_failure import report_skipped_steps
        report_skipped_steps(steps)

    def test_reports_each_remaining_transaction_as_skipped(self):
        self.report([(None, "Search"), (None, None), (None, "Checkout")])

        self.assertEqual([
            ("grid-session-1", BaseActionHandler.TEST_CASE_START, {"testCaseName": "Search", "testSuiteName": "sc1"}),
            ("grid-session-1", BaseActionHandler.TEST_CASE_STOP, {"status": "skipped", "message": ""}),
            ("grid-session-1", BaseActionHandler.TEST_CASE_START, {"testCaseName": "Checkout", "testSuiteName": "sc1"}),
            ("grid-session-1", BaseActionHandler.TEST_CASE_STOP, {"status": "skipped", "message": ""}),
        ], self.handler.calls)

    def test_no_flow_marker_sent_to_grid(self):
        self.report([(None, "Search")])
        self.driver.execute_script.assert_not_called()

    def test_nothing_to_report(self):
        self.report([])
        self.report([(None, None)])
        self.assertEqual([], self.handler.calls)

    def test_data_sources_add_iteration_id(self):
        apiritif.put_into_thread_store(data_sources=True)
        thread.set_iteration(3)
        try:
            self.report([(None, "Search")])
        finally:
            thread.set_iteration(0)

        self.assertEqual(3, self.handler.calls[0][2]["testDataIterationId"])

    def test_suite_name_fallback(self):
        apiritif.put_into_thread_store(scenario_name=None)
        self.report([(None, "Search")])
        self.assertEqual("TestSc1", self.handler.calls[0][2]["testSuiteName"])

    def test_handler_exception_is_swallowed(self):
        failing = RecordingActionHandler(fail=True)
        apiritif.put_into_thread_store(action_handlers=[failing, self.handler])

        self.report([(None, "Search")])  # must not raise: an escaped exception becomes an error sample

        self.assertEqual(1, len(failing.calls))
        self.assertEqual(2, len(self.handler.calls))

    def test_no_handlers_no_driver(self):
        apiritif.put_into_thread_store(action_handlers=None, driver=None)
        self.report([(None, "Search")])  # must not raise

    def test_reexported_from_selenium_extras_source(self):
        # selenium_extras imports selenium (not installed for unit tests), so check the source instead
        import os
        import bzt
        path = os.path.join(os.path.dirname(bzt.__file__), "resources", "selenium_extras.py")
        with open(path) as fds:
            self.assertIn("from bzt.resources.skip_on_failure import report_skipped_steps", fds.read())
