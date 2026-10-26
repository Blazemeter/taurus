# coding=utf-8
import ast
import os
from unittest import mock

import apiritif
from apiritif import thread
from apiritif.action_plugins import BaseActionHandler

import bzt.modules._apiritif.executor
import bzt.modules._selenium
from bzt.modules._apiritif import ApiritifNoseExecutor
from bzt.modules._selenium import SeleniumExecutor
from tests.unit import BZTestCase, RESOURCES_DIR, ExecutorTestCase, local_paths_config
from tests.unit.modules._selenium import MockPythonTool


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


# Login, set-variables, Search, "Checkout ${user}", teardown Logout -> _1_Login, _2_set_variables, _3_Search,
# _4_Checkout_user, _5_Logout
SKIP_REQUESTS = [
    {"label": "Login", "actions": ["go(https://blazedemo.com/login)"]},
    {"set-variables": {"user": "u1"}},
    {"label": "Search", "actions": ["go(https://blazedemo.com/search)"]},
    {"label": "Checkout ${user}", "actions": ["go(https://blazedemo.com/checkout)"]},
    {"teardown": [{"label": "Logout", "actions": ["go(https://blazedemo.com/logout)"]}]},
]
MAIN_STEPS = ["_1_Login", "_2_set_variables", "_3_Search", "_4_Checkout_user"]
TEARDOWN_STEPS = ["_5_Logout"]


class FakeTest(object):
    """ Stands in for the generated TestCase: each step records its call and sets the thread-store status """

    def __init__(self, statuses):
        self.calls = []
        for name in MAIN_STEPS + TEARDOWN_STEPS:
            status = statuses.get(name, None if name == "_2_set_variables" else "success")
            setattr(self, name, self._make_step(name, status))

    def _make_step(self, name, status):
        def step():
            self.calls.append(name)
            if status is not None:  # set-variables steps don't open a transaction, so they set no status
                apiritif.put_into_thread_store(status=status)

        return step


class TestSkipOnFailureGeneration(ExecutorTestCase):
    EXECUTOR = SeleniumExecutor

    def setUp(self):
        super(TestSkipOnFailureGeneration, self).setUp()
        self.tmp_selenium = bzt.modules._selenium.Selenium
        self.tmp_apiritif = bzt.modules._apiritif.executor.Apiritif
        self.tmp_selenium_apiritif = bzt.modules._apiritif.executor.Selenium
        bzt.modules._selenium.Selenium = MockPythonTool
        bzt.modules._apiritif.executor.Apiritif = MockPythonTool
        bzt.modules._apiritif.executor.Selenium = MockPythonTool
        self.engine.configure([local_paths_config()])
        self.obj.settings = self.engine.config.get("modules").get("selenium")
        self.obj.install_required_tools = lambda: None

    def tearDown(self):
        if self.obj and self.obj.runner:
            if self.obj.runner.stdout:
                self.obj.runner.stdout.close()
            if self.obj.runner.stderr:
                self.obj.runner.stderr.close()
        bzt.modules._selenium.Selenium = self.tmp_selenium
        bzt.modules._apiritif.executor.Apiritif = self.tmp_apiritif
        bzt.modules._apiritif.executor.Selenium = self.tmp_selenium_apiritif
        super(TestSkipOnFailureGeneration, self).tearDown()

    def prepare(self, requests=None, markers=True, **scenario_keys):
        scenario = {"generate-flow-markers": markers, "requests": requests or SKIP_REQUESTS}
        scenario.update(scenario_keys)
        self.configure({
            "execution": [{"executor": "selenium", "iterations": 1, "scenario": "sc"}],
            "scenarios": {"sc": scenario}})
        self.obj.prepare()
        with open(self.obj.script) as fds:
            return fds.read()

    def run_master(self, statuses, preset_status=None):
        """ Execute the generated test_* method against FakeTest; returns (step calls, reported labels) """
        with open(self.obj.script) as fds:
            tree = ast.parse(fds.read())
        test_class = next(node for node in tree.body if isinstance(node, ast.ClassDef))
        master = next(node for node in test_class.body
                      if isinstance(node, ast.FunctionDef) and node.name.startswith("test_"))
        reported = []
        namespace = {"apiritif": apiritif,
                     "report_skipped_steps": lambda steps: reported.extend(label for _, label in steps)}
        exec(compile(ast.Module(body=[master], type_ignores=[]), self.obj.script, "exec"), namespace)

        apiritif.put_into_thread_store(status=preset_status)
        fake = FakeTest(statuses)
        namespace[master.name](fake)
        return fake.calls, reported

    # --- golden files ---

    def test_nfc_golden_unchanged_when_flag_false(self):
        self.obj.engine.config.load([RESOURCES_DIR + 'selenium/test_nfc.yml'])
        self.obj.engine.config['scenarios']['sc1']['skip-on-failure'] = False
        self.configure(self.obj.engine.config['execution'][0])
        self.obj.settings['verbose'] = True
        self.obj.prepare()
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(RESOURCES_DIR + "selenium/test_nfc.py", self.obj.script, str_to_replace,
                              "/somewhere/", python_files=True)

    def test_nfc_golden_flag_on(self):
        self.obj.engine.config.load([RESOURCES_DIR + 'selenium/test_nfc.yml'])
        self.obj.engine.config['scenarios']['sc1']['skip-on-failure'] = True
        self.obj.engine.config['scenarios']['sc1']['generate-flow-markers'] = True
        self.configure(self.obj.engine.config['execution'][0])
        self.obj.settings['verbose'] = True
        self.obj.prepare()
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(RESOURCES_DIR + "selenium/test_skip_on_failure.py", self.obj.script, str_to_replace,
                              "/somewhere/", python_files=True)

    # --- shape ---

    def test_flag_absent_no_skip_code(self):
        script = self.prepare()
        self.assertNotIn("steps.pop(0)", script)
        self.assertNotIn("report_skipped_steps", script)

    def test_no_markers_no_report(self):
        script = self.prepare(markers=False, **{"skip-on-failure": True})
        self.assertIn("steps.pop(0)", script)
        self.assertNotIn("report_skipped_steps", script)

    def test_no_teardown_no_try(self):
        script = self.prepare(requests=SKIP_REQUESTS[:4], **{"skip-on-failure": True})
        self.assertIn("steps.pop(0)", script)
        self.assertNotIn("set_stage", script)
        calls, reported = self.run_master({"_1_Login": "failed"})
        self.assertEqual(["_1_Login"], calls)
        self.assertEqual([None, "Search", "Checkout ${user}"], reported)

    # --- behaviour (FR-3, FR-4, FR-5, AC-7) ---

    def test_assertion_failure_skips_rest_and_runs_teardown(self):
        self.prepare(**{"skip-on-failure": True})
        calls, reported = self.run_master({"_1_Login": "failed"})
        self.assertEqual(["_1_Login", "_5_Logout"], calls)
        self.assertEqual([None, "Search", "Checkout ${user}"], reported)

    def test_broken_skips_rest_and_runs_teardown(self):
        self.prepare(**{"skip-on-failure": True})
        calls, reported = self.run_master({"_3_Search": "broken"})
        self.assertEqual(["_1_Login", "_2_set_variables", "_3_Search", "_5_Logout"], calls)
        self.assertEqual(["Checkout ${user}"], reported)

    def test_all_pass_runs_everything(self):
        self.prepare(**{"skip-on-failure": True})
        calls, reported = self.run_master({})
        self.assertEqual(MAIN_STEPS + TEARDOWN_STEPS, calls)
        self.assertEqual([], reported)

    def test_stale_failed_status_does_not_skip(self):
        self.prepare(**{"skip-on-failure": True})
        calls, reported = self.run_master({}, preset_status="failed")  # left over from a previous iteration
        self.assertEqual(MAIN_STEPS + TEARDOWN_STEPS, calls)
        self.assertEqual([], reported)

    def test_teardown_failure_does_not_skip(self):
        self.prepare(**{"skip-on-failure": True})
        calls, reported = self.run_master({"_5_Logout": "failed"})
        self.assertEqual(MAIN_STEPS + TEARDOWN_STEPS, calls)
        self.assertEqual([], reported)

    def test_label_with_undefined_var_does_not_crash(self):
        script = self.prepare(**{"skip-on-failure": True})
        self.assertIn("'Checkout ${user}'", script)  # raw label, not 'Checkout {}'.format(self.vars['user'])
        calls, reported = self.run_master({"_3_Search": "failed"})  # FakeTest has no self.vars at all
        self.assertEqual(["Checkout ${user}"], reported)

    # --- flag sources (roadmap §3: scenario key wins, modules.apiritif fallback) ---

    def prepare_with_module(self, module_value, **scenario_keys):
        self.obj.engine.config.merge({"modules": {"apiritif": {"skip-on-failure": module_value}}})
        return self.prepare(**scenario_keys)

    def test_module_setting_fallback(self):
        script = self.prepare_with_module(True)  # what a.blazemeter MOB-54692 sends
        self.assertIn("steps.pop(0)", script)

    def test_scenario_false_wins_over_module(self):
        script = self.prepare_with_module(True, **{"skip-on-failure": False})
        self.assertNotIn("steps.pop(0)", script)

    def test_scenario_true_wins_over_module_false(self):
        script = self.prepare_with_module(False, **{"skip-on-failure": True})
        self.assertIn("steps.pop(0)", script)

    def test_string_true_is_on(self):
        script = self.prepare(**{"skip-on-failure": "True"})
        self.assertIn("steps.pop(0)", script)

    def test_string_false_is_off(self):
        script = self.prepare(**{"skip-on-failure": "false"})
        self.assertNotIn("steps.pop(0)", script)

    def test_script_scenario_is_noop(self):
        script_path = RESOURCES_DIR + "selenium/python/test_blazemeter_fail.py"
        self.configure({
            "modules": {"apiritif": {"skip-on-failure": True}},
            "execution": [{"executor": "selenium", "iterations": 1,
                           "scenario": {"script": script_path, "skip-on-failure": True}}]})
        self.obj.prepare()  # must not raise
        self.assertTrue(self.obj.script.endswith("test_blazemeter_fail.py"), self.obj.script)
        self.assertFalse(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "test_requests.py")))


class TestSkipOnFailureApiritifMode(ExecutorTestCase):
    """ plain apiritif (HTTP) scenarios are out of scope: the key is ignored """
    EXECUTOR = ApiritifNoseExecutor

    def test_apiritif_mode_ignores_key(self):
        self.configure({
            "modules": {"apiritif": {"skip-on-failure": True}},
            "execution": [{
                "test-mode": "apiritif",
                "iterations": 1,
                "scenario": {
                    "skip-on-failure": True,
                    "default-address": "http://blazedemo.com",
                    "requests": ["/", "/reserve.php"]}}]})
        tmp_tool = bzt.modules._apiritif.executor.Apiritif
        try:
            bzt.modules._apiritif.executor.Apiritif = MockPythonTool
            self.obj.prepare()
        finally:
            bzt.modules._apiritif.executor.Apiritif = tmp_tool

        with open(self.obj.script) as fds:
            self.assertNotIn("steps.pop(0)", fds.read())
