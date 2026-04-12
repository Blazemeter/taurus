import json
import logging
import os
import time
from os.path import exists

import bzt

from bzt import ToolError
from bzt.modules.javascript import NPMPackage, JavaScriptExecutor, NewmanExecutor, Mocha, JSSeleniumWebdriver, \
    PlaywrightTester, PLAYWRIGHT, PlaywrightTestPackage, IncrementalLineReader, PlaywrightLogReader, \
    NPMModulePackage, NPM
from bzt.utils import get_full_path, EXE_SUFFIX, CALL_PROBLEMS

from tests.unit import RESOURCES_DIR, BZTestCase, EngineEmul
from tests.unit.modules._selenium import SeleniumTestCase

from unittest.mock import patch, MagicMock


class TestSeleniumMochaRunner(SeleniumTestCase):
    RUNNER_STUB = RESOURCES_DIR + "selenium/js-mocha/mocha" + EXE_SUFFIX
    CMD_LINE = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = ' '.join(args)

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

    def prepare(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config['execution']
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

    def full_run(self, config):
        self.prepare(config)
        self.obj.runner.get_launch_cmdline = lambda *args: [TestSeleniumMochaRunner.RUNNER_STUB] + list(args)
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()

    def simple_run(self, config):
        self.prepare(config)
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

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
        self.simple_run({
            'execution': {
                'hold-for': '5s',
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-mocha/'},
                'executor': 'selenium'
            },
        })
        self.assertIn("--hold-for 5.0", self.CMD_LINE)

    def test_mocha_iterations(self):
        self.simple_run({
            'execution': {
                'iterations': 3,
                'scenario': {'script': RESOURCES_DIR + 'selenium/js-mocha'},
                'executor': 'selenium'
            },
        })
        self.assertIn("--iterations 3", self.CMD_LINE)


class TestNPMPackageNameParser(BZTestCase):
    def test_version_parsing(self):
        self.tools_dir = "~/.bzt/selenium-taurus/"

        class DummyPackageDefaultFormat(NPMPackage):
            PACKAGE_NAME = 'package@6.0.1'

        class DummyPackageScopedFormat(NPMPackage):
            PACKAGE_NAME = '@scope/package@9.0.0'

        self.npmPackageDefaultFormat = DummyPackageDefaultFormat(tools_dir=self.tools_dir, node_tool='', npm_tool='')
        self.npmPackageScopedFormat = DummyPackageScopedFormat(tools_dir=self.tools_dir, node_tool='', npm_tool='')

        self.assertEqual(self.npmPackageDefaultFormat.package_name, 'package')
        self.assertEqual(self.npmPackageDefaultFormat.version, '6.0.1')
        self.assertEqual(self.npmPackageScopedFormat.package_name, '@scope/package')
        self.assertEqual(self.npmPackageScopedFormat.version, '9.0.0')


class TestNewmanExecutor(BZTestCase):
    RUNNER_STUB = RESOURCES_DIR + "newman/newman" + EXE_SUFFIX

    def full_run(self, config):
        self.obj = NewmanExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.config.merge(config)
        execution = config["execution"][0] if isinstance(config["execution"], list) else config["execution"]
        self.obj.execution.merge(execution)

        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.node.tool_path = self.RUNNER_STUB

        self.obj.startup()
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


class TestPlaywrightExecutor(SeleniumTestCase):
    RUNNER_STUB = RESOURCES_DIR + "playwright/playwright" + EXE_SUFFIX
    CMD_LINE = None
    ENV = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = ''.join(args)
        self.ENV = kwargs.get('env').get() if 'env' in kwargs else {}

    def prepare(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config['execution']
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

    def full_run(self, config):
        self.prepare(config)
        self.obj.runner.get_launch_cmdline = lambda *args: [TestPlaywrightExecutor.RUNNER_STUB] + args[0]
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()

    def simple_run(self, config):
        self.prepare(config)
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

    def test_playwright_full(self):
        self.full_run({
            'execution': {
                "executor": "playwright",
                "iterations": 10,
                "scenario": {
                    "script": RESOURCES_DIR + "playwright"
                }
            }
        })
        self.assertEqual(self.obj.runner.execution["executor"], "playwright")
        self.assertEqual(self.obj.runner.engine.modules['playwright'], PlaywrightTester)

        self.assertTrue(os.path.exists(self.obj.runner.reader.filename))
        samples = [sample for sample in self.obj.runner.reader._read(final_pass=True)]
        self.assertEqual(2, len(samples))
        self.assertEqual(samples[0][1], "destination of week")
        self.assertEqual(samples[0][6], None)
        self.assertEqual(samples[1][1], "reserve flight")
        self.assertEqual(samples[1][6], None)

    def test_command_line(self):
        self.simple_run({
            'execution': {
                'iterations': 3,
                'concurrency': 10,
                'hold-for': '1m',
                'settings': {
                    'env': {
                        'BASE_URL': 'https://blazedemo.com/'
                    }
                },
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'browser': 'firefox',
                    'test': 'has title'
                },
                'executor': 'playwright',
            },
        })
        self.assertIn("npx playwright test", self.CMD_LINE)
        self.assertIn("--repeat-each 30", self.CMD_LINE)
        self.assertIn("--workers 10", self.CMD_LINE)
        self.assertIn("-project=firefox", self.CMD_LINE)
        self.assertIn("-reporter \"@taurus/playwright-custom-reporter\"", self.CMD_LINE)
        self.assertIn("-g 'has title'", self.CMD_LINE)
        self.assertEqual('60000', self.ENV.get("TAURUS_PWREPORT_DURATION", "undefined"))

    def test_command_line_additional_reporter(self):
        self.simple_run({
            'execution': {
                'iterations': 3,
                'concurrency': 10,
                'hold-for': '1m',
                'settings': {
                    'env': {
                        'BASE_URL': 'https://blazedemo.com/'
                    }
                },
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'browser': 'firefox',
                    'test': 'has title',
                    'reporters': ['"json" ']
                },
                'executor': 'playwright',
            },
        })
        self.assertIn("npx playwright test", self.CMD_LINE)
        self.assertIn("--repeat-each 30", self.CMD_LINE)
        self.assertIn("--workers 10", self.CMD_LINE)
        self.assertIn("-project=firefox", self.CMD_LINE)
        self.assertIn("-reporter \"@taurus/playwright-custom-reporter,json\"", self.CMD_LINE)
        self.assertIn("-g 'has title'", self.CMD_LINE)
        self.assertEqual('60000', self.ENV.get("TAURUS_PWREPORT_DURATION", "undefined"))

    def test_playwright_stdout_env_with_additional_reporter(self):
        """Test TAURUS_PWREPORT_STDOUT is false when additional reporters are present"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'reporters': ['html', 'json']
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('false', self.ENV.get("TAURUS_PWREPORT_STDOUT"))

    def test_playwright_stdout_env_without_additional_reporter(self):
        """Test TAURUS_PWREPORT_STDOUT is true when no additional reporters"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('true', self.ENV.get("TAURUS_PWREPORT_STDOUT"))

    def test_playwright_reporter_sanitization(self):
        """Test that reporter names are sanitized (spaces, quotes, etc. removed)"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'reporters': ['"html"', " json ", "list\t", "'dot'"]
                },
                'executor': 'playwright',
            },
        })
        # Check that sanitized reporters are in command line
        self.assertIn("@taurus/playwright-custom-reporter,html,json,list,dot", self.CMD_LINE)

    def test_playwright_reporter_empty_values_filtered(self):
        """Test that empty/None reporter values are filtered out"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'reporters': ['html', '', None, 'json', '  ']
                },
                'executor': 'playwright',
            },
        })
        # Should only include html and json
        self.assertIn("@taurus/playwright-custom-reporter,html,json", self.CMD_LINE)

    def test_playwright_reporter_non_list_ignored(self):
        """Test that non-list reporter config is ignored"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'reporters': 'html'  # String instead of list
                },
                'executor': 'playwright',
            },
        })
        # Should only have taurus reporter, not html
        self.assertIn("-reporter \"@taurus/playwright-custom-reporter\"", self.CMD_LINE)
        self.assertNotIn(",html", self.CMD_LINE)

    def test_playwright_reporter_non_string_items_filtered(self):
        """Test that non-string items in reporters list are filtered"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    'reporters': ['html', 123, {'dict': 'value'}, 'json']
                },
                'executor': 'playwright',
            },
        })
        # Should only include html and json
        self.assertIn("@taurus/playwright-custom-reporter,html,json", self.CMD_LINE)

    def test_playwright_tester_tools_dir_initialization(self):
        """Test that PlaywrightTester initializes tools_dir in __init__"""
        tester = PlaywrightTester()
        self.assertIsNotNone(tester.tools_dir)
        self.assertIn(".bzt/playwright", tester.tools_dir)

    def test_playwright_get_script_path_no_execution(self):
        """Test get_script_path returns full path when no execution"""
        tester = PlaywrightTester()
        tester.execution = None

        script_path = tester.get_script_path()
        self.assertIsNotNone(script_path)
        # Should return full path, not relative
        self.assertTrue(os.path.isabs(script_path) or script_path.startswith("~"))
        self.assertIn(".bzt/playwright", script_path)


class TestPlaywrightInstallation(BZTestCase):
    """Test PLAYWRIGHT tool installation logic"""

    def setUp(self):
        super(TestPlaywrightInstallation, self).setUp()
        import tempfile
        self.tools_dir = tempfile.mkdtemp() + "/playwright"

    @patch('bzt.modules.javascript.is_linux')
    @patch('os.geteuid', create=True)
    def test_playwright_install_linux_non_root(self, mock_geteuid, mock_is_linux):
        """Test Playwright install on Linux as non-root user (should skip --with-deps)"""
        mock_is_linux.return_value = True
        mock_geteuid.return_value = 1000  # non-root

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        playwright.call = MagicMock(return_value=("", ""))

        # No frozen version - should try to install
        with patch.dict(os.environ, {}, clear=False):
            if 'PLAYWRIGHT_PACKAGE_FORCED_VERSION' in os.environ:
                del os.environ['PLAYWRIGHT_PACKAGE_FORCED_VERSION']

            playwright.install()

            # Should be called with install but WITHOUT --with-deps
            playwright.call.assert_called_once()
            call_args = playwright.call.call_args[0][0]
            self.assertIn("npx", call_args)
            self.assertIn("playwright", call_args)
            self.assertIn("install", call_args)
            self.assertNotIn("--with-deps", call_args)

    @patch('bzt.modules.javascript.is_linux')
    @patch('os.geteuid', create=True)
    def test_playwright_install_linux_root(self, mock_geteuid, mock_is_linux):
        """Test Playwright install on Linux as root user (should include --with-deps)"""
        mock_is_linux.return_value = True
        mock_geteuid.return_value = 0  # root

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        playwright.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            if 'PLAYWRIGHT_PACKAGE_FORCED_VERSION' in os.environ:
                del os.environ['PLAYWRIGHT_PACKAGE_FORCED_VERSION']

            playwright.install()

            # Should be called with install AND --with-deps
            playwright.call.assert_called_once()
            call_args = playwright.call.call_args[0][0]
            self.assertIn("npx", call_args)
            self.assertIn("playwright", call_args)
            self.assertIn("install", call_args)
            self.assertIn("--with-deps", call_args)

    @patch('bzt.modules.javascript.is_linux')
    def test_playwright_install_non_linux(self, mock_is_linux):
        """Test Playwright install on non-Linux systems (should include --with-deps)"""
        mock_is_linux.return_value = False

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        playwright.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            if 'PLAYWRIGHT_PACKAGE_FORCED_VERSION' in os.environ:
                del os.environ['PLAYWRIGHT_PACKAGE_FORCED_VERSION']

            playwright.install()

            # Should be called with install AND --with-deps
            playwright.call.assert_called_once()
            call_args = playwright.call.call_args[0][0]
            self.assertIn("npx", call_args)
            self.assertIn("playwright", call_args)
            self.assertIn("install", call_args)
            self.assertIn("--with-deps", call_args)

    def test_playwright_install_frozen_version(self):
        """Test that Playwright install is skipped when version is frozen"""
        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        playwright.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {'PLAYWRIGHT_PACKAGE_FORCED_VERSION': '1.40.0'}):
            playwright.install()

            # Should NOT call install when version is frozen
            playwright.call.assert_not_called()

    def test_playwright_install_creates_tools_dir(self):
        """Test that Playwright install creates tools_dir if it doesn't exist"""
        import tempfile
        import shutil

        temp_base = tempfile.mkdtemp()
        try:
            tools_dir = os.path.join(temp_base, "non_existent_dir")
            self.assertFalse(os.path.exists(tools_dir))

            playwright = PLAYWRIGHT(tools_dir=tools_dir)
            playwright.call = MagicMock(return_value=("", ""))

            with patch.dict(os.environ, {}, clear=False):
                if 'PLAYWRIGHT_PACKAGE_FORCED_VERSION' in os.environ:
                    del os.environ['PLAYWRIGHT_PACKAGE_FORCED_VERSION']

                with patch('bzt.modules.javascript.is_linux', return_value=False):
                    playwright.install()

            # Directory should have been created
            self.assertTrue(os.path.exists(tools_dir))
        finally:
            shutil.rmtree(temp_base)

    @patch('bzt.modules.javascript.is_linux')
    def test_playwright_install_cmd_cwd_parameter(self, mock_is_linux):
        """Test that install_cmd uses tools_dir as cwd"""
        mock_is_linux.return_value = False

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        playwright.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            if 'PLAYWRIGHT_PACKAGE_FORCED_VERSION' in os.environ:
                del os.environ['PLAYWRIGHT_PACKAGE_FORCED_VERSION']

            os.makedirs(self.tools_dir, exist_ok=True)
            playwright.install()

            # Check that cwd was passed correctly
            call_kwargs = playwright.call.call_args[1]
            self.assertEqual(call_kwargs.get('cwd'), self.tools_dir)

    @patch('bzt.modules.javascript.is_linux')
    def test_playwright_non_frozen_version_uses_just_package(self, mock_is_linux):
        """Test that without frozen version would use playwright as package name"""
        mock_is_linux.return_value = False

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)

        # Even though install is skipped with frozen version,
        # we can verify the logic by testing without frozen version
        # and checking the package name in the command
        playwright.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            if 'PLAYWRIGHT_PACKAGE_FORCED_VERSION' in os.environ:
                del os.environ['PLAYWRIGHT_PACKAGE_FORCED_VERSION']

            os.makedirs(self.tools_dir, exist_ok=True)
            playwright.install()

            # Should use just "playwright" when not frozen
            call_args = playwright.call.call_args[0][0]
            # Find the package name in the command (should be just "playwright")
            self.assertIn("playwright", call_args)
            # Make sure it's not versioned by default
            self.assertTrue(any("playwright" in str(arg) and "@" not in str(arg) for arg in call_args if "playwright" in str(arg)))


class TestIncrementalLineReader(BZTestCase):
    def test_partial_buffer_is_merged(self):
        """Lines without trailing newline are buffered and merged with subsequent lines"""
        reader = IncrementalLineReader(logging.getLogger("test"), "/nonexistent.jsonl")
        # First call: "partial" has no newline so it's buffered; " line\n" completes it
        reader.file.get_lines = MagicMock(return_value=iter(["partial", " line\n"]))
        results = list(reader.read())
        self.assertEqual(results, ["partial line\n"])
        self.assertEqual(reader.partial_buffer, "")

    def test_complete_lines_yielded_immediately(self):
        """Lines with trailing newline are yielded without buffering"""
        reader = IncrementalLineReader(logging.getLogger("test"), "/nonexistent.jsonl")
        reader.file.get_lines = MagicMock(return_value=iter(["line1\n", "line2\n"]))
        results = list(reader.read())
        self.assertEqual(results, ["line1\n", "line2\n"])
        self.assertEqual(reader.partial_buffer, "")

    def test_trailing_partial_stays_in_buffer(self):
        """A chunk without newline at end of read stays in buffer for next call"""
        reader = IncrementalLineReader(logging.getLogger("test"), "/nonexistent.jsonl")
        reader.file.get_lines = MagicMock(return_value=iter(["complete\n", "incomplete"]))
        results = list(reader.read())
        self.assertEqual(results, ["complete\n"])
        self.assertEqual(reader.partial_buffer, "incomplete")


class TestPlaywrightLogReader(BZTestCase):
    def _make_reader(self):
        return PlaywrightLogReader("/nonexistent.jsonl", logging.getLogger("test"))

    def test_safe_ms_to_s_converts_value(self):
        reader = self._make_reader()
        self.assertAlmostEqual(reader._safe_ms_to_s(1000), 1.0)
        self.assertAlmostEqual(reader._safe_ms_to_s(500), 0.5)

    def test_safe_ms_to_s_none_returns_none(self):
        reader = self._make_reader()
        self.assertIsNone(reader._safe_ms_to_s(None))

    def test_safe_ms_to_s_zero_returns_zero(self):
        """Zero is falsy so returns as-is (not divided)"""
        reader = self._make_reader()
        self.assertEqual(reader._safe_ms_to_s(0), 0)

    def test_read_parses_jsonl_entries(self):
        reader = self._make_reader()
        entry = {
            "timestamp": 1000, "label": "my test", "concurency": 2,
            "duration": 500, "connectTime": 100, "latency": 50,
            "ok": True, "error": None, "runDetails": "worker1", "byte_count": 128,
        }
        reader.jsonl_reader.read = MagicMock(return_value=iter([json.dumps(entry) + "\n"]))
        results = list(reader._read())
        self.assertEqual(len(results), 1)
        ts, label, concurrency, duration, connect, latency, rc, error, details, bytes_ = results[0]
        self.assertAlmostEqual(ts, 1.0)
        self.assertEqual(label, "my test")
        self.assertEqual(concurrency, 2)
        self.assertAlmostEqual(duration, 0.5)
        self.assertAlmostEqual(connect, 0.1)
        self.assertAlmostEqual(latency, 0.05)
        self.assertEqual(rc, 0)  # 1 - int(True)
        self.assertIsNone(error)
        self.assertEqual(details, "worker1")
        self.assertEqual(bytes_, 128)

    def test_read_failed_entry_has_rc_one(self):
        reader = self._make_reader()
        entry = {"timestamp": 2000, "label": "fail test", "concurency": 1,
                 "duration": 200, "ok": False, "error": "AssertionError", "byte_count": 0}
        reader.jsonl_reader.read = MagicMock(return_value=iter([json.dumps(entry) + "\n"]))
        results = list(reader._read())
        self.assertEqual(results[0][6], 1)  # 1 - int(False) = 1
        self.assertEqual(results[0][7], "AssertionError")

    def test_read_missing_optional_fields_default_to_none(self):
        """connectTime and latency default to None when absent"""
        reader = self._make_reader()
        entry = {"timestamp": 3000, "label": "t", "concurency": 1,
                 "duration": 100, "ok": True, "byte_count": 0}
        reader.jsonl_reader.read = MagicMock(return_value=iter([json.dumps(entry) + "\n"]))
        results = list(reader._read())
        _, _, _, _, connect, latency, _, _, _, _ = results[0]
        self.assertIsNone(connect)
        self.assertIsNone(latency)


class TestNewmanExecutorCmdline(BZTestCase):
    """Tests for NewmanExecutor startup cmdline construction"""

    def _build_executor(self, scenario_cfg, execution_cfg=None):
        obj = NewmanExecutor()
        obj.engine = EngineEmul()
        execution = {"scenario": scenario_cfg}
        if execution_cfg:
            execution.update(execution_cfg)
        obj.execution.merge(execution)
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac
        return obj

    def _capture_startup_cmdline(self, obj):
        captured = []
        mock_proc = MagicMock()
        mock_proc.poll.return_value = 0
        mock_proc.stdout = None
        mock_proc.stderr = None

        def mock_start_subprocess(args, **kwargs):
            captured.extend(args)
            return mock_proc

        obj.engine.start_subprocess = mock_start_subprocess
        obj.startup()
        return captured

    def test_timeout_appended_to_cmdline(self):
        obj = self._build_executor({
            "script": RESOURCES_DIR + "functional/postman.json",
            "timeout": "5s",
        })
        cmdline = self._capture_startup_cmdline(obj)
        self.assertIn("--timeout-request", cmdline)
        idx = cmdline.index("--timeout-request")
        self.assertEqual(cmdline[idx + 1], "5000")

    def test_think_time_appended_to_cmdline(self):
        obj = self._build_executor({
            "script": RESOURCES_DIR + "functional/postman.json",
            "think-time": "2s",
        })
        cmdline = self._capture_startup_cmdline(obj)
        self.assertIn("--delay-request", cmdline)
        idx = cmdline.index("--delay-request")
        self.assertEqual(cmdline[idx + 1], "2000")

    def test_iterations_appended_to_cmdline(self):
        obj = self._build_executor(
            {"script": RESOURCES_DIR + "functional/postman.json"},
            {"iterations": 7},
        )
        cmdline = self._capture_startup_cmdline(obj)
        self.assertIn("--iteration-count", cmdline)
        idx = cmdline.index("--iteration-count")
        self.assertEqual(cmdline[idx + 1], "7")


class TestNewmanDumpVars(BZTestCase):
    def _make_executor(self, scenario_cfg):
        obj = NewmanExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": scenario_cfg})
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac
        return obj

    def test_dump_vars_string_value(self):
        """When globals is a file path string it is passed directly as a flag value"""
        obj = self._make_executor({
            "script": RESOURCES_DIR + "functional/postman.json",
            "globals": "/path/to/globals.json",
        })
        result = obj._dump_vars("globals")
        self.assertEqual(result, ["--globals", "/path/to/globals.json"])

    def test_dump_vars_list_value_written_to_artifact(self):
        """When globals is a list it is serialised to a JSON artifact file"""
        values = [{"key": "env", "value": "prod", "type": "string", "enabled": True}]
        obj = self._make_executor({
            "script": RESOURCES_DIR + "functional/postman.json",
            "globals": values,
        })
        result = obj._dump_vars("globals")
        self.assertEqual(result[0], "--globals")
        artifact_path = result[1]
        self.assertTrue(os.path.exists(artifact_path))
        with open(artifact_path) as fh:
            data = json.load(fh)
        self.assertEqual(data["values"], values)

    def test_dump_vars_empty_dict_writes_artifact(self):
        """An empty mapping still produces an artifact (not a string passthrough)"""
        obj = self._make_executor({
            "script": RESOURCES_DIR + "functional/postman.json",
        })
        result = obj._dump_vars("globals")  # globals key absent → BetterDict returns {}
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], "--globals")
        self.assertTrue(os.path.exists(result[1]))


class TestNPMCheckIfInstalled(BZTestCase):
    def test_npm_returns_false_when_all_candidates_fail(self):
        npm = NPM()
        npm.call = MagicMock(side_effect=OSError("not found"))
        result = npm.check_if_installed()
        self.assertFalse(result)

    def test_npm_appends_err_to_out(self):
        """When call returns stderr it is appended to stdout for logging"""
        npm = NPM()
        npm.call = MagicMock(return_value=("6.14.0", "some warning"))
        result = npm.check_if_installed()
        self.assertTrue(result)

    def test_module_package_uses_esm_import_syntax(self):
        """NPMModulePackage builds an ESM import command, not a require() command"""

        class DummyESM(NPMModulePackage):
            PACKAGE_NAME = "tsx@4.19.2"

        mock_node = MagicMock()
        mock_node.tool_path = "node"
        pkg = DummyESM(tools_dir="/tmp/tools", node_tool=mock_node, npm_tool=MagicMock())
        ok_msg = "%s is installed" % pkg.package_name
        pkg.call = MagicMock(return_value=(ok_msg, ""))
        result = pkg.check_if_installed()
        self.assertTrue(result)
        call_args = pkg.call.call_args[0][0]
        self.assertIn("--input-type=module", call_args)
        self.assertNotIn("require(", " ".join(call_args))

    def test_module_package_check_returns_false_on_exception(self):
        class DummyESM(NPMModulePackage):
            PACKAGE_NAME = "tsx@4.19.2"

        mock_node = MagicMock()
        mock_node.tool_path = "node"
        pkg = DummyESM(tools_dir="/tmp/tools", node_tool=mock_node, npm_tool=MagicMock())
        pkg.call = MagicMock(side_effect=OSError("node not found"))
        self.assertFalse(pkg.check_if_installed())


class TestNPMPackageInstall(BZTestCase):
    def _make_package(self, name):
        class Pkg(NPMPackage):
            PACKAGE_NAME = name

        mock_npm = MagicMock()
        mock_npm.tool_path = "npm"
        return Pkg(tools_dir="/tmp/tools", node_tool=MagicMock(), npm_tool=mock_npm)

    def test_install_appends_version_to_package_name(self):
        pkg = self._make_package("some-package@2.0.0")
        pkg.call = MagicMock(return_value=("", ""))
        pkg.install()
        call_args = pkg.call.call_args[0][0]
        self.assertIn("some-package@2.0.0", call_args)

    def test_install_handles_call_exception_without_raising(self):
        pkg = self._make_package("some-package")
        pkg.call = MagicMock(side_effect=OSError("npm crashed"))
        pkg.install()  # must not raise

    def test_install_logs_stderr_as_warning(self):
        pkg = self._make_package("some-package")
        pkg.call = MagicMock(return_value=("done", "some warning"))
        with self.assertLogs(level="WARNING") as cm:
            pkg.install()
        self.assertTrue(any("some warning" in msg for msg in cm.output))
