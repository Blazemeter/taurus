import json
import os
import time
from os.path import exists

import bzt

from bzt import ToolError
from bzt.modules.javascript import NPMPackage, JavaScriptExecutor, NewmanExecutor, Mocha, JSSeleniumWebdriver, \
    PlaywrightTester, PLAYWRIGHT, PlaywrightTestPackage, PlaywrightTypesNodePackage, PlaywrightCustomReporter, \
    PlaywrightBinLink, NPMModuleInstaller, PlaywrightLogReader, OFFLINE_INSTALL_ARGS, _link_frozen_path, \
    _is_linked_to_frozen_path, \
    _pin_package_json_dependency, _read_frozen_installed_version
from bzt.utils import get_full_path, EXE_SUFFIX

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
        self.assertEqual(samples[0][6], "")
        self.assertEqual(samples[1][1], "reserve flight")
        self.assertEqual(samples[1][6], "")

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

    def test_playwright_granularity_env_default(self):
        """Test TAURUS_PWREPORT_GRANULARITY defaults to AUTO"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('AUTO', self.ENV.get("TAURUS_PWREPORT_GRANULARITY"))

    def test_playwright_noreport_prefix_env_default(self):
        """Test TAURUS_PWREPORT_NOREPORT_PREFIX defaults to empty string"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('', self.ENV.get("TAURUS_PWREPORT_NOREPORT_PREFIX"))

    def test_playwright_granularity_env_from_scenario(self):
        """Test report-granularity scenario option is translated into TAURUS_PWREPORT_GRANULARITY"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    "report-granularity": "step-leaf",
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('STEP_LEAF', self.ENV.get("TAURUS_PWREPORT_GRANULARITY"))

    def test_playwright_granularity_env_explicit_auto(self):
        """Test report-granularity: auto is translated into TAURUS_PWREPORT_GRANULARITY=AUTO"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    "report-granularity": "auto",
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('AUTO', self.ENV.get("TAURUS_PWREPORT_GRANULARITY"))

    def test_playwright_granularity_unknown_value_logs_warning(self):
        """Test an unrecognized report-granularity value is passed through with a warning"""
        self.prepare({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    "report-granularity": "bogus",
                },
                'executor': 'playwright',
            },
        })
        self.sniff_log(self.obj.runner.log)
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

        self.assertEqual('BOGUS', self.ENV.get("TAURUS_PWREPORT_GRANULARITY"))
        self.assertTrue(any("Unknown report-granularity" in msg
                             for msg in self.log_recorder.warn_buff.getvalue().split('\n')))
        self.assertIn("default to AUTO", self.log_recorder.warn_buff.getvalue())

    def test_playwright_noreport_prefix_env_from_scenario(self):
        """Test report-exclude-prefix scenario option is passed through to TAURUS_PWREPORT_NOREPORT_PREFIX"""
        self.simple_run({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": RESOURCES_DIR + "playwright",
                    "report-exclude-prefix": "NOREPORT:",
                },
                'executor': 'playwright',
            },
        })
        self.assertEqual('NOREPORT:', self.ENV.get("TAURUS_PWREPORT_NOREPORT_PREFIX"))

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

    def test_playwright_exit_code1_with_tests_ran(self):
        """Exit code 1 should not raise when tests ran (some failed)."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        mock_process = MagicMock()
        mock_process.poll.return_value = 1
        self.obj.runner.process = mock_process
        self.obj.runner._tests_ran = MagicMock(return_value=True)

        self.assertTrue(self.obj.runner.check())

    def test_playwright_exit_code1_without_tests_ran(self):
        """Exit code 1 should raise ToolError when no tests ran (config error)."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        mock_process = MagicMock()
        mock_process.poll.return_value = 1
        self.obj.runner.process = mock_process
        self.obj.runner._tests_ran = MagicMock(return_value=False)

        with self.assertRaises(ToolError):
            self.obj.runner.check()

    def test_playwright_crash_exit_code_raises(self):
        """Non-1 non-zero exit (e.g. 134 OOM) should raise ToolError even if tests ran."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        mock_process = MagicMock()
        mock_process.poll.return_value = 134
        self.obj.runner.process = mock_process
        self.obj.runner._tests_ran = MagicMock(return_value=True)

        with self.assertRaises(ToolError):
            self.obj.runner.check()

    def test_tests_ran_file_exists_with_content(self):
        """_tests_ran returns True when result file exists and is non-empty."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        filename = self.obj.runner.reader.filename
        with patch('bzt.modules.javascript.os.path.exists', return_value=True) as mock_exists, \
                patch('bzt.modules.javascript.os.path.getsize', return_value=42) as mock_getsize:
            self.assertTrue(self.obj.runner._tests_ran())
            mock_exists.assert_called_once_with(filename)
            mock_getsize.assert_called_once_with(filename)

    def test_tests_ran_file_exists_empty(self):
        """_tests_ran returns False when result file exists but is empty."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        with patch('bzt.modules.javascript.os.path.exists', return_value=True), \
                patch('bzt.modules.javascript.os.path.getsize', return_value=0):
            self.assertFalse(self.obj.runner._tests_ran())

    def test_tests_ran_file_absent(self):
        """_tests_ran returns False when result file does not exist."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        with patch('bzt.modules.javascript.os.path.exists', return_value=False):
            self.assertFalse(self.obj.runner._tests_ran())

    def test_tests_ran_no_reader(self):
        """_tests_ran returns False when reader is None."""
        self.prepare({
            'execution': {
                "executor": "playwright",
                "iterations": 1,
                "scenario": {"script": RESOURCES_DIR + "playwright"}
            }
        })
        self.obj.runner.reader = None
        self.assertFalse(self.obj.runner._tests_ran())


class TestPlaywrightLogReaderStripAnsi(BZTestCase):
    def setUp(self):
        super().setUp()
        self.reader = PlaywrightLogReader("nonexistent.jsonl", self.log)

    def test_strip_ansi_empty_input_returned_unchanged(self):
        self.assertIsNone(self.reader._strip_ansi(None))
        self.assertEqual("", self.reader._strip_ansi(""))

    def test_strip_ansi_removes_ansi_escapes(self):
        colored = "\x1b[31mfail\x1b[0m at \x1b[1;33mline 5\x1b[0m"
        self.assertEqual("fail at line 5", self.reader._strip_ansi(colored))


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
        """Test that Playwright install is skipped when frozen version is already installed"""
        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        # `npx --no -- playwright --version` reports the installed version, matching the frozen one
        playwright.call = MagicMock(return_value=("Version 1.40.0\n", ""))

        with patch.dict(os.environ, {'PLAYWRIGHT_PACKAGE_FORCED_VERSION': '1.40.0'}):
            playwright.install()

            # Should call npx --no -- playwright --version once, against the frozen
            # build-time location (~/.bzt/playwright), not tools_dir.
            playwright.call.assert_called_once_with(
                ["npx", "--no", "--", "playwright", "--version"],
                cwd=get_full_path("~/.bzt/playwright"),
            )

    @patch('bzt.modules.javascript.is_linux')
    def test_playwright_install_frozen_version_changed(self, mock_is_linux):
        """Test that Playwright re-installs when installed version differs from frozen version"""
        mock_is_linux.return_value = False

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        os.makedirs(self.tools_dir, exist_ok=True)

        # Probe reports a different (old) version — frozen version mismatch
        playwright.call = MagicMock(side_effect=[("Version 1.39.0\n", ""), ("", "")])

        with patch.dict(os.environ, {'PLAYWRIGHT_PACKAGE_FORCED_VERSION': '1.40.0'}):
            playwright.install()

            # First call: version probe, run against the frozen build-time location
            first_call_args = playwright.call.call_args_list[0][0][0]
            self.assertEqual(first_call_args, ["npx", "--no", "--", "playwright", "--version"])
            self.assertEqual(playwright.call.call_args_list[0][1].get('cwd'), get_full_path("~/.bzt/playwright"))

            # Second call: npx playwright@1.40.0 install --with-deps
            self.assertEqual(playwright.call.call_count, 2)
            second_call_args = playwright.call.call_args_list[1][0][0]
            self.assertIn("npx", second_call_args)
            self.assertIn("playwright@1.40.0", second_call_args)
            self.assertIn("install", second_call_args)
            self.assertIn("--with-deps", second_call_args)

    @patch('bzt.modules.javascript.is_linux')
    def test_playwright_install_frozen_version_probe_oserror(self, mock_is_linux):
        """Test that Playwright re-installs when the version probe raises OSError"""
        mock_is_linux.return_value = False

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        os.makedirs(self.tools_dir, exist_ok=True)

        # First call (version probe) raises OSError; second call is the actual install
        playwright.call = MagicMock(side_effect=[OSError("npx probe failed"), ("", "")])

        with patch.dict(os.environ, {'PLAYWRIGHT_PACKAGE_FORCED_VERSION': '1.40.0'}):
            playwright.install()

            self.assertEqual(playwright.call.call_count, 2)
            first_call_args = playwright.call.call_args_list[0][0][0]
            self.assertEqual(first_call_args, ["npx", "--no", "--", "playwright", "--version"])

            second_call_args = playwright.call.call_args_list[1][0][0]
            self.assertIn("npx", second_call_args)
            self.assertIn("playwright@1.40.0", second_call_args)
            self.assertIn("install", second_call_args)
            self.assertIn("--with-deps", second_call_args)

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


class TestFrozenPackageLinkHelpers(BZTestCase):
    """Tests for the module-level symlink/pin helpers shared by all frozen packages"""

    def setUp(self):
        super(TestFrozenPackageLinkHelpers, self).setUp()
        import tempfile
        self.frozen_store = tempfile.mkdtemp()
        self.tools_dir = tempfile.mkdtemp()
        self.get_full_path_patcher = patch('bzt.modules.javascript.get_full_path', return_value=self.frozen_store)
        self.get_full_path_patcher.start()
        self.addCleanup(self.get_full_path_patcher.stop)

    def _make_frozen_package(self, relative_parts, contents=b"module.exports = {};"):
        path = os.path.join(self.frozen_store, "node_modules", *relative_parts)
        os.makedirs(path, exist_ok=True)
        with open(os.path.join(path, "index.js"), "wb") as fds:
            fds.write(contents)
        return path

    def test_link_frozen_path_creates_symlink(self):
        source = self._make_frozen_package(("@types", "node"))
        _link_frozen_path(self.tools_dir, ("@types", "node"))

        target = os.path.join(self.tools_dir, "node_modules", "@types", "node")
        self.assertTrue(os.path.islink(target))
        self.assertEqual(os.path.realpath(target), os.path.realpath(source))

    def test_link_frozen_path_replaces_existing_real_directory(self):
        self._make_frozen_package(("@types", "node"))
        stale = os.path.join(self.tools_dir, "node_modules", "@types", "node")
        os.makedirs(stale, exist_ok=True)
        with open(os.path.join(stale, "stale.txt"), "w") as fds:
            fds.write("stale customer-declared copy")

        _link_frozen_path(self.tools_dir, ("@types", "node"))

        self.assertTrue(os.path.islink(stale))

    def test_link_frozen_path_replaces_existing_symlink(self):
        self._make_frozen_package(("@types", "node"))
        target = os.path.join(self.tools_dir, "node_modules", "@types", "node")
        os.makedirs(os.path.dirname(target), exist_ok=True)
        os.symlink("/nonexistent", target)

        _link_frozen_path(self.tools_dir, ("@types", "node"))

        self.assertTrue(os.path.exists(os.path.join(target, "index.js")))

    def test_is_linked_to_frozen_path_true_after_linking(self):
        self._make_frozen_package(("@playwright", "test"))
        _link_frozen_path(self.tools_dir, ("@playwright", "test"))

        self.assertTrue(_is_linked_to_frozen_path(self.tools_dir, ("@playwright", "test")))

    def test_is_linked_to_frozen_path_false_when_missing(self):
        self._make_frozen_package(("@playwright", "test"))
        self.assertFalse(_is_linked_to_frozen_path(self.tools_dir, ("@playwright", "test")))

    def test_is_linked_to_frozen_path_false_for_unrelated_symlink(self):
        target = os.path.join(self.tools_dir, "node_modules", "@playwright", "test")
        os.makedirs(os.path.dirname(target), exist_ok=True)
        os.symlink("/somewhere/else", target)

        self.assertFalse(_is_linked_to_frozen_path(self.tools_dir, ("@playwright", "test")))

    def test_pin_package_json_dependency_updates_existing_devDependencies_entry(self):
        pkg_json = os.path.join(self.tools_dir, "package.json")
        with open(pkg_json, "w") as fds:
            json.dump({"devDependencies": {"@types/node": "^22.15.21"}}, fds)

        _pin_package_json_dependency(self.tools_dir, "@types/node", "26.5.0")

        with open(pkg_json) as fds:
            data = json.load(fds)
        self.assertEqual(data["devDependencies"]["@types/node"], "^26.5.0")

    def test_pin_package_json_dependency_updates_existing_dependencies_entry(self):
        pkg_json = os.path.join(self.tools_dir, "package.json")
        with open(pkg_json, "w") as fds:
            json.dump({"dependencies": {"@playwright/test": "^1.58.1"}}, fds)

        _pin_package_json_dependency(self.tools_dir, "@playwright/test", "1.63.0")

        with open(pkg_json) as fds:
            data = json.load(fds)
        self.assertEqual(data["dependencies"]["@playwright/test"], "^1.63.0")
        self.assertNotIn("@playwright/test", data.get("devDependencies", {}))

    def test_pin_package_json_dependency_adds_missing_entry(self):
        pkg_json = os.path.join(self.tools_dir, "package.json")
        with open(pkg_json, "w") as fds:
            json.dump({"devDependencies": {"@playwright/test": "^1.58.1"}}, fds)

        _pin_package_json_dependency(self.tools_dir, "@types/node", "26.5.0")

        with open(pkg_json) as fds:
            data = json.load(fds)
        self.assertEqual(data["devDependencies"]["@types/node"], "^26.5.0")

    def test_read_frozen_installed_version_reads_actual_version(self):
        path = self._make_frozen_package(("@types", "node"))
        with open(os.path.join(path, "package.json"), "w") as fds:
            json.dump({"version": "26.5.0"}, fds)

        self.assertEqual(_read_frozen_installed_version(("@types", "node")), "26.5.0")

    def test_read_frozen_installed_version_missing_returns_none(self):
        self.assertIsNone(_read_frozen_installed_version(("@types", "node")))


class TestPlaywrightTestPackageInstallation(BZTestCase):
    """Tests for PlaywrightTestPackage: symlink-based when frozen, normal npm install otherwise"""

    def setUp(self):
        super(TestPlaywrightTestPackageInstallation, self).setUp()
        import tempfile
        self.node_mock = MagicMock()
        self.node_mock.tool_path = "node"
        self.npm_mock = MagicMock()
        self.npm_mock.tool_path = "npm"
        self.frozen_store = tempfile.mkdtemp()
        self.tools_dir = tempfile.mkdtemp()
        self.get_full_path_patcher = patch('bzt.modules.javascript.get_full_path', return_value=self.frozen_store)
        self.get_full_path_patcher.start()
        self.addCleanup(self.get_full_path_patcher.stop)

    def _create_package(self):
        return PlaywrightTestPackage(
            tools_dir=self.tools_dir,
            node_tool=self.node_mock,
            npm_tool=self.npm_mock,
        )

    def _freeze_playwright_test(self, version="1.63.0"):
        path = os.path.join(self.frozen_store, "node_modules", "@playwright", "test")
        os.makedirs(path, exist_ok=True)
        with open(os.path.join(path, "package.json"), "w") as fds:
            json.dump({"version": version}, fds)
        return version

    def test_check_if_installed_not_frozen_delegates_to_super(self):
        """Without a forced version, behaves like a plain NPMPackage (require() check only)"""
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("@playwright/test is installed", ""))

        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            result = pkg.check_if_installed()

        self.assertTrue(result)
        pkg.call.assert_called_once()

    def test_install_not_frozen_delegates_to_super(self):
        """Without a forced version, install() runs a normal npm install"""
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            pkg.install()

        cmdline = pkg.call.call_args[0][0]
        self.assertEqual(cmdline, ["npm", "install", "@playwright/test", "--prefix", self.tools_dir])

    def test_install_frozen_links_package(self):
        version = self._freeze_playwright_test()
        pkg = self._create_package()
        pkg.call = MagicMock()

        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            pkg.install()

        pkg_target = os.path.join(self.tools_dir, "node_modules", "@playwright", "test")
        self.assertTrue(os.path.islink(pkg_target))
        pkg.call.assert_not_called()

    def test_install_frozen_pins_package_json(self):
        version = self._freeze_playwright_test(version="1.63.0")
        pkg = self._create_package()
        with open(os.path.join(self.tools_dir, "package.json"), "w") as fds:
            json.dump({"devDependencies": {"@playwright/test": "^1.58.1"}}, fds)

        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            pkg.install()

        with open(os.path.join(self.tools_dir, "package.json")) as fds:
            data = json.load(fds)
        self.assertEqual(data["devDependencies"]["@playwright/test"], "^1.63.0")

    def test_check_if_installed_frozen_true_after_install(self):
        version = self._freeze_playwright_test()
        pkg = self._create_package()
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            pkg.install()
            self.assertTrue(pkg.check_if_installed())

    def test_check_if_installed_frozen_false_before_install(self):
        version = self._freeze_playwright_test()
        pkg = self._create_package()
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            self.assertFalse(pkg.check_if_installed())


class TestPlaywrightBinLink(BZTestCase):
    """
    Tests for PlaywrightBinLink: reasserts node_modules/.bin/playwright -> @playwright/test's
    cli.js as the last install step, since NPMModuleInstaller may have just overwritten it
    while reifying a customer-declared top-level "playwright" dependency of their own (same
    bin name, different package).
    """

    def setUp(self):
        super(TestPlaywrightBinLink, self).setUp()
        import tempfile
        self.frozen_store = tempfile.mkdtemp()
        self.tools_dir = tempfile.mkdtemp()
        self.get_full_path_patcher = patch('bzt.modules.javascript.get_full_path', return_value=self.frozen_store)
        self.get_full_path_patcher.start()
        self.addCleanup(self.get_full_path_patcher.stop)

    def _freeze_bin(self):
        bin_dir = os.path.join(self.frozen_store, "node_modules", ".bin")
        os.makedirs(bin_dir, exist_ok=True)
        with open(os.path.join(bin_dir, "playwright"), "w") as fds:
            fds.write("#!/bin/sh\n")

    def test_check_if_installed_not_frozen_always_true(self):
        link = PlaywrightBinLink(tools_dir=self.tools_dir)
        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            self.assertTrue(link.check_if_installed())

    def test_check_if_installed_frozen_false_when_missing(self):
        self._freeze_bin()
        link = PlaywrightBinLink(tools_dir=self.tools_dir)
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.63.0'}):
            self.assertFalse(link.check_if_installed())

    def test_install_creates_correct_link(self):
        self._freeze_bin()
        link = PlaywrightBinLink(tools_dir=self.tools_dir)
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.63.0'}):
            link.install()
            self.assertTrue(link.check_if_installed())

    def test_install_overwrites_link_clobbered_by_another_package(self):
        """Simulates NPMModuleInstaller overwriting .bin/playwright with a different package's cli.js"""
        self._freeze_bin()
        bin_target = os.path.join(self.tools_dir, "node_modules", ".bin", "playwright")
        os.makedirs(os.path.dirname(bin_target), exist_ok=True)
        os.symlink("../playwright/cli.js", bin_target)

        link = PlaywrightBinLink(tools_dir=self.tools_dir)
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.63.0'}):
            link.install()

        self.assertEqual(
            os.path.realpath(bin_target),
            os.path.realpath(os.path.join(self.frozen_store, "node_modules", ".bin", "playwright")),
        )


class TestPlaywrightTypesNodePackage(BZTestCase):
    """Tests for PlaywrightTypesNodePackage: symlink-based when frozen, normal npm install otherwise"""

    def setUp(self):
        super(TestPlaywrightTypesNodePackage, self).setUp()
        import tempfile
        self.node_mock = MagicMock()
        self.node_mock.tool_path = "node"
        self.npm_mock = MagicMock()
        self.npm_mock.tool_path = "npm"
        self.frozen_store = tempfile.mkdtemp()
        self.tools_dir = tempfile.mkdtemp()
        self.get_full_path_patcher = patch('bzt.modules.javascript.get_full_path', return_value=self.frozen_store)
        self.get_full_path_patcher.start()
        self.addCleanup(self.get_full_path_patcher.stop)

    def _create_package(self):
        return PlaywrightTypesNodePackage(
            tools_dir=self.tools_dir,
            node_tool=self.node_mock,
            npm_tool=self.npm_mock,
        )

    def _freeze_types_node(self, version="26.5.0"):
        path = os.path.join(self.frozen_store, "node_modules", "@types", "node")
        os.makedirs(path, exist_ok=True)
        with open(os.path.join(path, "package.json"), "w") as fds:
            json.dump({"version": version}, fds)
        return version

    def test_check_if_installed_not_frozen_delegates_to_super(self):
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            # @types/node has no requirable entry point at all - always False, as expected
            result = pkg.check_if_installed()

        self.assertFalse(result)

    def test_install_not_frozen_delegates_to_super(self):
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            pkg.install()

        cmdline = pkg.call.call_args[0][0]
        self.assertEqual(cmdline, ["npm", "install", "@types/node", "--prefix", self.tools_dir])

    def test_install_frozen_links_package(self):
        version = self._freeze_types_node()
        pkg = self._create_package()
        pkg.call = MagicMock()

        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            pkg.install()

        target = os.path.join(self.tools_dir, "node_modules", "@types", "node")
        self.assertTrue(os.path.islink(target))
        pkg.call.assert_not_called()

    def test_check_if_installed_frozen_true_after_install(self):
        version = self._freeze_types_node()
        pkg = self._create_package()
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            pkg.install()
            self.assertTrue(pkg.check_if_installed())

    def test_check_if_installed_frozen_false_before_install(self):
        version = self._freeze_types_node()
        pkg = self._create_package()
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': version}):
            self.assertFalse(pkg.check_if_installed())


class TestPlaywrightCustomReporterInstallation(BZTestCase):
    """
    Tests for PlaywrightCustomReporter: local (registry-free) npm install when not frozen,
    linked from the frozen store when frozen - so an unrelated uncached customer dependency
    sharing the same tools_dir can no longer collaterally block it.
    """

    def setUp(self):
        super(TestPlaywrightCustomReporterInstallation, self).setUp()
        import tempfile
        self.node_mock = MagicMock()
        self.node_mock.tool_path = "node"
        self.npm_mock = MagicMock()
        self.npm_mock.tool_path = "npm"
        self.frozen_store = tempfile.mkdtemp()
        self.tools_dir = tempfile.mkdtemp()
        self.get_full_path_patcher = patch('bzt.modules.javascript.get_full_path', return_value=self.frozen_store)
        self.get_full_path_patcher.start()
        self.addCleanup(self.get_full_path_patcher.stop)

    def _create_package(self):
        return PlaywrightCustomReporter(
            tools_dir=self.tools_dir,
            node_tool=self.node_mock,
            npm_tool=self.npm_mock,
        )

    def _freeze_reporter(self):
        path = os.path.join(self.frozen_store, "node_modules", "@taurus", "playwright-custom-reporter")
        os.makedirs(path, exist_ok=True)
        with open(os.path.join(path, "index.js"), "w") as fds:
            fds.write("module.exports = {};")

    def test_check_if_installed_not_frozen_always_false(self):
        """Not frozen: always reinstall - npm version resolving for local modules is not reliable"""
        pkg = self._create_package()
        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            self.assertFalse(pkg.check_if_installed())

    def test_install_not_frozen_runs_local_npm_install(self):
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("", ""))

        with patch.dict(os.environ, {}, clear=False):
            os.environ.pop('PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION', None)
            pkg.install()

        args, kwargs = pkg.call.call_args
        self.assertEqual(args[0], ["npm", "install", ".", "--install-links", "--prefix", self.tools_dir, "--offline"])
        self.assertEqual(kwargs.get("cwd"), pkg.package_local_path)

    def test_install_frozen_links_reporter(self):
        self._freeze_reporter()
        pkg = self._create_package()
        pkg.call = MagicMock()

        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.63.0'}):
            pkg.install()

        target = os.path.join(self.tools_dir, "node_modules", "@taurus", "playwright-custom-reporter")
        self.assertTrue(os.path.islink(target))
        pkg.call.assert_not_called()

    def test_check_if_installed_frozen_true_after_install(self):
        self._freeze_reporter()
        pkg = self._create_package()
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.63.0'}):
            pkg.install()
            self.assertTrue(pkg.check_if_installed())

    def test_check_if_installed_frozen_false_before_install(self):
        self._freeze_reporter()
        pkg = self._create_package()
        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.63.0'}):
            self.assertFalse(pkg.check_if_installed())


class TestNPMModuleInstallerInstallation(BZTestCase):
    """Tests for NPMModuleInstaller (customer's own arbitrary deps): offline-first, then --prefer-offline"""

    def setUp(self):
        super(TestNPMModuleInstallerInstallation, self).setUp()
        self.node_mock = MagicMock()
        self.node_mock.tool_path = "node"
        self.npm_mock = MagicMock()
        self.npm_mock.tool_path = "npm"
        self.tools_dir = "/tmp/customer-tools-dir"

    def _create_installer(self):
        return NPMModuleInstaller(
            tools_dir=self.tools_dir,
            node_tool=self.node_mock,
            npm_tool=self.npm_mock,
        )

    def test_package_local_path_is_tools_dir(self):
        installer = self._create_installer()
        self.assertEqual(installer.package_local_path, self.tools_dir)

    def test_install_offline_succeeds(self):
        installer = self._create_installer()
        installer.call = MagicMock(return_value=("added 3 packages", ""))

        installer.install()

        installer.call.assert_called_once()
        cmdline = installer.call.call_args[0][0]
        self.assertIn("--offline", cmdline)
        self.assertNotIn("--prefer-offline", cmdline)

    def test_install_offline_fails_prefer_offline_succeeds(self):
        installer = self._create_installer()
        installer.call = MagicMock(side_effect=[
            OSError("ENOTCACHED"),
            ("added 3 packages", ""),
        ])

        installer.install()

        self.assertEqual(installer.call.call_count, 2)
        first_cmdline = installer.call.call_args_list[0][0][0]
        second_cmdline = installer.call.call_args_list[1][0][0]
        self.assertIn("--offline", first_cmdline)
        self.assertIn("--prefer-offline", second_cmdline)

    def test_install_both_offline_attempts_fail(self):
        installer = self._create_installer()
        installer.call = MagicMock(side_effect=[
            OSError("ENOTCACHED"),
            OSError("ECONNREFUSED"),
        ])

        installer.install()  # must not raise

        self.assertEqual(installer.call.call_count, len(OFFLINE_INSTALL_ARGS))
