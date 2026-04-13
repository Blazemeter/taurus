import json
import os
import time
from os.path import exists

import bzt

from bzt import ToolError
from bzt.modules.javascript import NPMPackage, JavaScriptExecutor, NewmanExecutor, Mocha, JSSeleniumWebdriver, \
    PlaywrightTester, PLAYWRIGHT, PlaywrightTestPackage
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
        """Test that Playwright install is skipped when frozen version is already installed"""
        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        # npm list returns output containing the frozen version
        playwright.call = MagicMock(return_value=("playwright@1.40.0 node_modules/playwright", ""))

        with patch.dict(os.environ, {'PLAYWRIGHT_PACKAGE_FORCED_VERSION': '1.40.0'}):
            playwright.install()

            # Should call npm list once to check the installed version
            playwright.call.assert_called_once_with(["npm", "list"])

    @patch('bzt.modules.javascript.is_linux')
    def test_playwright_install_frozen_version_changed(self, mock_is_linux):
        """Test that Playwright re-installs when installed version differs from frozen version"""
        mock_is_linux.return_value = False

        playwright = PLAYWRIGHT(tools_dir=self.tools_dir)
        os.makedirs(self.tools_dir, exist_ok=True)

        # npm list returns a different (old) version — frozen version NOT present
        playwright.call = MagicMock(return_value=("playwright@1.39.0 node_modules/playwright", ""))

        with patch.dict(os.environ, {'PLAYWRIGHT_PACKAGE_FORCED_VERSION': '1.40.0'}):
            playwright.install()

            # First call: npm list version check
            first_call_args = playwright.call.call_args_list[0][0][0]
            self.assertEqual(first_call_args, ["npm", "list"])

            # Second call: npx playwright@1.40.0 install --with-deps
            self.assertEqual(playwright.call.call_count, 2)
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


class TestPlaywrightTestPackageInstallation(BZTestCase):
    """Tests for PlaywrightTestPackage.check_if_installed()"""

    def setUp(self):
        super(TestPlaywrightTestPackageInstallation, self).setUp()
        self.node_mock = MagicMock()
        self.node_mock.tool_path = "node"
        self.npm_mock = MagicMock()
        self.npm_mock.tool_path = "npm"
        self.tools_dir = "~/.bzt/playwright"

    def _create_package(self):
        return PlaywrightTestPackage(
            tools_dir=self.tools_dir,
            node_tool=self.node_mock,
            npm_tool=self.npm_mock,
        )

    def test_check_if_installed_super_returns_false(self):
        """When the parent require() check fails, return False without calling npm list"""
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("", ""))

        result = pkg.check_if_installed()

        self.assertFalse(result)
        pkg.call.assert_called_once()

    def test_check_if_installed_no_forced_version(self):
        """When no forced version is set, any installed version is acceptable and npm list is not called"""
        pkg = self._create_package()
        pkg.call = MagicMock(return_value=("@playwright/test is installed", ""))

        with patch.dict(os.environ, {}, clear=False):
            if 'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION' in os.environ:
                del os.environ['PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION']

            result = pkg.check_if_installed()

        self.assertTrue(result)
        pkg.call.assert_called_once()

    def test_check_if_installed_forced_version_correct(self):
        """When forced version matches the installed version, return True"""
        pkg = self._create_package()
        pkg.call = MagicMock(side_effect=[
            ("@playwright/test is installed", ""),
            ("@playwright/test@1.40.0 node_modules/@playwright/test", ""),
        ])

        with patch.object(PlaywrightTestPackage, 'PACKAGE_NAME', '@playwright/test@1.40.0'):
            with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.40.0'}):
                result = pkg.check_if_installed()

        self.assertTrue(result)
        self.assertEqual(pkg.call.call_count, 2)
        second_call_args = pkg.call.call_args_list[1][0][0]
        self.assertEqual(second_call_args, [self.npm_mock.tool_path, "list"])

    def test_check_if_installed_forced_version_mismatch(self):
        """When forced version is not present in npm list output, return False"""
        pkg = self._create_package()
        pkg.call = MagicMock(side_effect=[
            ("@playwright/test is installed", ""),
            ("@playwright/test@1.39.0 node_modules/@playwright/test", ""),
        ])

        with patch.object(PlaywrightTestPackage, 'PACKAGE_NAME', '@playwright/test@1.40.0'):
            with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.40.0'}):
                result = pkg.check_if_installed()

        self.assertFalse(result)
        self.assertEqual(pkg.call.call_count, 2)

    def test_check_if_installed_npm_list_call_fails(self):
        """When npm list raises an OSError, return False"""
        pkg = self._create_package()
        pkg.call = MagicMock(side_effect=[
            ("@playwright/test is installed", ""),
            OSError("npm list failed"),
        ])

        with patch.dict(os.environ, {'PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION': '1.40.0'}):
            result = pkg.check_if_installed()

        self.assertFalse(result)
