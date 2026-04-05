import json
import os
import tempfile
import time
from os.path import exists

import bzt

from bzt import ToolError
from bzt.modules.javascript import NPMPackage, JavaScriptExecutor, NewmanExecutor, Mocha, JSSeleniumWebdriver, \
    PlaywrightTester, PLAYWRIGHT, PlaywrightTestPackage, IncrementalLineReader, PlaywrightLogReader
from bzt.utils import get_full_path, EXE_SUFFIX

from tests.unit import RESOURCES_DIR, BZTestCase, EngineEmul, ROOT_LOGGER
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

    def _prepared_obj(self):
        self.obj = NewmanExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.config.merge({"execution": {"scenario": {
            "script": RESOURCES_DIR + 'functional/postman.json',
        }}})
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + 'functional/postman.json'}})
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

    def test_dump_vars_string_passes_path_directly(self):
        self._prepared_obj()
        self.obj.get_scenario().data["globals"] = "/path/to/globals.json"
        cmdline = self.obj._dump_vars("globals")
        self.assertEqual(cmdline, ["--globals", "/path/to/globals.json"])

    def test_dump_vars_list_writes_json_file(self):
        self._prepared_obj()
        self.obj.get_scenario().data["globals"] = [{"key": "host", "value": "localhost"}]
        cmdline = self.obj._dump_vars("globals")
        self.assertEqual(cmdline[0], "--globals")
        with open(cmdline[1]) as f:
            data = json.load(f)
        self.assertEqual(data["values"], [{"key": "host", "value": "localhost"}])

    def test_dump_vars_empty_dict_writes_json_file(self):
        self._prepared_obj()
        self.obj.get_scenario().data["environment"] = {}
        cmdline = self.obj._dump_vars("environment")
        self.assertEqual(cmdline[0], "--environment")
        with open(cmdline[1]) as f:
            data = json.load(f)
        self.assertIn("values", data)


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
        self.assertEqual(samples[0][6], 0)
        self.assertEqual(samples[1][1], "reserve flight")
        self.assertEqual(samples[1][6], 0)

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

    def test_duration_without_iterations_repeat_each_is_1000(self):
        """When duration is set but iterations are not, repeat-each should be the safe default 1000."""
        self.simple_run({
            'execution': {
                'hold-for': '60s',
                'scenario': {"script": RESOURCES_DIR + "playwright"},
                'executor': 'playwright',
            },
        })
        self.assertIn("--repeat-each 1000", self.CMD_LINE)

    def test_no_duration_no_iterations_repeat_each_is_1(self):
        """With neither duration nor iterations set, concurrency=1, so repeat-each = 1*1 = 1."""
        self.simple_run({
            'execution': {
                'scenario': {"script": RESOURCES_DIR + "playwright"},
                'executor': 'playwright',
            },
        })
        self.assertIn("--repeat-each 1", self.CMD_LINE)
        self.assertIn("--workers 1", self.CMD_LINE)

    def test_duration_with_iterations_repeat_each_multiplied(self):
        """With duration + iterations + concurrency, repeat-each = concurrency * iterations."""
        self.simple_run({
            'execution': {
                'hold-for': '30s',
                'iterations': 4,
                'concurrency': 3,
                'scenario': {"script": RESOURCES_DIR + "playwright"},
                'executor': 'playwright',
            },
        })
        self.assertIn("--repeat-each 12", self.CMD_LINE)  # 3 * 4


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

    def _make_reader(self, content):
        tmp = tempfile.NamedTemporaryFile(mode='w', suffix='.jsonl', delete=False)
        tmp.write(content)
        tmp.close()
        self.addCleanup(os.unlink, tmp.name)
        return IncrementalLineReader(ROOT_LOGGER, tmp.name)

    def test_complete_lines_yielded(self):
        reader = self._make_reader('{"a":1}\n{"b":2}\n')
        lines = list(reader.read())
        self.assertEqual(len(lines), 2)
        self.assertEqual(lines[0], '{"a":1}\n')
        self.assertEqual(lines[1], '{"b":2}\n')

    def test_partial_line_buffered_not_yielded(self):
        reader = self._make_reader('{"a":1}')
        lines = list(reader.read())
        self.assertEqual(lines, [])
        self.assertEqual(reader.partial_buffer, '{"a":1}')

    def test_partial_buffer_prepends_to_next_complete_line(self):
        tmp = tempfile.NamedTemporaryFile(mode='w', suffix='.jsonl', delete=False)
        tmp.write('{"a"')
        tmp.flush()
        self.addCleanup(os.unlink, tmp.name)
        reader = IncrementalLineReader(ROOT_LOGGER, tmp.name)

        list(reader.read())  # partial — nothing yielded
        self.assertEqual(reader.partial_buffer, '{"a"')

        tmp.write(':1}\n')
        tmp.close()
        lines = list(reader.read())
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0], '{"a":1}\n')
        self.assertEqual(reader.partial_buffer, '')

    def test_empty_file_yields_nothing(self):
        reader = self._make_reader('')
        self.assertEqual(list(reader.read()), [])

    def test_multiple_reads_continue_from_last_position(self):
        tmp = tempfile.NamedTemporaryFile(mode='w', suffix='.jsonl', delete=False)
        tmp.write('line1\n')
        tmp.flush()
        self.addCleanup(os.unlink, tmp.name)
        reader = IncrementalLineReader(ROOT_LOGGER, tmp.name)

        first = list(reader.read())
        self.assertEqual(first, ['line1\n'])

        tmp.write('line2\n')
        tmp.close()
        second = list(reader.read())
        self.assertEqual(second, ['line2\n'])


class TestPlaywrightLogReader(BZTestCase):

    def _write_jsonl(self, records):
        tmp = tempfile.NamedTemporaryFile(mode='w', suffix='.jsonl', delete=False)
        for rec in records:
            tmp.write(json.dumps(rec) + '\n')
        tmp.close()
        self.addCleanup(os.unlink, tmp.name)
        return tmp.name

    def test_safe_ms_to_s_converts(self):
        reader = PlaywrightLogReader("/dev/null", ROOT_LOGGER)
        self.assertAlmostEqual(reader._safe_ms_to_s(2000), 2.0)
        self.assertAlmostEqual(reader._safe_ms_to_s(500), 0.5)

    def test_safe_ms_to_s_none_returns_none(self):
        reader = PlaywrightLogReader("/dev/null", ROOT_LOGGER)
        self.assertIsNone(reader._safe_ms_to_s(None))

    def test_safe_ms_to_s_zero_returns_zero(self):
        reader = PlaywrightLogReader("/dev/null", ROOT_LOGGER)
        # zero is falsy — should be passed through unchanged
        result = reader._safe_ms_to_s(0)
        self.assertEqual(result, 0)

    def test_read_parses_fields_correctly(self):
        record = {
            "timestamp": 1000,
            "label": "homepage load",
            "concurency": 2,
            "duration": 300,
            "connectTime": 20,
            "latency": 80,
            "ok": True,
            "error": None,
            "runDetails": {"worker": 1},
            "byte_count": 1024,
        }
        path = self._write_jsonl([record])
        reader = PlaywrightLogReader(path, ROOT_LOGGER)
        rows = list(reader._read())
        self.assertEqual(len(rows), 1)
        ts, label, concurrency, duration, connect, latency, err_flag, error, details, byte_count = rows[0]
        self.assertAlmostEqual(ts, 1.0)           # 1000ms → 1.0s
        self.assertEqual(label, "homepage load")
        self.assertAlmostEqual(duration, 0.3)     # 300ms → 0.3s
        self.assertAlmostEqual(connect, 0.02)     # 20ms → 0.02s
        self.assertAlmostEqual(latency, 0.08)     # 80ms → 0.08s
        self.assertEqual(err_flag, 0)             # ok=True → 1 - 1 = 0
        self.assertEqual(byte_count, 1024)

    def test_read_error_flag_when_not_ok(self):
        record = {
            "timestamp": 2000,
            "label": "failing test",
            "concurency": 1,
            "duration": 100,
            "ok": False,
            "error": "AssertionError: expected true",
            "byte_count": 0,
        }
        path = self._write_jsonl([record])
        reader = PlaywrightLogReader(path, ROOT_LOGGER)
        rows = list(reader._read())
        err_flag = rows[0][6]
        self.assertEqual(err_flag, 1)             # ok=False → 1 - 0 = 1

    def test_read_multiple_records(self):
        records = [
            {"timestamp": 1000, "label": "t1", "concurency": 1, "duration": 100,
             "ok": True, "byte_count": 0},
            {"timestamp": 2000, "label": "t2", "concurency": 1, "duration": 200,
             "ok": False, "byte_count": 0},
        ]
        path = self._write_jsonl(records)
        reader = PlaywrightLogReader(path, ROOT_LOGGER)
        rows = list(reader._read())
        self.assertEqual(len(rows), 2)
        self.assertEqual(rows[0][1], "t1")
        self.assertEqual(rows[1][1], "t2")
