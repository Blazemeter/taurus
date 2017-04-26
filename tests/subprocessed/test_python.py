import time

import os
from bzt import ToolError, TaurusConfigError
from tests import __dir__, BZTestCase

from bzt.engine import ScenarioExecutor
from bzt.modules.functional import FuncSamplesReader
from bzt.modules.python import NoseTester
from tests.mocks import EngineEmul
from tests.subprocessed import SeleniumTestCase


class TestSeleniumNoseRunner(SeleniumTestCase):
    def test_selenium_prepare_python_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/python/test_blazemeter_fail.py"
        }})
        self.obj.prepare()

    def test_selenium_prepare_python_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/python/"}})
        self.obj.prepare()

    def test_selenium_startup_shutdown_python_single(self):
        """
        run tests from .py file
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/python/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/python/test_blazemeter_fail.py"
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))

    def test_selenium_startup_shutdown_python_folder(self):
        """
        run tests from .py files
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/python/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))

    def test_runner_fail_no_test_found(self):
        """
        Check that Python Nose runner fails if no tests were found
        :return:
        """
        self.configure({
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": __dir__() + "/../selenium/invalid/dummy.py"}
            }
        })
        self.obj.prepare()
        self.obj.startup()
        try:
            while not self.obj.check():
                time.sleep(1)
            self.fail()
        except ToolError as exc:
            self.assertIn("Nothing to test.", exc.args[0])
        self.obj.shutdown()

    def test_resource_files_collection_remote_nose(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/python/"}})
        self.assertEqual(len(self.obj.resource_files()), 1)

    def test_setup_exception(self):
        """
        Do not crash when test's setUp/setUpClass fails
        :return:
        """
        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/python/test_setup_exception.py"
        }})
        self.obj.prepare()
        self.obj.startup()
        while True:
            try:
                finished = self.obj.check()
                if finished:
                    self.fail("Should've failed with 'nothing to test'")
            except ToolError as exc:
                self.assertIn("Catch that", str(exc))
                self.assertIn("Nothing to test", str(exc))
                break

    def test_long_iterations_value(self):
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
                time.sleep(1.0)
        finally:
            self.obj.shutdown()


class TestNoseRunner(BZTestCase):
    def setUp(self):
        super(TestNoseRunner, self).setUp()
        self.obj = NoseTester()
        self.obj.engine = EngineEmul()

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config["execution"][0]

    def test_full_single_script(self):
        self.obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../apiritif/test_api_example.py"
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
        self.assertNotEquals(self.obj.process, None)

    def test_apiritif_generated_requests(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                        "/reserve.php",
                    ]
                }
            }]
        })
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
                "scenario": {
                    "script": __dir__() + "/../apiritif/test_transactions.py"
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
        reader = FuncSamplesReader(__dir__() + "/../apiritif/transactions.ldjson", self.obj.engine, self.obj.log, [])
        items = list(reader.read(last_pass=True))
        self.assertEqual(len(items), 6)
        self.assertEqual(items[0].test_case, "test_1_single_request")
        self.assertEqual(items[1].test_case, "test_2_multiple_requests")
        self.assertEqual(items[2].test_case, "Transaction")
        self.assertEqual(items[3].test_case, "Transaction")
        self.assertEqual(items[4].test_case, "Transaction 1")
        self.assertEqual(items[5].test_case, "Transaction 2")


class TestSeleniumScriptBuilder(SeleniumTestCase):
    def test_build_script(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            {"waitByName('toPort')": "visible"},
                            {"keysByName(\"toPort\")": "B"},
                            "clickByXPath(//div[3]/form/select[1]//option[3])",
                            "clickByXPath(//div[3]/form/select[2]//option[6])",
                            "clickByXPath(//input[@type='submit'])",
                            "clickByLinkText(destination of the week! The Beach!)"
                        ],

                    }, {
                        "label": "empty"
                    }]
                }
            },
            "modules": {
                "selenium": {
                    "^virtual-display": 0}}})
        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.readlines()
        with open(__dir__() + "/../selenium/generated_from_requests.py") as sample:
            sample_contents = sample.readlines()

        # strip line terminator and exclude specific build path
        gen_contents = [line.rstrip() for line in gen_contents if 'webdriver' not in line]
        sample_contents = [line.rstrip() for line in sample_contents if 'webdriver' not in line]

        self.assertEqual(gen_contents, sample_contents)


class TestApiritifScriptBuilder(BZTestCase):
    def setUp(self):
        super(TestApiritifScriptBuilder, self).setUp()
        self.obj = NoseTester()
        self.obj.engine = EngineEmul()

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config["execution"][0]

    def test_keepalive_default(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("target.keep_alive(True)", test_script)

    def test_keepalive(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "keepalive": False,
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("target.keep_alive(False)", test_script)

    def test_timeout_default(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertNotIn("timeout=30.0", test_script)

    def test_timeout(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "timeout": "10s",
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?tag=1",
                        {
                            "url": "/?tag=2",
                            "timeout": "2s",
                        }
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("self.target.timeout(10.0)", test_script)
        self.assertNotIn("get('/?tag=1', timeout=10.0", test_script)
        self.assertIn("get('/?tag=2', timeout=2.0", test_script)

    def test_think_time(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        {
                            "url": "/?tag=2",
                            "think-time": "1s500ms",
                        }
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("time.sleep(1.5)", test_script)

    def test_methods(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        {"url": "/?tag=get",
                         "method": "GET"},
                        {"url": "/?tag=post",
                         "method": "POST"},
                        {"url": "/?tag=put",
                         "method": "PUT"},
                        {"url": "/?tag=patch",
                         "method": "PATCH"},
                        {"url": "/?tag=head",
                         "method": "HEAD"},
                        {"url": "/?tag=delete",
                         "method": "DELETE"},
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("get('/?tag=get'", test_script)
        self.assertIn("post('/?tag=post'", test_script)
        self.assertIn("put('/?tag=put'", test_script)
        self.assertIn("patch('/?tag=patch'", test_script)
        self.assertIn("head('/?tag=head'", test_script)
        self.assertIn("delete('/?tag=delete'", test_script)

    def test_default_address_path_prefix(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://a.blazemeter.com",
                    "base-path": "/api/latest",
                    "requests": [
                        "/user",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("target('https://a.blazemeter.com')", test_script)
        self.assertIn("target.base_path('/api/latest')", test_script)

    def test_headers(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "headers": {"X-Foo": "foo"},
                    "requests": [{
                        "url": "/",
                        "headers": {"X-Bar": "bar"}
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("'X-Foo': 'foo'", test_script)
        self.assertIn("'X-Bar': 'bar'", test_script)

    def test_follow_redirects_default(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("self.target.allow_redirects(True)", test_script)
        self.assertNotIn("allow_redirects=True", test_script)

    def test_follow_redirects(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "follow-redirects": False,
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("allow_redirects=False", test_script)

    def test_body_params(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "body": {
                            "foo": "bar",
                        },
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("params={'foo': 'bar'}", test_script)

    def test_body_json(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "headers": {
                            "Content-Type": "application/json",
                        },
                        "body": {
                            "foo": "bar",
                        },
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("json={'foo': 'bar'}", test_script)

    def test_body_string(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "body": "MY PERFECT BODY"
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("data='MY PERFECT BODY'", test_script)

    def test_body_unknown(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "body": 123
                    }]
                }
            }]
        })
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_plain_assertions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "assert": [
                            "Welcome", "Simple Travel Agency"
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("response.assert_regex_in_body('Welcome')", test_script)
        self.assertIn("response.assert_regex_in_body('Simple Travel Agency')", test_script)

    def test_plain_assertion_kinds(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "assert": [
                            {"contains": ["1"], "regexp": False, "not": False},
                            {"contains": ["2"], "regexp": False, "not": True},
                            {"contains": ["3"], "regexp": True, "not": False},
                            {"contains": ["4"], "regexp": True, "not": True},
                            {"contains": ["5"], "regexp": False, "not": False, "subject": "headers"},
                            {"contains": ["6"], "regexp": False, "not": True, "subject": "headers"},
                            {"contains": ["7"], "regexp": True, "not": False, "subject": "headers"},
                            {"contains": ["8"], "regexp": True, "not": True, "subject": "headers"},
                            {"contains": ["8"], "regexp": True, "not": True, "subject": "headers"},
                            {"contains": ["9"], "not": False, "subject": "http-code"},
                            {"contains": ["10"], "not": True, "subject": "http-code"},
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("assert_in_body('1')", test_script)
        self.assertIn("assert_not_in_body('2')", test_script)
        self.assertIn("assert_regex_in_body('3')", test_script)
        self.assertIn("assert_regex_not_in_body('4')", test_script)
        self.assertIn("assert_in_headers('5')", test_script)
        self.assertIn("assert_not_in_headers('6')", test_script)
        self.assertIn("assert_regex_in_headers('7')", test_script)
        self.assertIn("assert_regex_not_in_headers('8')", test_script)
        self.assertIn("assert_status_code('9')", test_script)
        self.assertIn("assert_not_status_code('10')", test_script)

    def test_jsonpath_assertions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-jsonpath": [
                            "$.foo.bar"
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("assert_jsonpath('$.foo.bar', expected_value=None)", test_script)

    def test_jsonpath_assertions_kinds(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-jsonpath": [
                            {"jsonpath": "$.1", "invert": False},
                            {"jsonpath": "$.2", "invert": True},
                            {"jsonpath": "$.3", "expected-value": "value"},
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("assert_jsonpath('$.1', expected_value=None)", test_script)
        self.assertIn("assert_not_jsonpath('$.2', expected_value=None)", test_script)
        self.assertIn("assert_jsonpath('$.3', expected_value='value')", test_script)

    def test_xpath_assertions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-xpath": [
                            "//head/title"
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("assert_xpath('//head/title', parser_type='html', validate=False)", test_script)

    def test_xpath_assertions_kinds(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-xpath": [
                            {"xpath": "//1", "invert": False},
                            {"xpath": "//2", "invert": True},
                            {"xpath": "//3", "validate-xml": True},
                            {"xpath": "//4", "validate-xml": False, "use-tolerant-parser": False},
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj._script) as fds:
            test_script = fds.read()
        self.assertIn("assert_xpath('//1', parser_type='html', validate=False)", test_script)
        self.assertIn("assert_not_xpath('//2', parser_type='html', validate=False)", test_script)
        self.assertIn("assert_xpath('//3', parser_type='html', validate=True)", test_script)
        self.assertIn("assert_xpath('//4', parser_type='xml', validate=False)", test_script)

    def test_complex_codegen(self):
        """ This test serves code review purposes, to make changes more visible """
        self.obj.engine.config.load([__dir__() + '/../apiritif/test_codegen.yml'])
        self.configure(self.obj.engine.config['execution'][0])
        self.obj.settings['verbose'] = True
        self.obj.prepare()
        exp_file = __dir__() + '/../apiritif/test_codegen.py'
        # import shutil; shutil.copy2(self.obj._script, exp_file)  # keep this coment to ease updates
        self.assertFilesEqual(exp_file, self.obj._script)
