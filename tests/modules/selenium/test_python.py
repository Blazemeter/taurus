import json
import os
import time

from bzt import TaurusConfigError
from bzt.engine import ScenarioExecutor
from bzt.modules.functional import FuncSamplesReader, LoadSamplesReader, FunctionalAggregator
from bzt.modules.python import ApiritifNoseExecutor, PyTestExecutor, RobotExecutor, ApiritifLoadReader, \
    ApiritifFuncReader
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul
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
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "apiritif-0.csv")))

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
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "apiritif-0.csv")))

    def test_runner_fail_no_test_found(self):
        """
        Check that Python Nose runner fails if no tests were found
        :return:
        """
        self.configure({
            ScenarioExecutor.EXEC: {
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
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()


class TestNoseRunner(BZTestCase):
    def setUp(self):
        super(TestNoseRunner, self).setUp()
        self.obj = ApiritifNoseExecutor()
        self.obj.engine = EngineEmul()
        self.obj.env = self.obj.engine.env

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config["execution"][0]

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
        reader = LoadSamplesReader(os.path.join(self.obj.engine.artifacts_dir, "apiritif-0.ldjson"), self.obj.log)
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
        reader = FuncSamplesReader(os.path.join(self.obj.engine.artifacts_dir, "apiritif-0.ldjson"),
                                   self.obj.engine, self.obj.log)
        samples = list(reader.read(last_pass=True))
        self.assertEqual(len(samples), 4)
        self.assertIsNotNone(samples[-1].status)


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
                    "variables": {
                        "red_pill": "take_it",
                        "name": "Name"
                    },
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)",
                            "mouseMoveByXPath(/html/body/div[2]/div/p[2]/a)",
                            "doubleClickByXPath(/html/body/div[3]/h2)",
                            "mouseDownByXPath(/html/body/div[3]/form/select[1])",
                            "mouseUpByXPath(/html/body/div[3]/form/select[1]/option[6])",
                            {"selectByName(toPort)": "London"},
                            {"keysByCSS(body input.btn.btn-primary)": "KEY_ENTER"},
                            {"assertValueByID(address)": "123 Beautiful st."},
                            {"assertTextByXPath(/html/body/div[2]/form/div[1]/label)": "${name}"},
                            {"waitByName('toPort')": "visible"},
                            {"keysByName(\"toPort\")": "B"},
                            "clickByXPath(//div[3]/form/select[1]//option[3])",
                            "clickByXPath(//div[3]/form/select[2]//option[6])",
                            "selectWindow(0)",
                            "selectWindow('win_ser_local')",
                            "selectWindow('win_ser_1')",
                            "selectWindow('that_window')",
                            "closeWindow(1)",
                            "closeWindow('win_ser_local')",
                            "closeWindow('win_ser_1')",
                            "closeWindow('that_window')",
                            "submitByName(\"toPort\")",
                            "runScript(\"alert('This is Sparta');\")",
                            "selectFrameByName('my_frame')",
                            "selectFrameByIdx(1)",
                            {"editContentById(editor)": "lo-la-lu"},
                            "pauseFor(3s)",
                            "clearCookies()",
                            "clickByLinkText(destination of the week! The Beach!)",
                            "go(http:\\blazemeter.com)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                },
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": [
                        {
                            "browser": "firefox",
                            "version": "54.0",
                            "platform": "linux",
                            "javascript": "True",
                            "os_version": "",
                            "selenium": "",
                            "device": "",
                            "app": ""
                        }
                    ],
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.readlines()

        with open(RESOURCES_DIR + "selenium/generated_from_requests.py") as sample:
            sample_contents = sample.readlines()

        # strip line terminator and exclude specific build path
        gen_contents = [line.rstrip() for line in gen_contents if 'webdriver' not in line]
        sample_contents = [line.rstrip() for line in sample_contents if 'webdriver' not in line]

        self.assertEqual(gen_contents, sample_contents)

    def test_headless_default(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertNotIn("options.set_headless()", gen_contents)

    def test_headless_chrome(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertIn("options.set_headless()", gen_contents)

    def test_headless_firefox(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Firefox",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertIn("options.set_headless()", gen_contents)

    def test_headless_safari(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Opera",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertNotIn("options.set_headless()", gen_contents)

    def test_build_script_remote(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": [
                        {
                            "browser": "firefox",
                            "version": "54.0",
                            "platform": "linux",
                            "javascript": "True",
                            "os_version": "",
                            "selenium": "",
                            "device": "",
                            "app": ""
                        }
                    ],
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.readlines()

        with open(RESOURCES_DIR + "selenium/generated_from_requests_remote.py") as sample:
            sample_contents = sample.readlines()

        # strip line terminator
        gen_contents = [line.rstrip() for line in gen_contents]
        sample_contents = [line.rstrip() for line in sample_contents]

        self.assertEqual(gen_contents, sample_contents)

    def test_build_script_appium_browser(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc_appium"}],
            "scenarios": {
                "loc_sc_appium": {
                    "browser": "Chrome-Android",
                    "capabilities": [
                        {
                            "device": "",
                        }
                    ],
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.readlines()

        with open(RESOURCES_DIR + "selenium/generated_from_requests_appium_browser.py") as sample:
            sample_contents = sample.readlines()

        # strip line terminator
        gen_contents = [line.rstrip() for line in gen_contents]
        sample_contents = [line.rstrip() for line in sample_contents]

        self.assertEqual(gen_contents, sample_contents)


class TestApiritifScriptGenerator(BZTestCase):
    def setUp(self):
        super(TestApiritifScriptGenerator, self).setUp()
        self.obj = ApiritifNoseExecutor()
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target.timeout(10.0)", test_script)
        self.assertNotIn("get('/?tag=1', timeout=10.0", test_script)
        self.assertIn("get('/?tag=2', timeout=2.0", test_script)

    def test_timeout_notarget(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "timeout": "10s",
                    "requests": [
                        "http://blazedemo.com/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("get('http://blazedemo.com/', timeout=10.0", test_script)

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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target.allow_redirects(True)", test_script)
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
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
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assert_xpath('//1', parser_type='html', validate=False)", test_script)
        self.assertIn("assert_not_xpath('//2', parser_type='html', validate=False)", test_script)
        self.assertIn("assert_xpath('//3', parser_type='html', validate=True)", test_script)
        self.assertIn("assert_xpath('//4', parser_type='xml', validate=False)", test_script)

    def test_complex_codegen(self):
        """ This test serves code review purposes, to make changes more visible """
        self.obj.engine.config.load([RESOURCES_DIR + 'apiritif/test_codegen.yml'])
        self.configure(self.obj.engine.config['execution'][0])
        self.obj.settings['verbose'] = True
        self.obj.prepare()
        exp_file = RESOURCES_DIR + 'apiritif/test_codegen.py'
        # import shutil; shutil.copy2(self.obj.script, exp_file)  # keep this comment to ease updates
        self.assertFilesEqual(self.obj.script, exp_file)

    def test_jmeter_functions_time(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?time=${__time()}",
                        "/?time=${__time(MM/dd/yy)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("'/?time={}'.format(apiritif.format_date())", test_script)
        self.assertIn("'/?time={}'.format(apiritif.format_date('MM/dd/yy'))", test_script)

    def test_jmeter_functions_random(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?random=${__Random(1, 10)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("'/?random={}'.format(apiritif.random_uniform(1, 10))", test_script)

    def test_jmeter_functions_random_string(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?rs=${__RandomString(3)}",
                        "/?rs=${__RandomString(4,abcdef)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("'/?rs={}'.format(apiritif.random_string(3))", test_script)
        self.assertIn("'/?rs={}'.format(apiritif.random_string(4, 'abcdef'))", test_script)

    def test_jmeter_functions_base64_encode(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "headers": {
                        "Authorization": "Basic ${__base64Encode(user:pass)}",
                    },
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("base64_encode('user:pass')", test_script)

    def test_jmeter_functions_base64_decode(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "headers": {
                        "Additional": "${__base64Decode(dGVzdCBzdHJpbmc=)}",
                    },
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("base64_decode('dGVzdCBzdHJpbmc=')", test_script)

    def test_jmeter_functions_urlencode(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/${__urlencode(Foo Bar Baz)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("encode_url('Foo Bar Baz')", test_script)

    def test_jmeter_functions_uuid(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/${__UUID()}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("uuid()", test_script)

    def test_load_reader(self):
        reader = ApiritifLoadReader(self.obj.log)
        items = list(reader._read())
        self.assertEqual(len(items), 0)
        reader.register_file(RESOURCES_DIR + "jmeter/jtl/tranctl.jtl")
        items = list(reader._read())
        self.assertEqual(len(items), 2)
        reader.register_file(RESOURCES_DIR + "jmeter/jtl/tranctl.jtl")
        reader.register_file(RESOURCES_DIR + "jmeter/jtl/tranctl.jtl")
        items = list(reader._read())
        self.assertEqual(len(items), 4)

    def test_func_reader(self):
        reader = ApiritifFuncReader(self.obj.engine, self.obj.log)
        items = list(reader.read())
        self.assertEqual(len(items), 0)
        reader.register_file(RESOURCES_DIR + "apiritif/transactions.ldjson")
        reader.register_file(RESOURCES_DIR + "apiritif/transactions.ldjson")
        items = list(reader.read())
        self.assertEqual(len(items), 18)

    def test_codegen_requests(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "requests": [{
                        "url": "http://localhost:8000/",
                        "label": "apiritif",
                    }]
                }
            }]
        })
        self.obj.prepare()
        self.assertFilesEqual(self.obj.script, RESOURCES_DIR + "/apiritif/test_codegen_requests.py")

    def test_generator_crash(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "variables": {
                      "product_id": "5b6c",
                    },
                    "requests": [{
                        "url": "/",
                        "method": "POST",
                        "body": {
                            "product": "${product_id}"  # notice the space
                        }
                    }]
                }
            }]
        })
        self.obj.prepare()  # Unparser shouldn't crash with AttributeError because of malformed AST
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("data=[('product', vars['product_id'])]", test_script)

    def test_inherit_test_case(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "requests": [
                        "http://example.com/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("class TestAPI(unittest.TestCase)", test_script)


class TestPyTestExecutor(BZTestCase):
    def setUp(self):
        super(TestPyTestExecutor, self).setUp()
        self.obj = PyTestExecutor()
        self.obj.engine = EngineEmul()
        self.obj.env = self.obj.engine.env

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config["execution"][0]

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
        with open(self.obj.stdout_file) as fds:
            stdout = fds.read()
            self.assertIn(additional_args, stdout)


class TestRobotExecutor(BZTestCase):
    def setUp(self):
        super(TestRobotExecutor, self).setUp()
        self.obj = RobotExecutor()
        self.obj.engine = EngineEmul()
        self.obj.env = self.obj.engine.env

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config["execution"][0]

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
