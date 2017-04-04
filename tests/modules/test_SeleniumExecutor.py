import json
import logging
import re
import shutil
import sys
import time

import os
import yaml
from bzt import ToolError, TaurusConfigError
from tests import BZTestCase, local_paths_config, __dir__

from bzt.engine import ScenarioExecutor
from bzt.modules.provisioning import Local
from bzt.modules.selenium import NoseTester
from bzt.modules.selenium import SeleniumExecutor, LoadSamplesReader, LDJSONReader, FuncSamplesReader
from bzt.six import StringIO
from bzt.utils import is_windows, get_full_path
from tests.mocks import EngineEmul


class SeleniumTestCase(BZTestCase):
    def setUp(self):
        super(SeleniumTestCase, self).setUp()
        engine_obj = EngineEmul()
        paths = [__dir__() + "/../../bzt/10-base.json", local_paths_config()]
        engine_obj.configure(paths)  # FIXME: avoid using whole engine in particular module test!
        self.obj = SeleniumExecutor()
        self.obj.settings = engine_obj.config.get("modules").get("selenium")
        self.obj.settings.merge({"virtual-display": {"width": 1024, "height": 768}})
        engine_obj.create_artifacts_dir(paths)
        self.obj.engine = engine_obj

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config.get('execution')
        if isinstance(self.obj.execution, list):
            self.obj.execution = self.obj.execution[0]

    def tearDown(self):
        exc, _, _ = sys.exc_info()
        if exc:
            try:
                stdout_path = os.path.join(self.obj.engine.artifacts_dir, "selenium.out")
                if os.path.exists(stdout_path):
                    stdout = open(stdout_path).read()
                    logging.info('Selenium stdout: """\n%s\n"""', stdout)
            except BaseException:
                pass
            try:
                stdout_path = os.path.join(self.obj.engine.artifacts_dir, "selenium.err")
                if os.path.exists(stdout_path):
                    stderr = open(stdout_path).read()
                    logging.info('Selenium stderr: """\n%s\n"""', stderr)
            except BaseException:
                pass
        self.obj.free_virtual_display()




class TestSeleniumRSpecRunner(SeleniumTestCase):
    def test_selenium_prepare_rspec(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": __dir__() + "/../selenium/ruby/example_spec.rb"
                }}})
        self.obj.prepare()

    def test_rspec_full(self):
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))
        lines = open(self.obj.runner.settings.get("report-file")).readlines()
        self.assertEqual(len(lines), 3)
        first, second, third = lines
        self.assertEqual(json.loads(first)["status"], "PASSED")
        self.assertEqual(json.loads(second)["status"], "FAILED")
        self.assertEqual(json.loads(third)["status"], "FAILED")

    def test_rspec_hold(self):
        self.configure({
            'execution': {
                'hold-for': '10s',
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 10)

    def test_rspec_iterations(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))
        lines = open(self.obj.runner.settings.get("report-file")).readlines()
        self.assertEqual(len(lines), 9)

    def test_interpreter(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {'script': __dir__() + '/../selenium/ruby/example_spec.rb'},
                'executor': 'selenium'
            },
        })
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        dummy = __dir__() + '/../selenium/ruby/ruby-dummy'
        dummy += '.bat' if is_windows() else ''
        self.obj.settings.merge({
            "selenium-tools": {"rspec": {"interpreter": dummy}}
        })
        self.obj.prepare()


class TestSeleniumMochaRunner(SeleniumTestCase):
    def test_selenium_prepare_mocha(self):
        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/js-mocha/bd_scenarios.js"
        }})
        self.obj.prepare()

    def test_mocha_full(self):
        self.obj.engine.config.merge({
            'execution': {
                "script": __dir__() + "/../selenium/js-mocha/bd_scenarios.js"
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']

        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/js-mocha/bd_scenarios.js"
        }})

        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))
        lines = open(self.obj.runner.settings.get("report-file")).readlines()
        self.assertEqual(len(lines), 3)

    def test_mocha_hold(self):
        self.obj.engine.config.merge({
            'execution': {
                'hold-for': '5s',
                'scenario': {'script': __dir__() + '/../selenium/js-mocha/'},
                'executor': 'selenium'
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']

        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/js-mocha/"
        }})

        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 5)

    def test_mocha_iterations(self):
        self.obj.engine.config.merge({
            'execution': {
                'iterations': 3,
                'scenario': {'script': __dir__() + '/../selenium/js-mocha'},
                'executor': 'selenium'
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']

        self.obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/js-mocha"
        }})

        self.obj.settings.merge(self.obj.engine.config.get("modules").get("selenium"))
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))
        lines = open(self.obj.runner.settings.get("report-file")).readlines()
        self.assertEqual(len(lines), 9)

    def test_install_mocha(self):
        dummy_installation_path = get_full_path(__dir__() + "/../../build/tmp/selenium-taurus/mocha")
        mocha_link = get_full_path(__dir__() + "/../data/mocha-3.1.0.tgz")
        wd_link = get_full_path(__dir__() + "/../data/selenium-webdriver-1.0.0.tgz")

        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)
        self.assertFalse(os.path.exists(dummy_installation_path))

        old_node_path = os.environ.get("NODE_PATH")
        if old_node_path:
            os.environ.pop("NODE_PATH")

        orig_mocha_package = SeleniumExecutor.MOCHA_NPM_PACKAGE_NAME
        orig_wd_package = SeleniumExecutor.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME
        SeleniumExecutor.MOCHA_NPM_PACKAGE_NAME = mocha_link
        SeleniumExecutor.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = wd_link

        self.obj.settings.merge({"selenium-tools": {
            "mocha": {"tools-dir": dummy_installation_path}
        }})

        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/js-mocha/bd_scenarios.js"}})
        self.obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "node_modules")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "node_modules", "mocha")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "node_modules", "mocha", "index.js")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "node_modules", "selenium-webdriver")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "node_modules", "selenium-webdriver",
                                                    "index.js")))

        SeleniumExecutor.MOCHA_NPM_PACKAGE_NAME = orig_mocha_package
        SeleniumExecutor.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = orig_wd_package
        if old_node_path:
            os.environ["NODE_PATH"] = old_node_path


class LDJSONReaderEmul(object):
    def __init__(self):
        self.data = []

    def read(self, last_pass=False):
        for line in self.data:
            yield line


class TestSeleniumStuff(SeleniumTestCase):
    def test_empty_scenario(self):
        """
        Raise runtime error when no scenario provided
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {"executor": "selenium"}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_javac_fail(self):
        """
        Test RuntimeError when compilation fails
        :return:
        """
        self.configure({
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": __dir__() + "/../selenium/invalid/invalid.java"}
            }
        })
        self.assertRaises(ToolError, self.obj.prepare)

    def test_no_supported_files_to_test(self):
        """
        Test RuntimeError raised when no files of known types were found.
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/not_found"}
        }})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_samples_count_annotations(self):
        """
        Test exact number of tests when java annotations used
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/SeleniumTest.java"}
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

    def test_samples_count_testcase(self):
        """
        Test exact number of tests when test class extends JUnit TestCase
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/SimpleTest.java"}
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

    def test_no_test_in_name(self):
        """
        Test exact number of tests when annotations used and no "test" in class name
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/selenium1.java"}
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

    def test_requests(self):
        self.configure(yaml.load(open(__dir__() + "/../yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        with open(os.path.join(self.obj.engine.artifacts_dir, "selenium.err")) as fds:
            contents = fds.read()
            msg = "file: '%s', size: %s, content: '%s'" % (fds, fds.__sizeof__(), contents)
            self.assertEqual(3, contents.count("ok"), msg)
            self.assertEqual(1, contents.count("OK"))

    def test_fail_on_zero_results(self):
        self.configure(yaml.load(open(__dir__() + "/../yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        self.obj.engine.provisioning = prov
        self.assertRaises(ToolError, self.obj.engine.provisioning.post_process)

    def test_remote_prov_requests(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": [
                    "http://blazedemo.com"
                ]
            }
        })
        resources = self.obj.resource_files()
        self.assertEqual(0, len(resources))

    def test_labels_translation(self):
        self.configure({
            "scenarios": {
                "req_sel": {
                    "requests": [
                        "http://blazedemo.com",
                        {
                            'url': 'http://blazemeter.com',
                            'label': 'Main Page'
                        }]}}})
        self.obj.execution.merge({
            "scenario": "req_sel"})
        self.obj.prepare()
        gen_methods = self.obj.generated_methods
        name1 = 'test_00000_http_blazedemo_com'
        url1 = 'http://blazedemo.com'
        name2 = 'test_00001_Main_Page'
        label2 = 'Main Page'
        name3 = 'test_00002_just_for_lulz'
        label3 = 'just_for_lulz'
        self.assertEqual(url1, gen_methods[name1])
        self.assertEqual(label2, gen_methods[name2])
        self.obj.reader.report_reader.json_reader = LDJSONReaderEmul()
        self.obj.reader.report_reader.json_reader.data.extend([
            {
                'test_case': name1, 'start_time': 1472049887, 'duration': 1.0, 'status': 'PASSED',
                'test_suite': 'Tests', 'error_msg': None, 'error_trace': None, 'extras': None,
            }, {
                'test_case': name2, 'start_time': 1472049888, 'duration': 1.0, 'status': 'PASSED',
                'test_suite': 'Tests', 'error_msg': None, 'error_trace': None, 'extras': None,
            }, {
                'test_case': name3, 'start_time': 1472049889, 'duration': 1.0, 'status': 'PASSED',
                'test_suite': 'Tests', 'error_msg': None, 'error_trace': None, 'extras': None,
            }])
        res = list(self.obj.reader._read())
        self.assertIn(url1, res[0])
        self.assertIn(label2, res[1])
        self.assertIn(label3, res[2])

    def test_dont_copy_local_script_to_artifacts(self):
        "ensures that .java file is not copied into artifacts-dir"
        filename = "BlazeDemo.java"
        script_path = __dir__() + "/../data/" + filename
        self.obj.execution.merge({
            "scenario": {
                "script": script_path,
            }
        })
        self.obj.prepare()
        files = self.obj.resource_files()
        self.assertIn(script_path, files)
        artifacts_script = os.path.join(self.obj.engine.artifacts_dir, filename)
        self.assertFalse(os.path.exists(artifacts_script))

    def test_take_script_from_artifacts(self):
        "ensures that executor looks for script in artifacts-dir (for cloud/remote cases)"
        self.obj.engine.file_search_paths = [self.obj.engine.artifacts_dir]

        script_name = "BlazeDemo.java"
        test_script = __dir__() + "/../data/" + script_name
        artifacts_script = os.path.join(self.obj.engine.artifacts_dir, script_name)
        shutil.copy2(test_script, artifacts_script)

        self.obj.execution.merge({
            "scenario": {
                "script": script_name,
            }
        })
        self.obj.prepare()

    def test_do_not_modify_scenario_script(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": ["address"],
            }
        })
        self.obj.prepare()
        self.assertNotIn("script", self.obj.get_scenario())

    def test_default_address_gen(self):
        self.obj.execution.merge({
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": ["/", "http://absolute.address.com/somepage", "/reserve.php"],
            }
        })
        self.obj.prepare()
        with open(os.path.join(self.obj.engine.artifacts_dir, os.path.basename(self.obj.script))) as fds:
            script = fds.read()
        urls = re.findall(r"get\('(.+)'\)", script)
        self.assertEqual("http://blazedemo.com/", urls[0])
        self.assertEqual("http://absolute.address.com/somepage", urls[1])
        self.assertEqual("http://blazedemo.com/reserve.php", urls[2])

    def test_force_runner(self):
        self.obj.execution.merge({
            'scenario': {'script': __dir__() + '/../selenium/junit/jar/'},
            'runner': 'nose',
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, NoseTester)

    def test_additional_classpath_resource_files(self):
        self.obj.execution.merge({
            'scenario': {
                'script': __dir__() + '/../selenium/junit/jar/dummy.jar',
                'runner': 'junit',
                'additional-classpath': [__dir__() + '/../selenium/junit/jar/another_dummy.jar'],
            },
        })
        self.obj.settings.merge({
            'additional-classpath': [__dir__() + '/../selenium/testng/jars/testng-suite.jar'],
        })
        resources = self.obj.resource_files()
        # scenario.script, scenario.additional-classpath, settings.additional-classpath
        self.assertEqual(len(resources), 3)

    def test_required_tools(self):
        self.obj.install_required_tools()


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


class TestReportReader(BZTestCase):
    def test_report_reader(self):
        reader = LoadSamplesReader(__dir__() + "/../selenium/report.ldjson", logging.getLogger(), None)
        items = list(reader._read())
        self.assertEqual(4, len(items))
        self.assertEqual(items[0][1], 'testFailure')
        self.assertEqual(items[0][6], '400')
        self.assertEqual(items[1][1], 'testBroken')
        self.assertEqual(items[1][6], '500')
        self.assertEqual(items[2][1], 'testSuccess')
        self.assertEqual(items[2][6], '200')
        self.assertEqual(items[3][1], 'testUnexp')
        self.assertEqual(items[3][6], 'UNKNOWN')

    def test_reader_buffering(self):
        first_part = '{"a": 1, "b": 2}\n{"a": 2,'
        second_part = '"b": 3}\n{"a": 3, "b": 4}\n'
        reader = LDJSONReader("yip", logging.getLogger())
        buffer = StringIO(first_part)
        reader.fds = buffer

        items = list(reader.read(last_pass=False))
        self.assertEqual(len(items), 1)

        buffer.write(second_part)
        items = list(reader.read(last_pass=False))
        self.assertEqual(len(items), 2)

    def test_func_reader(self):
        reader = FuncSamplesReader(__dir__() + "/../selenium/report.ldjson", logging.getLogger(), None)
        items = list(reader.read())
        self.assertEqual(4, len(items))
        self.assertEqual(items[0].test_case, 'testFailure')
        self.assertEqual(items[0].status, "FAILED")
        self.assertEqual(items[1].test_case, 'testBroken')
        self.assertEqual(items[1].status, "BROKEN")
        self.assertEqual(items[2].test_case, 'testSuccess')
        self.assertEqual(items[2].status, "PASSED")
