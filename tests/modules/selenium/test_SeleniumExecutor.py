import logging
import os
import re
import shutil
import time

import yaml

from bzt import ToolError, TaurusConfigError
from bzt.engine import ScenarioExecutor
from bzt.modules.functional import LoadSamplesReader, FuncSamplesReader
from bzt.modules.provisioning import Local
from bzt.modules.python import NoseTester
from bzt.six import StringIO
from bzt.utils import LDJSONReader
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul
from tests.modules.selenium import SeleniumTestCase


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
                "scenario": {"script": __dir__() + "/../../resources/selenium/invalid/invalid.java"}
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
            "scenario": {"script": __dir__() + "/../../resources/selenium/invalid/not_found"}
        }})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_samples_count_annotations(self):
        """
        Test exact number of tests when java annotations used
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../../resources/selenium/invalid/SeleniumTest.java"}
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
            "scenario": {"script": __dir__() + "/../../resources/selenium/invalid/SimpleTest.java"}
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
            "scenario": {"script": __dir__() + "/../../resources/selenium/invalid/selenium1.java"}
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

    def test_requests(self):
        self.configure(yaml.load(open(__dir__() + "/../../resources/yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        with open(os.path.join(self.obj.engine.artifacts_dir, self.obj.runner.execution['executor'] + ".err")) as fds:
            contents = fds.read()
            msg = "file: '%s', size: %s, content: '%s'" % (fds, fds.__sizeof__(), contents)
            self.assertEqual(3, contents.count("ok"), msg)
            self.assertEqual(1, contents.count("OK"))

    def test_fail_on_zero_results(self):
        self.configure(yaml.load(open(__dir__() + "/../../resources/yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        self.obj.engine.provisioning = prov
        self.assertRaises(ToolError, self.obj.engine.provisioning.post_process)

    def test_aremote_prov_requests(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": [
                    "http://blazedemo.com"]}})
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
        name1 = 'test_00000_http_blazedemo_com'
        name2 = 'test_00001_Main_Page'
        name3 = 'test_00002_just_for_lulz'
        reader = self.obj.runner.reader
        reader.report_reader.json_reader = LDJSONReaderEmul()
        reader.report_reader.json_reader.data.extend([
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
        res = list(reader._read())
        self.assertIn('http_blazedemo_com', res[0])
        self.assertIn('Main_Page', res[1])
        self.assertIn('just_for_lulz', res[2])

    def test_dont_copy_local_script_to_artifacts(self):
        "ensures that .java file is not copied into artifacts-dir"
        filename = "BlazeDemo.java"
        script_path = __dir__() + "/../../resources/" + filename
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
        """ensures that executor looks for script in artifacts-dir (for cloud/remote cases)"""
        self.obj.engine.file_search_paths = [self.obj.engine.artifacts_dir]

        script_name = "BlazeDemo.java"
        test_script = __dir__() + "/../../resources/" + script_name
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
            'scenario': {'script': __dir__() + '/../../resources/selenium/junit/jar/'},
            'runner': 'nose',
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, NoseTester)

    def test_additional_classpath_resource_files(self):
        self.obj.execution.merge({
            'scenario': {
                'script': __dir__() + '/../../resources/selenium/junit/jar/dummy.jar',
                'runner': 'junit',
                'additional-classpath': [__dir__() + '/../../resources/selenium/junit/jar/another_dummy.jar']}})
        self.obj.settings.merge({
            'selenium-tools': {
                'junit': {
                    'additional-classpath': [__dir__() + '/../../resources/selenium/testng/jars/testng-suite.jar']}}})
        own_resources = self.obj.resource_files()
        all_resources = list(set(self.obj.get_resource_files()))

        # scenario.script, scenario.additional-classpath, settings.additional-classpath
        self.assertEqual(len(own_resources), 2)
        self.assertEqual(len(all_resources), 3)


class TestReportReader(BZTestCase):
    def test_report_reader(self):
        reader = LoadSamplesReader(__dir__() + "/../../resources/selenium/report.ldjson", logging.getLogger(), None)
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
        reader = FuncSamplesReader(__dir__() + "/../../resources/selenium/report.ldjson", EngineEmul(), logging.getLogger(), None)
        items = list(reader.read())
        self.assertEqual(4, len(items))
        self.assertEqual(items[0].test_case, 'testFailure')
        self.assertEqual(items[0].status, "FAILED")
        self.assertEqual(items[1].test_case, 'testBroken')
        self.assertEqual(items[1].status, "BROKEN")
        self.assertEqual(items[2].test_case, 'testSuccess')
        self.assertEqual(items[2].status, "PASSED")
