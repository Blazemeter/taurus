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
from bzt.modules.python.python import ApiritifNoseExecutor
from bzt.six import BytesIO
from bzt.utils import LDJSONReader, FileReader
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul, DummyListener
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
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/invalid.java"}
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
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/not_found"}
        }})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_samples_count_annotations(self):
        """
        Test exact number of tests when java annotations used
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/SeleniumTest.java"}
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
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/SimpleTest.java"}
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
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/selenium1.java"}
        }})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

    def test_from_extension(self):
        self.configure(yaml.load(open(RESOURCES_DIR + "yaml/selenium_from_extension.yml").read()))
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        results = list(self.obj.runner.reader._read(final_pass=True))
        self.assertEquals(1, len(results))
        self.assertIsNone(results[0][7])    # error msg

    def test_from_extension_reuse(self):
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "iterations": 1,
            "scenario": {"script": RESOURCES_DIR + "selenium/python/reuse_after_extension.py"}
        }})
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        results = list(self.obj.runner.reader._read(final_pass=True))
        self.assertEquals(1, len(results))
        self.assertIsNone(results[0][7])  # error msg

    def test_requests(self):
        self.configure(yaml.load(open(RESOURCES_DIR + "yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        reader = FileReader(os.path.join(self.obj.engine.artifacts_dir, "apiritif-0.csv"))
        lines = reader.get_lines(last_pass=True)
        self.assertEquals(4, len(list(lines)))

    def test_fail_on_zero_results(self):
        self.configure(yaml.load(open(RESOURCES_DIR + "yaml/selenium_executor_requests.yml").read()))
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

    def test_dont_copy_local_script_to_artifacts(self):
        "ensures that .java file is not copied into artifacts-dir"
        filename = "BlazeDemo.java"
        script_path = RESOURCES_DIR + "" + filename
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
        test_script = RESOURCES_DIR + "" + script_name
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
            'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
            'runner': 'nose',
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, ApiritifNoseExecutor)

    def test_additional_classpath_resource_files(self):
        self.obj.execution.merge({
            'scenario': {
                'script': RESOURCES_DIR + 'selenium/junit/jar/dummy.jar',
                'runner': 'junit',
                'additional-classpath': [RESOURCES_DIR + 'selenium/junit/jar/another_dummy.jar']}})
        self.obj.engine.config.merge({
            'modules': {
                'junit': {
                    'additional-classpath': [RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar']}}})
        own_resources = self.obj.resource_files()
        all_resources = list(set(self.obj.get_resource_files()))

        # scenario.script, scenario.additional-classpath, settings.additional-classpath
        self.assertEqual(len(own_resources), 2)
        self.assertEqual(len(all_resources), 3)

    def test_add_env_path(self):
        path1 = os.path.join("foo", "bar")
        path2 = os.path.join("bar", "baz")
        self.obj.env.add_path({"PATH": path1})
        self.obj.env.add_path({"PATH": path2})
        self.assertIn(path1, self.obj.env.get("PATH"))
        self.assertIn(path2, self.obj.env.get("PATH"))

    def test_subscribe_to_transactions(self):
        dummy = DummyListener()

        self.configure({
            'execution': {
                "iterations": 5,
                'scenario': {'script': RESOURCES_DIR + 'selenium/python/test_selenium_transactions.py'},
                'executor': 'selenium'
            },
        })
        self.obj.prepare()
        self.obj.subscribe_to_transactions(dummy)
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()

        self.assertEqual(10, dummy.transactions['hello there'])


class TestReportReader(BZTestCase):
    def test_report_reader(self):
        reader = LoadSamplesReader(RESOURCES_DIR + "selenium/report.ldjson", logging.getLogger())
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
        first_part = b'{"a": 1, "b": 2}\n{"a": 2,'
        second_part = b'"b": 3}\n{"a": 3, "b": 4}\n'
        reader = LDJSONReader("yip", logging.getLogger())
        buffer = BytesIO(first_part)
        reader.file.fds = buffer
        reader.file.fds.name = "yip"

        items = list(reader.read(last_pass=False))
        self.assertEqual(len(items), 1)

        buffer.write(second_part)
        items = list(reader.read(last_pass=False))
        self.assertEqual(len(items), 2)

    def test_func_reader(self):
        reader = FuncSamplesReader(RESOURCES_DIR + "selenium/report.ldjson", EngineEmul(), logging.getLogger())
        items = list(reader.read())
        self.assertEqual(4, len(items))
        self.assertEqual(items[0].test_case, 'testFailure')
        self.assertEqual(items[0].status, "FAILED")
        self.assertEqual(items[1].test_case, 'testBroken')
        self.assertEqual(items[1].status, "BROKEN")
        self.assertEqual(items[2].test_case, 'testSuccess')
        self.assertEqual(items[2].status, "PASSED")
