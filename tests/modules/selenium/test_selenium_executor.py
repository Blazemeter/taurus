import os
import re
import shutil
import time

import yaml
from io import BytesIO

from bzt import ToolError, TaurusConfigError
from bzt.engine import EXEC
from bzt.modules.apiritif import ApiritifNoseExecutor
from bzt.modules.functional import LoadSamplesReader, FuncSamplesReader
from bzt.modules.provisioning import Local
from bzt.modules.selenium import SeleniumExecutor
from bzt.utils import LDJSONReader, FileReader
from tests import BZTestCase, RESOURCES_DIR, ROOT_LOGGER
from tests.mocks import EngineEmul, DummyListener
from tests.modules.selenium import SeleniumTestCase


class LDJSONReaderEmul(object):
    def __init__(self):
        self.data = []

    def read(self, last_pass=False):
        for line in self.data:
            yield line


class TestSeleniumExecutor(SeleniumTestCase):
    # todo: get_error_diagnostics: only geckodriver, not chrome-?
    def setUp(self):
        super(TestSeleniumExecutor, self).setUp()
        self.CMD_LINE = ''

    def run_script(self, name):
        with open(RESOURCES_DIR + "selenium/" + name + ".py") as script:
            self.wd_log = self.obj.engine.create_artifact("webdriver", ".log")
            script_lines = script.readlines()

            new_script = self.obj.engine.create_artifact(name, ".py")
            with open(new_script, 'w+') as new_script_file:
                for line in script_lines:
                    new_script_file.write(line.replace("'webdriver.log'", repr(self.wd_log)))

        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "iterations": 1,
                "scenario": {
                    "script": new_script}}]})
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.runner.process, None)

    def check_transaction_logged(self):
        with open(os.path.join(self.obj.engine.artifacts_dir, "apiritif.out")) as out:
            content = out.readlines()

            # todo: check for loadgen debug log ('find me!')

            stages = "Transaction started", "Transaction ended"
            names = "t1", "t2", "t3"

            for stage in stages:
                cases = [line for line in content if stage in line]
                for name in names:
                    self.assertIn(name, '\n'.join(cases))

    def check_flow_markers(self):
        with open(self.wd_log) as wd_file:
            content = wd_file.read()

            wd_lines = content.split("[INFO]")

        flow_markers = [l for l in wd_lines if "FLOW_MARKER" in l]
        for arg in ["t1", "start"]:
            self.assertIn(arg, flow_markers[0])
        for arg in ["success", "stop"]:
            self.assertIn(arg, flow_markers[1])
        for arg in ["t2", "start"]:
            self.assertIn(arg, flow_markers[2])
        for arg in ["Assertion", "failed", "stop"]:
            self.assertIn(arg, flow_markers[3])
        for arg in ["t3", "start"]:
            self.assertIn(arg, flow_markers[4])
        for arg in ["broken", "stop"]:
            self.assertIn(arg, flow_markers[5])

    def check_samples(self):
        # apiritif.0.csv filled by ApiritifPlugin
        with open(os.path.join(self.obj.engine.artifacts_dir, "apiritif.0.csv")) as sample_file:
            samples = sample_file.readlines()

        for arg in ["t1", "true"]:
            self.assertIn(arg, samples[1])

        for arg in ["t2", "Assertion"]:
            self.assertIn(arg, samples[2])

        for arg in ["t3", "Exception"]:
            self.assertIn(arg, samples[3])

    def test_selenium_old_flow(self):
        self.run_script("test_old_flow")
        self.check_transaction_logged()
        self.check_flow_markers()
        self.check_samples()

    def test_selenium_new_flow(self):
        self.run_script("test_new_flow")
        self.check_transaction_logged()
        self.check_flow_markers()
        self.check_samples()

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = " ".join(args)

    def test_data_source_in_action(self):
        self.configure({
            EXEC: {
                "executor": "selenium",
                "iterations": 1,
                "scenario": {
                    "data-sources": [RESOURCES_DIR + "selenium/data-sources/data.csv"],
                    "requests": [{
                        "label": "exec_it",
                        "assert": ["Simple Travel Agency"],
                        "actions": ["go(${host}/${page})"]}]}}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_user_iter(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "iterations": 100,
                "scenario": {
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = True
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertIn("--iterations 100", self.CMD_LINE)

    def test_load_no_iter(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "scenario": {
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = False
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertIn("--iterations 1", self.CMD_LINE)

    def test_load_no_iter_duration(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "hold-for": "2s",
                "scenario": {
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = False
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertNotIn("--iterations", self.CMD_LINE)

    def test_func_no_iter(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "scenario": {
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = True
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertIn("--iterations 1", self.CMD_LINE)

    def test_func_0_iter(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "iterations": 0,
                "scenario": {
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = True
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertNotIn('--iterations', self.CMD_LINE)

    def test_func_ds_0_iter(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "iterations": 0,
                "scenario": {
                    "data-sources": ['one.csv'],
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = True
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertNotIn('--iterations', self.CMD_LINE)

    def test_func_ds_no_iter(self):
        self.configure({
            EXEC: {
                "executor": "apiritif",
                "scenario": {
                    "data-sources": ['one.csv'],
                    "requests": [
                        "http://blazedemo.com"]}}})

        self.obj.engine.aggregator.is_functional = True
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        self.assertNotIn('--iterations', self.CMD_LINE)


class TestSeleniumStuff(SeleniumTestCase):
    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = args

    def obj_prepare(self):
        super(SeleniumExecutor, self.obj).prepare()
        self.obj.install_required_tools()
        for driver in self.obj.webdrivers:
            self.obj.env.add_path({"PATH": driver.get_driver_dir()})
        self.obj.create_runner()
        self.obj.runner._check_tools = lambda *args: None
        self.obj.runner._compile_scripts = lambda: None
        self.obj.runner.prepare()
        self.obj.script = self.obj.runner.script

    def test_empty_scenario(self):
        """
        Raise runtime error when no scenario provided
        :return:
        """
        self.configure({EXEC: {"executor": "selenium"}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_various_raise(self):
        self.configure({  # RuntimeError when
            EXEC: [{  # compilation fails
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/invalid.java"}
            }, {  # no files of known types were found.
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/not_found"}
            }]})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_empty_test_methods(self):
        self.configure({  # Test exact number of tests when
            EXEC: [{  # java annotations used
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/SeleniumTest.java"}
            }, {  # test class extends JUnit TestCase
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/SimpleTest.java"}
            }, {  # annotations used and no "test" in class name
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/selenium1.java"}
        }]})
        self.obj_prepare()

    def test_from_extension(self):
        self.configure(yaml.full_load(open(RESOURCES_DIR + "yaml/selenium_from_extension.yml").read()))
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.post_process()

    def test_requests(self):
        self.configure(yaml.full_load(open(RESOURCES_DIR + "yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.post_process()

    def test_fail_on_zero_results(self):
        self.configure(yaml.full_load(open(RESOURCES_DIR + "yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        prov.started_modules = [self.obj]
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
        files = self.obj.resource_files()
        self.obj_prepare()
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
        self.obj_prepare()

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
        urls = re.findall(r"\.get\('(.+)'\)", script)
        self.assertEqual("http://blazedemo.com/", urls[0])
        self.assertEqual("http://absolute.address.com/somepage", urls[1])
        self.assertEqual("http://blazedemo.com/reserve.php", urls[2])

    def test_force_runner(self):
        self.obj.execution.merge({
            'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
            'runner': 'apiritif',
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
        self.obj_prepare()
        self.obj.subscribe_to_transactions(dummy)
        try:
            self.obj.engine.start_subprocess = self.start_subprocess
            self.obj.startup()
            fake_out = os.path.join(RESOURCES_DIR, 'apiritif/dummy-output.out')
            self.obj.runner._tailer = FileReader(filename=fake_out, parent_logger=self.log)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertEqual(10, dummy.transactions['hello there'])


class TestReportReader(BZTestCase):
    def test_report_reader(self):
        reader = LoadSamplesReader(RESOURCES_DIR + "selenium/report.ldjson", ROOT_LOGGER)
        items = list(reader._read(last_pass=True))
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
        reader = LDJSONReader("yip", ROOT_LOGGER)
        buffer = BytesIO(first_part)
        reader.file.fds = buffer
        reader.file.fds.name = "yip"

        items = list(reader.read(last_pass=False))
        self.assertEqual(len(items), 1)

        buffer.write(second_part)
        items = list(reader.read(last_pass=False))
        self.assertEqual(len(items), 2)

    def test_func_reader(self):
        reader = FuncSamplesReader(RESOURCES_DIR + "selenium/report.ldjson", EngineEmul(), ROOT_LOGGER)
        items = list(reader.read(last_pass=True))
        self.assertEqual(5, len(items))
        self.assertEqual(items[0].test_case, 'testFailure')
        self.assertEqual(items[0].status, "FAILED")
        self.assertEqual(items[1].test_case, 'testBroken')
        self.assertEqual(items[1].status, "BROKEN")
        self.assertEqual(items[2].test_case, 'testSuccess')
        self.assertEqual(items[2].status, "PASSED")
        self.assertEqual(items[4].test_case, 'SkippedTest')
        self.assertEqual(items[4].status, "SKIPPED")
