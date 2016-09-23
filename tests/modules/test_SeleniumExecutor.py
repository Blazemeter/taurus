import logging
import os
import re
import shutil
import time

import yaml

from bzt.engine import ScenarioExecutor
from bzt.modules.selenium import SeleniumExecutor, JUnitJar, LoadSamplesReader, LDJSONReader, FuncSamplesReader
from bzt.modules.selenium import NoseTester
from bzt.six import StringIO
from tests import BZTestCase, local_paths_config, __dir__
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
        self.obj.free_virtual_display()


class TestSeleniumJUnitRunner(SeleniumTestCase):
    def test_install_tools(self):
        """
        check installation of selenium-server, junit
        :return:
        """
        dummy_installation_path = __dir__() + "/../../build/tmp/selenium-taurus"
        base_link = "file:///" + __dir__() + "/../data/"

        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = base_link + "/selenium-server-standalone-2.46.0.jar"

        junit_link = SeleniumExecutor.JUNIT_DOWNLOAD_LINK
        junit_mirrors = SeleniumExecutor.JUNIT_MIRRORS_SOURCE
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = base_link + "/junit-4.12.jar"
        SeleniumExecutor.JUNIT_MIRRORS_SOURCE = base_link + "unicode_file"

        hamcrest_link = SeleniumExecutor.HAMCREST_DOWNLOAD_LINK
        SeleniumExecutor.HAMCREST_DOWNLOAD_LINK = base_link + "/hamcrest-core-1.3.jar"

        self.assertFalse(os.path.exists(dummy_installation_path))

        self.obj.settings.merge({"selenium-tools": {
            "junit": {"selenium-server": os.path.join(dummy_installation_path, "selenium-server.jar")}
        }})
        self.obj.settings.merge({"selenium-tools": {
            "junit": {"hamcrest-core": os.path.join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar")}
        }})
        self.obj.settings.merge({"selenium-tools": {
            "junit": {"path": os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")}
        }})

        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/"}})
        self.obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "selenium-server.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar")))
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = junit_link
        SeleniumExecutor.HAMCREST_DOWNLOAD_LINK = hamcrest_link
        SeleniumExecutor.JUNIT_MIRRORS_SOURCE = junit_mirrors

    def test_prepare_java_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java/TestBlazemeterFail.java"}})
        self.obj.prepare()
        self.assertFalse(os.path.exists(os.path.join(self.obj.runner.working_dir, "TestBlazemeterFail.java")))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "TestBlazemeterFail.class")))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))

    def test_prepare_java_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java/"}})
        self.obj.prepare()
        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 2)
        self.assertEqual(len(jars), 1)

    def test_prepare_java_package(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java_package/"}})
        self.obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))

    def test_selenium_startup_shutdown_java_package(self):
        """
        Run tests from package
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java_package/src'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))

    def test_prepare_jar_single(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/dummy.jar"}})
        self.obj.prepare()

    def test_prepare_jar_folder(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/"}})
        self.obj.prepare()

    def test_selenium_startup_shutdown_jar_single(self):
        """
        runt tests from single jar
        :return:
        """
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/dummy.jar"}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_jar_folder(self):
        """
        run tests from jars
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_java_single(self):
        """
        run tests from single .java file
        :return:
        """
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java/TestBlazemeterFail.java"}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))
        self.assertEqual(1, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        run tests from .java files
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })

        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))
        self.assertEqual(2, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))

    def test_not_junit(self):
        """
        Check that JUnit runner fails if no tests were found
        :return:
        """
        self.configure({
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": __dir__() + "/../selenium/invalid/NotJUnittest.java"}}})
        self.obj.prepare()
        self.obj.startup()
        try:
            while not self.obj.check():
                time.sleep(1)
            self.fail()
        except BaseException as exc:
            self.assertIn("Nothing to test", exc.args[0])
        self.obj.shutdown()

    def test_resource_files_collection_remote_java(self):
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)

    def test_resource_files_collection_remote_jar(self):
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)


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
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))

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
        self.assertTrue(os.path.exists(self.obj.runner.settings.get("report-file")))

    def runner_fail_no_test_found(self):
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
        except RuntimeError as exc:
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
            except RuntimeError as exc:
                self.assertIn("Catch that", exc.message)
                self.assertIn("Nothing to test", exc.message)
                break


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
        self.assertRaises(ValueError, self.obj.prepare)

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
        self.assertRaises(RuntimeError, self.obj.prepare)

    def test_no_supported_files_to_test(self):
        """
        Test RuntimeError raised when no files of known types were found.
        :return:
        """
        self.configure({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/not_found"}
        }})
        self.assertRaises(ValueError, self.obj.prepare)

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
            self.assertEqual(3, contents.count("ok"), "file: '%s', size: %s, content: '%s'" % (fds, fds.__sizeof__(),
                                                                                               contents))
            self.assertEqual(1, contents.count("OK"))

    def test_fail_on_zero_results(self):
        self.configure(yaml.load(open(__dir__() + "/../yaml/selenium_executor_requests.yml").read()))
        self.obj.prepare()
        self.assertRaises(RuntimeWarning, self.obj.post_process)

    def test_junit_mirrors(self):
        dummy_installation_path = __dir__() + "/../../build/tmp/selenium-taurus"
        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)
        obj = SeleniumExecutor()
        objjm = JUnitJar(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar"), obj.log,
                         SeleniumExecutor.JUNIT_VERSION)
        objjm.install()

    def test_remote_prov_requests(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": [
                    "http://blazedemo.com"
                ]
            }
        })
        self.obj.resource_files()

    def test_a_labels_translation(self):
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

    def test_force_language(self):
        self.obj.execution.merge({
            'scenario': {'script': __dir__() + '/../selenium/jar/'},
            'language': 'python-nose',
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, NoseTester)


class TestASeleniumScriptBuilder(SeleniumTestCase):
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
                    "requests": [{
                        "url": "/"}]}},
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
