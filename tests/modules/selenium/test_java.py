import json
import logging
import os
import shutil
import time
import traceback

import yaml

from os import listdir
from os.path import exists, join, dirname
from bzt.engine import ScenarioExecutor
from bzt.modules import java
from bzt.modules.selenium import SeleniumExecutor
from bzt.modules.java import JUnitTester, JavaTestRunner, TestNGTester, JUnitJar, JUNIT_VERSION, JavaC
from bzt.utils import get_full_path, ToolError
from tests import BZTestCase, local_paths_config, RESOURCES_DIR, BUILD_DIR
from tests.mocks import EngineEmul
from tests.modules.selenium import SeleniumTestCase
from bzt.modules.aggregator import ConsolidatingAggregator, KPISet


class TestTestNGTester(BZTestCase):
    def setUp(self):
        super(TestTestNGTester, self).setUp()
        engine_obj = EngineEmul()
        paths = [local_paths_config()]
        engine_obj.configure(paths)
        self.obj = TestNGTester()
        self.obj.settings = engine_obj.config.get("modules").get("testng")
        self.obj.engine = engine_obj
        self.obj.env = self.obj.engine.env

    def test_simple(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/testng/TestNGSuite.java"}})
        self.obj.settings['autodetect-xml'] = False
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()

    def test_install_tools(self):
        dummy_installation_path = BUILD_DIR + "selenium-taurus"
        base_link = "file:///" + RESOURCES_DIR + "selenium"

        shutil.rmtree(dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = java.SELENIUM_DOWNLOAD_LINK
        testng_link = java.TESTNG_DOWNLOAD_LINK
        hamcrest_link = java.HAMCREST_DOWNLOAD_LINK
        try:
            java.SELENIUM_DOWNLOAD_LINK = base_link + "/selenium-server-standalone-2.46.0.jar"
            java.TESTNG_DOWNLOAD_LINK = base_link + "/testng-6.8.5.jar"
            java.HAMCREST_DOWNLOAD_LINK = base_link + "/hamcrest-core-1.3.jar"

            self.assertFalse(exists(dummy_installation_path))

            self.obj.settings.merge({
                "selenium-server": join(dummy_installation_path, "selenium-server.jar"),
                "hamcrest-core": join(dummy_installation_path, "tools", "testng", "hamcrest-core.jar"),
                "path": join(dummy_installation_path, "tools", "testng", "testng.jar")
            })

            self.obj.execution.merge({
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/testng/jars/testng-suite.jar"}})
            self.obj.install_required_tools()
            self.obj.prepare()
            self.assertTrue(exists(join(dummy_installation_path, "selenium-server.jar")))
            self.assertTrue(exists(join(dummy_installation_path, "tools", "testng", "testng.jar")))
            self.assertTrue(exists(join(dummy_installation_path, "tools", "testng", "hamcrest-core.jar")))
        finally:
            java.SELENIUM_DOWNLOAD_LINK = selenium_server_link
            java.TESTNG_DOWNLOAD_LINK = testng_link
            java.HAMCREST_DOWNLOAD_LINK = hamcrest_link

    def test_failed_setup(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/testng/TestNGFailingSetup.java"}})
        self.obj.settings['autodetect-xml'] = False
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        samples = [
            json.loads(line)
            for line in open(os.path.join(self.obj.engine.artifacts_dir, 'TestNGTester.ldjson')).readlines()
        ]
        self.assertEqual(samples[0]["status"], "FAILED")
        self.assertEqual(samples[1]["status"], "SKIPPED")


class TestJavaC(BZTestCase):
    def test_missed_tool(self):
        self.obj = JavaC(logging.getLogger(''), tool_path='javac-not-found')
        self.assertEqual(False, self.obj.check_if_installed())
        self.assertRaises(ToolError, self.obj.install)


class TestJUnitTester(BZTestCase):
    def setUp(self):
        super(TestJUnitTester, self).setUp()
        engine_obj = EngineEmul()
        paths = [local_paths_config()]
        engine_obj.configure(paths)

        # just download geckodriver & chromedriver with selenium
        selenium = SeleniumExecutor()
        selenium.engine = engine_obj
        selenium.env = selenium.engine.env
        selenium.execution.merge({"scenario": {"requests": ["req"]}})
        selenium.prepare()

        self.obj = JUnitTester()
        self.obj.settings = engine_obj.config.get("modules").get("junit")
        self.obj.engine = engine_obj
        self.obj.env = self.obj.engine.env

    def test_install_tools(self):
        """
        check installation of selenium-server, junit
        :return:
        """
        dummy_installation_path = BUILD_DIR + "selenium-taurus"
        base_link = "file:///" + RESOURCES_DIR + "selenium"

        shutil.rmtree(dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = java.SELENIUM_DOWNLOAD_LINK
        junit_link = java.JUNIT_DOWNLOAD_LINK
        junit_mirrors = java.JUNIT_MIRRORS_SOURCE
        hamcrest_link = java.HAMCREST_DOWNLOAD_LINK
        try:
            java.SELENIUM_DOWNLOAD_LINK = base_link + "/selenium-server-standalone-2.46.0.jar"
            java.JUNIT_DOWNLOAD_LINK = base_link + "/junit-4.12.jar"
            java.JUNIT_MIRRORS_SOURCE = base_link + "unicode_file"
            java.HAMCREST_DOWNLOAD_LINK = base_link + "/hamcrest-core-1.3.jar"

            self.assertFalse(exists(dummy_installation_path))

            self.obj.settings.merge({
                "selenium-server": join(dummy_installation_path, "selenium-server.jar"),
                "hamcrest-core": join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar"),
                "path": join(dummy_installation_path, "tools", "junit", "junit.jar")
            })

            self.obj.execution.merge({
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/junit/jar/"},
                "runner": "junit"})
            self.obj.install_required_tools()
            self.obj.prepare()
            self.assertIsInstance(self.obj, JUnitTester)
            self.assertTrue(exists(join(dummy_installation_path, "selenium-server.jar")))
            self.assertTrue(exists(join(dummy_installation_path, "tools", "junit", "junit.jar")))
            self.assertTrue(
                exists(join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar")))
        finally:
            java.SELENIUM_DOWNLOAD_LINK = selenium_server_link
            java.JUNIT_DOWNLOAD_LINK = junit_link
            java.HAMCREST_DOWNLOAD_LINK = hamcrest_link
            java.JUNIT_MIRRORS_SOURCE = junit_mirrors

    def test_simple(self):
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "BlazeDemo.java"}})
        self.obj.prepare()
        self.obj.engine.aggregator.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        self.assertTrue(self.obj.has_results())
        self.assertEqual(1, self.obj.engine.aggregator.cumulative[''][KPISet.SUCCESSES])


class TestSeleniumJUnitTester(SeleniumTestCase):
    """
    :type obj: bzt.modules.selenium.SeleniumExecutor
    """
    def __init__(self, methodName='runTest'):
        super(TestSeleniumJUnitTester, self).__init__(methodName)
        self.obj = None

    def test_junit_mirrors(self):
        dummy_installation_path = BUILD_DIR + "selenium-taurus"
        shutil.rmtree(dirname(dummy_installation_path), ignore_errors=True)
        junit_path = join(dummy_installation_path, "tools", "junit", "junit.jar")
        objjm = JUnitJar(junit_path, logging.getLogger(), JUNIT_VERSION)
        objjm.install()

    def test_prepare_java_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "selenium/junit/java/TestBlazemeterFail.java"}
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertFalse(exists(join(self.obj.runner.working_dir, "TestBlazemeterFail.java")))
        self.assertTrue(exists(join(self.obj.runner.working_dir, "TestBlazemeterFail.class")))
        self.assertTrue(exists(join(self.obj.runner.working_dir, "compiled.jar")))

    def test_prepare_java_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/java/"}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
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
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/java_package/"}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertTrue(exists(join(self.obj.runner.working_dir, "compiled.jar")))

    def test_selenium_startup_shutdown_java_package(self):
        """
        Run tests from package
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java_package/src'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertTrue(exists(join(self.obj.runner.working_dir, "compiled.jar")))

    def test_prepare_jar_single(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/jar/dummy.jar"}})
        self.obj.prepare()

    def test_prepare_jar_folder(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/jar/"}})
        self.obj.prepare()

    def test_selenium_startup_shutdown_jar_single(self):
        """
        runt tests from single jar
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
                'runner': 'junit',
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JUnitTester)
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)
        self.assertTrue(exists(self.obj.runner.report_file))

    def test_selenium_startup_shutdown_jar_folder(self):
        """
        run tests from jars
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)
        self.assertTrue(exists(self.obj.runner.report_file))

    def test_selenium_startup_shutdown_java_single(self):
        """
        run tests from single .java file
        :return:
        """
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "selenium/junit/java/TestBlazemeterFail.java"}
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))
        self.assertEqual(1, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(exists(join(self.obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(exists(self.obj.runner.report_file))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        run tests from .java files
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })

        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))
        self.assertEqual(2, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(exists(join(self.obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(exists(self.obj.runner.report_file))

    def test_not_junit(self):
        """
        Check that JUnit runner fails if no tests were found
        :return:
        """
        self.configure({
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": RESOURCES_DIR + "selenium/invalid/NotJUnittest.java"}}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JUnitTester)
        self.obj.startup()
        try:
            while not self.obj.check():
                time.sleep(1)
            self.fail()
        except ToolError as exc:
            diagnostics = "\n".join(exc.diagnostics)
            self.assertIn("Nothing to test", diagnostics)
        except BaseException as exc:
            logging.debug(traceback.format_exc())
            self.fail("Unexpected exception %s, expected ToolError" % exc)
        self.obj.shutdown()

    def test_resource_files_collection_remote_java(self):
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)

    def test_additional_classpath(self):
        scenario_cp = 'class_path_from_scenario'
        settings_cp = 'class_path_from_settings'
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/junit/java/',
                    'additional-classpath': [scenario_cp]},
                'executor': 'selenium', },
            'modules': {
                'junit': {
                    'additional-classpath': [settings_cp]}}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        base_class_path = ':'.join(self.obj.runner.base_class_path)
        self.assertIn(scenario_cp, base_class_path)
        self.assertIn(settings_cp, base_class_path)

    def test_resource_files_collection_remote_jar(self):
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)


class TestSeleniumTestNGRunner(SeleniumTestCase):
    def test_prepare_java_package(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1.0)
        self.obj.shutdown()
        self.obj.post_process()
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 3)

    def test_prepare_java_file(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/TestNGSuite.java'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False
                }}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1.0)
        self.obj.shutdown()
        self.obj.post_process()
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 3)

    def test_resource_files(self):
        script_jar = RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'
        self.configure({
            'execution': {
                'testng-xml': 'testng.xml',
                'scenario': {
                    'script': script_jar,
                },
                'runner': 'testng',
            },
        })
        resources = self.obj.get_resource_files()
        self.assertEqual(resources, [script_jar, 'testng.xml'])

    def test_resource_files_detect_config(self):
        script_jar = RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'
        self.configure({
            'execution': {
                'scenario': {
                    'script': script_jar,
                },
                'runner': 'testng',
            },
        })
        resources = self.obj.get_resource_files()
        self.assertEqual(resources, [script_jar,
                                     get_full_path(RESOURCES_DIR + 'selenium/testng/jars/testng.xml')])

    def test_hold(self):
        self.configure({
            'execution': {
                'hold-for': '5s',
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 5)

    def test_iterations(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 9)

    def test_with_testng_config(self):
        self.configure({
            'execution': {
                'testng-xml': RESOURCES_DIR + 'selenium/testng/jars/testng.xml',
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 6)

    def test_testng_config_autodetect(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 6)

    def test_autodetect_script_type(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar',
                },
            },
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, TestNGTester)

    def test_detect_testng_xml_with_config(self):
        test_yml = RESOURCES_DIR + "selenium/testng/test.yml"
        self.obj.engine.config.merge(yaml.load(open(test_yml)))
        self.obj.execution = self.obj.engine.config.get('execution')
        self.obj.engine.file_search_paths.append(dirname(test_yml))
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, TestNGTester)
