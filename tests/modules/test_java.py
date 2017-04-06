import logging
import shutil
import time
import traceback

import os
import yaml
from tests import __dir__

from bzt.engine import ScenarioExecutor
from bzt.modules import java
from bzt.modules.java import JUnitTester, JavaTestRunner, TestNGTester, JUnitJar, JUNIT_VERSION
from bzt.utils import get_full_path
from tests.mocks import EngineEmul
from tests.modules.test_SeleniumExecutor import SeleniumTestCase


class TestSeleniumJUnitTester(SeleniumTestCase):
    def test_junit_mirrors(self):
        dummy_installation_path = __dir__() + "/../../build/tmp/selenium-taurus"
        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)
        junit_path = os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")
        objjm = JUnitJar(junit_path, logging.getLogger(), JUNIT_VERSION)
        objjm.install()

    def test_install_tools(self):
        """
        check installation of selenium-server, junit
        :return:
        """
        dummy_installation_path = __dir__() + "/../../build/tmp/selenium-taurus"
        base_link = "file:///" + __dir__() + "/../data/"

        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = java.SELENIUM_DOWNLOAD_LINK
        java.SELENIUM_DOWNLOAD_LINK = base_link + "/selenium-server-standalone-2.46.0.jar"

        junit_link = java.JUNIT_DOWNLOAD_LINK
        junit_mirrors = java.JUNIT_MIRRORS_SOURCE
        java.JUNIT_DOWNLOAD_LINK = base_link + "/junit-4.12.jar"
        java.JUNIT_MIRRORS_SOURCE = base_link + "unicode_file"

        hamcrest_link = java.HAMCREST_DOWNLOAD_LINK
        java.HAMCREST_DOWNLOAD_LINK = base_link + "/hamcrest-core-1.3.jar"

        self.assertFalse(os.path.exists(dummy_installation_path))

        self.obj = JUnitTester()
        self.obj.engine = EngineEmul()
        self.obj.settings.merge({
            "selenium-server": os.path.join(dummy_installation_path, "selenium-server.jar"),
            "hamcrest-core": os.path.join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar"),
            "path": os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")
        })

        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/junit/jar/"},
                                  "runner": "junit"})
        self.obj.install_required_tools()
        self.obj.prepare()
        self.assertIsInstance(self.obj, JUnitTester)
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "selenium-server.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar")))
        java.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        java.JUNIT_DOWNLOAD_LINK = junit_link
        java.HAMCREST_DOWNLOAD_LINK = hamcrest_link
        java.JUNIT_MIRRORS_SOURCE = junit_mirrors

    def test_prepare_java_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        self.obj.execution.merge({
            "scenario": {"script": __dir__() + "/../selenium/junit/java/TestBlazemeterFail.java"}
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertFalse(os.path.exists(os.path.join(self.obj.runner.working_dir, "TestBlazemeterFail.java")))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "TestBlazemeterFail.class")))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))

    def test_prepare_java_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/junit/java/"}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
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
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/junit/java_package/"}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))

    def test_selenium_startup_shutdown_java_package(self):
        """
        Run tests from package
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/java_package/src'},
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
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))

    def test_prepare_jar_single(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/junit/jar/dummy.jar"}})
        self.obj.prepare()

    def test_prepare_jar_folder(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/junit/jar/"}})
        self.obj.prepare()

    def test_selenium_startup_shutdown_jar_single(self):
        """
        runt tests from single jar
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/jar/'},
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
        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))

    def test_selenium_startup_shutdown_jar_folder(self):
        """
        run tests from jars
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/jar/'},
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
        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))

    def test_selenium_startup_shutdown_java_single(self):
        """
        run tests from single .java file
        :return:
        """
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({
            "scenario": {"script": __dir__() + "/../selenium/junit/java/TestBlazemeterFail.java"}
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))
        self.assertEqual(1, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        run tests from .java files
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/java/'},
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
        prepared_files = os.listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))
        self.assertEqual(2, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(self.obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))

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
        self.assertIsInstance(self.obj.runner, JUnitTester)
        self.obj.startup()
        try:
            while not self.obj.check():
                time.sleep(1)
            self.fail()
        except BaseException as exc:
            logging.debug(traceback.format_exc())
            self.assertIn("Nothing to test", exc.args[0])
        self.obj.shutdown()

    def test_resource_files_collection_remote_java(self):
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/java/'},
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
                    'script': __dir__() + '/../selenium/junit/java/',
                    'additional-classpath': [scenario_cp]},
                'executor': 'selenium', },
            'modules': {
                'selenium': {
                    'additional-classpath': [settings_cp]}}})
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertTrue(any(scenario_cp in element for element in self.obj.runner.base_class_path))
        self.assertTrue(any(settings_cp in element for element in self.obj.runner.base_class_path))

    def test_resource_files_collection_remote_jar(self):
        self.configure({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/junit/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)


class TestSeleniumTestNGRunner(SeleniumTestCase):
    def test_install_tools(self):
        dummy_installation_path = __dir__() + "/../../build/tmp/selenium-taurus"
        base_link = "file:///" + __dir__() + "/../data/"

        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = java.SELENIUM_DOWNLOAD_LINK
        java.SELENIUM_DOWNLOAD_LINK = base_link + "/selenium-server-standalone-2.46.0.jar"

        testng_link = java.TESTNG_DOWNLOAD_LINK
        java.TESTNG_DOWNLOAD_LINK = base_link + "/testng-6.8.5.jar"

        hamcrest_link = java.HAMCREST_DOWNLOAD_LINK
        java.HAMCREST_DOWNLOAD_LINK = base_link + "/hamcrest-core-1.3.jar"

        self.assertFalse(os.path.exists(dummy_installation_path))

        self.obj = TestNGTester()
        self.obj.engine = EngineEmul()
        self.obj.settings.merge({
            "selenium-server": os.path.join(dummy_installation_path, "selenium-server.jar"),
            "hamcrest-core": os.path.join(dummy_installation_path, "tools", "testng", "hamcrest-core.jar"),
            "path": os.path.join(dummy_installation_path, "tools", "testng", "testng.jar")
        })

        self.obj.execution.merge({
            "runner": "testng",
            "scenario": {
                "script": __dir__() + "/../selenium/testng/jars/testng-suite.jar",
                'testng-xml': None,
            },
        })
        self.obj.install_required_tools()
        self.obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "selenium-server.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "testng", "testng.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "testng", "hamcrest-core.jar")))
        java.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        java.TESTNG_DOWNLOAD_LINK = testng_link
        java.HAMCREST_DOWNLOAD_LINK = hamcrest_link

    def test_prepare_java_package(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/jars/testng-suite.jar',
                    'testng-xml': None,
                },
                'runner': 'testng',
            },
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1.0)
        self.obj.shutdown()
        self.obj.post_process()
        lines = open(self.obj.report_file).readlines()
        self.assertEqual(len(lines), 3)

    def test_prepare_java_file(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/TestNGSuite.java',
                    'testng-xml': None
                },
                'runner': 'testng',
            },
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1.0)
        self.obj.shutdown()
        self.obj.post_process()
        lines = open(self.obj.report_file).readlines()
        self.assertEqual(len(lines), 3)

    def test_resource_files(self):
        script_jar = __dir__() + '/../selenium/testng/jars/testng-suite.jar'
        self.configure({
            'execution': {
                'scenario': {
                    'script': script_jar,
                    'testng-xml': 'testng.xml',
                },
                'runner': 'testng',
            },
        })
        resources = self.obj.get_resource_files()
        self.assertEqual(resources, [script_jar, 'testng.xml'])

    def test_resource_files_detect_config(self):
        script_jar = __dir__() + '/../selenium/testng/jars/testng-suite.jar'
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
                                     get_full_path(__dir__() + '/../selenium/testng/jars/testng.xml')])

    def test_hold(self):
        self.configure({
            'execution': {
                'hold-for': '5s',
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/jars/testng-suite.jar',
                    'testng-xml': None,
                },
                'runner': 'testng',
            },
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        duration = time.time() - self.obj.start_time
        self.assertGreater(duration, 5)

    def test_iterations(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/jars/testng-suite.jar',
                    'testng-xml': None,
                },
                'runner': 'testng',
            },
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        lines = open(self.obj.runner.execution.get("report-file")).readlines()
        self.assertEqual(len(lines), 9)

    def test_with_testng_config(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/jars/testng-suite.jar',
                    'testng-xml': __dir__() + '/../selenium/testng/jars/testng.xml',
                },
            },
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        lines = open(self.obj.runner.execution.get("report-file")).readlines()
        self.assertEqual(len(lines), 6)

    def test_testng_config_autodetect(self):
        testng_xml_path = get_full_path(__dir__() + '/../selenium/testng/jars/testng.xml')
        self.configure({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/jars/testng-suite.jar',
                },
            },
        })
        self.obj.prepare()
        self.assertEqual(testng_xml_path, self.obj.runner.settings.get("testng-xml", None))
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
        self.assertTrue(os.path.exists(self.obj.runner.execution.get("report-file")))
        lines = open(self.obj.runner.execution.get("report-file")).readlines()
        self.assertEqual(len(lines), 6)

    def test_autodetect_script_type(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../selenium/testng/jars/testng-suite.jar',
                },
            },
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, TestNGTester)

    def test_detect_testng_xml_with_config(self):
        test_yml = __dir__() + "/../selenium/testng/test.yml"
        testng_xml = get_full_path(__dir__() + "/../selenium/testng/testng.xml")
        self.obj.engine.config.merge(yaml.load(open(test_yml)))
        self.obj.execution = self.obj.engine.config.get('execution')
        self.obj.engine.file_search_paths.append(os.path.dirname(test_yml))
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, TestNGTester)
        self.assertEqual(self.obj.runner.settings["testng-xml"], testng_xml)
