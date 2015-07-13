from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.selenium import SeleniumExecutor
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import os
import shutil
import yaml
import time
import csv

setup_test_logging()


class TestSeleniumJUnitRunner(BZTestCase):
    """

    java:one/folder/project/list
    jar:one/folder/list
    python:one/folder/list
    """

    def test_install_tools(self):
        """
        check installation of selenium-server, junit
        :return:
        """
        dummy_installation_path = os.path.abspath(__dir__() + "/../../build/tmp/selenium-taurus")
        base_link = "file://" + __dir__() + "/../data/"

        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = base_link + "selenium-server-standalone-2.46.0.jar"

        junit_link = SeleniumExecutor.JUNIT_DOWNLOAD_LINK
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = base_link + "junit-4.12.jar"

        self.assertFalse(os.path.exists(dummy_installation_path))
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"selenium-tools": {
            "junit": {"selenium-server": os.path.join(dummy_installation_path, "selenium-server.jar")}}})
        obj.settings.merge({"selenium-tools": {
            "junit": {"path": os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")}}})

        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/")}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "selenium-server.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")))

        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = junit_link

    def test_prepare_java_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/TestBlazemeterFail.java")}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "TestBlazemeterFail.java")))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "TestBlazemeterFail.class")))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))

    def test_prepare_java_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/")}})
        obj.prepare()
        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [file for file in prepared_files if file.endswith(".java")]
        class_files = [file for file in prepared_files if file.endswith(".class")]
        jars = [file for file in prepared_files if file.endswith(".jar")]
        self.assertEqual(len(java_files), 2)
        self.assertEqual(len(class_files), 2)
        self.assertEqual(len(jars), 1)

    def test_prepare_java_package(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java_package/")}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))

    def test_selenium_startup_shutdown_java_package(self):
        """
        Run tests from package
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_java_package.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']

        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))

    def test_prepare_jar_single(self):
        """
        Check if jar exists in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/dummy.jar")}})
        obj.prepare()
        self.assertTrue(
            os.path.exists(os.path.join(obj.runner.working_dir, "dummy.jar")))

    def test_prepare_jar_folder(self):
        """
        Check if jars exist in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/")}})
        obj.prepare()
        java_scripts = os.listdir(obj.runner.working_dir)
        self.assertEqual(len(java_scripts), 2)

    def test_selenium_startup_shutdown_jar_single(self):
        """
        runt tests from single jar
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_jar.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/dummy.jar")}})
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [file for file in prepared_files if file.endswith(".java")]
        class_files = [file for file in prepared_files if file.endswith(".class")]
        jars = [file for file in prepared_files if file.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 1)
        self.assertTrue(os.path.exists(obj.runner.report_file))

    def test_selenium_startup_shutdown_jar_folder(self):
        """
        run tests from jars
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_jar.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [file for file in prepared_files if file.endswith(".java")]
        class_files = [file for file in prepared_files if file.endswith(".class")]
        jars = [file for file in prepared_files if file.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 2)
        self.assertTrue(os.path.exists(obj.runner.report_file))

    def test_selenium_startup_shutdown_java_single(self):
        """
        run tests from single .java file
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_java.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/TestBlazemeterFail.java")}})
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [file for file in prepared_files if file.endswith(".java")]
        class_files = [file for file in prepared_files if file.endswith(".class")]
        jars = [file for file in prepared_files if file.endswith(".jar")]
        self.assertEqual(1, len(java_files))
        self.assertEqual(1, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(obj.runner.report_file))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        run tests from .java files
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_java.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [file for file in prepared_files if file.endswith(".java")]
        class_files = [file for file in prepared_files if file.endswith(".class")]
        jars = [file for file in prepared_files if file.endswith(".jar")]
        self.assertEqual(2, len(java_files))
        self.assertEqual(2, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(obj.runner.report_file))

    def test_not_junit(self):
        """
        Check that JUnit runner fails if no tests were found
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/NotJUnittest.java"}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(1)
            self.fail()
        except BaseException as exc:
            self.assertEqual(exc.args[0], "Test runner JunitTester has failed: There is nothing to test.")
        obj.shutdown()


class TestSeleniumNoseRunner(BZTestCase):
    def test_selenium_prepare_python_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {
            "script": os.path.abspath(__dir__() + "/../../tests/selenium/python/test_blazemeter_fail.py")}})
        obj.prepare()
        python_scripts = os.listdir(obj.runner.working_dir)
        self.assertEqual(len(python_scripts), 1)

    def test_selenium_prepare_python_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/python/")}})
        obj.prepare()
        python_scripts = os.listdir(obj.runner.working_dir)
        self.assertEqual(len(python_scripts), 2)

    def test_selenium_startup_shutdown_python_single(self):
        """
        run tests from .py file
        :return:
        """

        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_python.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']

        obj.execution.merge({"scenario": {
            "script": os.path.abspath(__dir__() + "/../../tests/selenium/python/test_blazemeter_fail.py")}})

        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        prepared_files = os.listdir(obj.runner.working_dir)
        python_files = [file for file in prepared_files if file.endswith(".py")]
        self.assertEqual(1, len(python_files))
        self.assertTrue(os.path.exists(obj.runner.report_file))

    def test_selenium_startup_shutdown_python_folder(self):
        """
        run tests from .py files
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_python.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        prepared_files = os.listdir(obj.runner.working_dir)
        python_files = [file for file in prepared_files if file.endswith(".py")]
        self.assertEqual(2, len(python_files))
        self.assertTrue(os.path.exists(obj.runner.report_file))

    def runner_fail_no_test_found(self):
        """
        Check that Python Nose runner fails if no tests were found
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/dummy.py"}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(1)
            self.fail()
        except RuntimeError as exc:
            self.assertIn("Nothing to test.", exc.args[0])
        obj.shutdown()


class TestSeleniumStuff(BZTestCase):
    def test_empty_scenario(self):
        """
        Raise runtime error when no scenario provided
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({"execution": {"executor": "selenium"}})
        obj.execution = obj.engine.config['execution']
        self.assertRaises(RuntimeError, obj.prepare)

    def test_javac_fail(self):
        """
        Test RuntimeError when compilation fails
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/invalid.java"}}})
        obj.execution = obj.engine.config['execution']
        self.assertRaises(RuntimeError, obj.prepare)

    def test_no_supported_files_to_test(self):
        """
        Test RuntimeError raised when no files of known types were found.
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/not_found"}}})
        obj.execution = obj.engine.config['execution']
        self.assertRaises(RuntimeError, obj.prepare)

    def test_samples_count_annotations(self):
        """
        Test exact number of tests when java annotations used
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/SeleniumTest.java"}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        with open(obj.kpi_file) as kpi_fds:
            reader = csv.reader(kpi_fds)
            rows = list(reader)
        self.assertEqual(len(rows), 3)

    def test_samples_count_testcase(self):
        """
        Test exact number of tests when test class extends JUnit TestCase
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/SimpleTest.java"}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        with open(obj.kpi_file) as kpi_fds:
            reader = csv.reader(kpi_fds)
            rows = list(reader)
        self.assertEqual(len(rows), 3)

    def test_no_test_in_name(self):
        """
        Test exact number of tests when annotations used and no "test" in class name
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(
            {"execution": {"executor": "selenium", "scenario": {"script": "tests/selenium/invalid/selenium1.java"}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        with open(obj.kpi_file) as kpi_fds:
            reader = csv.reader(kpi_fds)
            rows = list(reader)
        self.assertEqual(len(rows), 3)
