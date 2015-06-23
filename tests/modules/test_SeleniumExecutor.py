from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.selenium import SeleniumExecutor
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import os
import shutil
import yaml
import time

setup_test_logging()


class TestSeleniumJUnitRunner(BZTestCase):
    """

    java:one/folder/project/list
    jar:one/folder/list
    python:one/folder/list
    """

    def test_install_tools(self):
        """
        check installation of selenium-server, junit, junit_plugin
        :return:
        """
        dummy_installation_path = os.path.abspath(__dir__() + "/../../build/tmp/selenium-taurus")
        base_link = "file://" + __dir__() + "/../data/"
        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = base_link + "selenium-server-standalone-2.46.0.jar"

        junit_link = SeleniumExecutor.JUNIT_DOWNLOAD_LINK
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = base_link + "junit-4.12.jar"

        junit_listener_link = SeleniumExecutor.JUNIT_LISTENER_LINK
        SeleniumExecutor.JUNIT_LISTENER_LINK = base_link + "junit_listener.jar"

        self.assertFalse(os.path.exists(dummy_installation_path))
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"selenium-tools": {
        "junit": {"selenium-server": os.path.join(dummy_installation_path, "selenium-server.jar")}}})
        obj.settings.merge({"selenium-tools": {
        "junit": {"path": os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")}}})
        obj.settings.merge({"selenium-tools": {"junit": {
        "junit-listener": os.path.join(dummy_installation_path, "tools", "junit-listener", "junit_listener.jar")}}})
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/")}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "selenium-server.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")))
        self.assertTrue(
            os.path.exists(os.path.join(dummy_installation_path, "tools", "junit-listener", "junit_listener.jar")))

        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = junit_link
        SeleniumExecutor.JUNIT_LISTENER_LINK = junit_listener_link

    def test_prepare_java_single(self):
        """
        Check if script copied
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/TestBlazemeterFail.java")}})
        obj.prepare()
        self.assertTrue(
            os.path.exists(os.path.join(obj.engine.artifacts_dir, "selenium_scripts", "TestBlazemeterFail.java")))

    def test_prepare_java_folder(self):
        """
        Check if script copied
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/")}})
        obj.prepare()
        java_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(java_scripts), 2)

    def test_prepare_jar_single(self):
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/selenium-test-small.jar")}})
        obj.prepare()
        self.assertTrue(
            os.path.exists(os.path.join(obj.engine.artifacts_dir, "selenium_scripts", "selenium-test-small.jar")))

    def test_prepare_jar_folder(self):
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/")}})
        obj.prepare()
        java_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(java_scripts), 3)

    def test_selenium_startup_shutdown_jar_single(self):
        """
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor_jar.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/selenium-test-small.jar")}})
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    def test_selenium_startup_shutdown_jar_folder(self):
        """
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

    def test_selenium_startup_shutdown_java_single(self):
        """
        Check if jar copied and test completed with single jar file
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

        result_files = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        java_scripts = [script for script in result_files if script.endswith(".java")]
        java_classes = [script for script in result_files if script.endswith(".class")]
        self.assertEqual(1, len(java_scripts))
        self.assertEqual(1, len(java_classes))
        self.assertTrue(os.path.exists(os.path.join(obj.engine.artifacts_dir, "selenium_scripts", "compiled.jar")))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        Check if jar copied and test completed with single jar file
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

        result_files = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        java_scripts = [script for script in result_files if script.endswith(".java")]
        self.assertEqual(2, len(java_scripts))


class TestSeleniumNoseRunner(BZTestCase):
    def test_selenium_prepare_python_single(self):
        """
        Check if scripts copied, folder, python
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {
        "script": os.path.abspath(__dir__() + "/../../tests/selenium/python/test_blazemeter_fail.py")}})
        obj.prepare()
        python_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(python_scripts), 1)

    def test_selenium_prepare_python_folder(self):
        """
        Check if scripts copied, folder, python
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/python/")}})
        obj.prepare()
        python_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(python_scripts), 2)

    def test_selenium_startup_shutdown_python_single(self):
        """
        Check if script copied and test completed with folder, python
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
        self.assertTrue(os.path.exists(os.path.join(obj.engine.artifacts_dir, "selenium_scripts", "test_blazemeter_fail.py")))

    def test_selenium_startup_shutdown_python_folder(self):
        """
        Check if script copied and test completed with folder, python
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
        result_files = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        python_scripts = [script for script in result_files if script.endswith(".py")]
        self.assertEqual(2, len(python_scripts))
