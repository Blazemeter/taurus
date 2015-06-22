from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.selenium import SeleniumExecutor
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import os
import shutil
import yaml
import time

setup_test_logging()


class TestSeleniumExecutor(BZTestCase):
    def test_install_selenium_server(self):
        """
        Test selenium_server installation
        :return:
        """

        path = os.path.abspath(__dir__() + "/../../build/tmp/selenium-taurus/selenium-server.jar")

        shutil.rmtree(os.path.dirname(path), ignore_errors=True)

        selenium_server_link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK
        selenium_server_version = SeleniumExecutor.SELENIUM_VERSION
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = "file://" + __dir__() + \
                                                  "/../data/selenium-server-standalone-2.46.0.jar"
        SeleniumExecutor.SELENIUM_VERSION = "2.46"
        self.assertFalse(os.path.exists(path))

        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/")}})
        obj.prepare()
        self.assertTrue(os.path.exists(path))
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        SeleniumExecutor.SELENIUM_VERSION = selenium_server_version

    def test_selenium_prepare_folder_java(self):
        """
        Check if scripts copied, folder, .java
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/")}})
        obj.prepare()
        java_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(java_scripts), 2)

    def test_selenium_prepare_folder_python(self):
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

    def test_selenium_startup_shutdown_java_maven(self):
        """
        Check if scripts copied and test completed with folder, java
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
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
        self.assertIn("reports", [os.path.basename(folder_path) for folder_path in result_files])
        self.assertIn("compiled", [os.path.basename(folder_path) for folder_path in result_files])

    def test_selenium_startup_shutdown_java_maven_single(self):
        """
        Check if script copied and test completed with single java file
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/TestBlazemeterPass.java")}})
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        result_files = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        java_scripts = [script for script in result_files if script.endswith(".java")]
        self.assertEqual(1, len(java_scripts))
        self.assertIn("reports", [os.path.basename(folder_path) for folder_path in result_files])
        self.assertIn("compiled", [os.path.basename(folder_path) for folder_path in result_files])

    def test_selenium_startup_shutdown_python(self):
        """
        Check if script copied and test completed with folder, python
        :return:
        """
        # FIXME: Will fail on py3

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
        compiled_scripts = [script for script in result_files if script.endswith(".pyc")]
        self.assertEqual(2, len(python_scripts))
        self.assertEqual(2, len(compiled_scripts))

    def test_selenium_prepare_single_jar(self):
        """
        Check if script copied with single jar file
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge(
            {"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/selenium-test-small.jar")}})
        obj.prepare()
        java_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(java_scripts), 1)

    def test_selenium_startup_shutdown_jar(self):
        """
        Check if jar copied and test completed with single jar file
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
