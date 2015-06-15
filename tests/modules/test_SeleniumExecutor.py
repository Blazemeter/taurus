from tests import setup_test_logging
from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.selenium import SeleniumExecutor, Maven
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import os
import shutil
import yaml
import time

setup_test_logging()

class TestSeleniumExecutor(BZTestCase):

    def test_install_maven(self):
        """
        Test maven installation
        :return:
        """

        maven_config = BetterDict()
        maven_config.merge({"path":__dir__() + "/../../build/tmp/selenium-taurus/tools/maven/bin/mvn"})
        maven_dir = os.path.dirname(os.path.dirname(maven_config.get("path")))
        if os.path.exists(maven_dir):
            shutil.rmtree(maven_dir)
        maven = Maven(maven_config)
        self.assertFalse(maven.check_if_installed())
        maven.install()
        self.assertTrue(maven.check_if_installed())

    def test_install_selenium_server(self):
        """
        Test selenium_server installation
        :return:
        """

        path = os.path.abspath(__dir__() + "/../../build/tmp/selenium-taurus/selenium-server.jar")

        shutil.rmtree(os.path.dirname(path), ignore_errors=True)

        selenium_server_link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK
        selenium_server_version = SeleniumExecutor.SELEINUM_VERSION
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = "file://" + __dir__() + "/../data/selenium-server-standalone-2.46.0.jar"
        SeleniumExecutor.SELEINUM_VERSION = "2.46"
        self.assertFalse(os.path.exists(path))

        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        obj.execution = BetterDict()
        obj.prepare()
        self.assertTrue(os.path.exists(path))
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        SeleniumExecutor.SELEINUM_VERSION = selenium_server_version

    def test_selenium_prepare_folder_java(self):
        """
        Check if scripts copied
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
        Check if scripts copied python
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
        Check if scripts copied and test completed
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        self.assertNotEqual(obj.hub_process.poll(), 1)
        self.assertEqual(obj.test_runner.process.poll(), 0)

    def test_selenium_startup_shutdown_java_maven_single(self):
        """
        test if script copied and test completed with single java file
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/java/TestBlazemeterPass.java")}})
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        self.assertNotEqual(obj.hub_process.poll(), 1)
        self.assertEqual(obj.test_runner.process.poll(), 0)

    def test_selenium_startup_shutdown_python(self):
        """
        test if script copied and test completed with single java file
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/python/")}})
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        self.assertNotEqual(obj.hub_process.poll(), 1)
        self.assertEqual(obj.test_runner.process.poll(), 0)

    def test_selenium_prepare_single_jar(self):
        """
        Check if scripts copied
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/TestBlazemeterPass.jar")}})
        obj.prepare()
        java_scripts = os.listdir(os.path.join(obj.engine.artifacts_dir, "selenium_scripts"))
        self.assertEqual(len(java_scripts), 1)

    def test_selenium_startup_shutdown_jar(self):
        """
        test if script copied and test completed with single java file
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/selenium_executor.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        #obj.execution.merge({"scenario": {"script": os.path.abspath(__dir__() + "/../../tests/selenium/jar/TestBlazemeterPass.jar")}})
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        self.assertNotEqual(obj.hub_process.poll(), 1)
        self.assertEqual(obj.test_runner.process.poll(), 0)