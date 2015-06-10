from tests import setup_test_logging
from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.selenium import SeleniumExecutor, Maven
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import os
import shutil

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

    def test_startup_shutdown(self):
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()


