import bzt
import bzt.modules._apiritif
from bzt.utils import RequiredTool
from tests.unit import local_paths_config, ExecutorTestCase
from bzt.modules._selenium import SeleniumExecutor
from bzt.modules.services import VirtualDisplay


class SeleniumTestCase(ExecutorTestCase):
    """ ExecutorTestCase with virtual display """
    EXECUTOR = SeleniumExecutor

    def __init__(self, methodName='runTest'):
        super(SeleniumTestCase, self).__init__(methodName)
        self.obj = None
        self.tmp_selenium = None
        self.tmp_selenium_apiritif = None

    def setUp(self):
        super(SeleniumTestCase, self).setUp()
        self.tmp_selenium = bzt.modules._selenium.Selenium
        self.tmp_selenium_apiritif = bzt.modules._apiritif.executor.Selenium
        bzt.modules._selenium.Selenium = MockPythonTool
        bzt.modules._apiritif.executor.Selenium = MockPythonTool

        paths = [local_paths_config()]
        self.engine.configure(paths)  # FIXME: avoid using whole engine in particular module test!

        self.virtual_display = VirtualDisplay()
        self.virtual_display.engine = self.engine
        self.virtual_display.startup()

        self.tmp_chromedriver = bzt.modules._selenium.ChromeDriver
        self.tmp_geckodriver = bzt.modules._selenium.GeckoDriver
        bzt.modules._selenium.ChromeDriver = MockDriver
        bzt.modules._selenium.GeckoDriver = MockDriver
        self.obj.settings = self.engine.config.get("modules").get("selenium")

    def tearDown(self):
        self.virtual_display.shutdown()
        if self.obj and self.obj.runner:
            if self.obj.runner.stdout:
                self.obj.runner.stdout.close()
            if self.obj.runner.stderr:
                self.obj.runner.stderr.close()
        bzt.modules._selenium.Selenium = self.tmp_selenium
        bzt.modules._apiritif.executor.Selenium = self.tmp_selenium_apiritif
        bzt.modules._selenium.ChromeDriver = self.tmp_chromedriver
        bzt.modules._selenium.GeckoDriver = self.tmp_geckodriver
        super(SeleniumTestCase, self).tearDown()


class MockPythonTool(RequiredTool):
    tool_name = "MockPythonTool"
    version = ""
    called = False

    def __init__(self, engine, settings, **kwargs):
        pass

    def check_if_installed(self):
        return False

    def install(self):
        self.called = True

    def get_version(self):
        return self.version

    def post_process(self):
        pass


class MockDriver(RequiredTool):
    tool_name = "MockDriver"
    tool_path = ""

    def __init__(self, **kwargs):
        pass

    def check_if_installed(self):
        return True

    @staticmethod
    def get_driver_dir():
        return ""

    def install(self):
        pass
