import bzt
import bzt.modules._apiritif
from bzt.utils import RequiredTool
from tests.unit import local_paths_config, ExecutorTestCase
from bzt.modules._selenium import SeleniumExecutor, ChromeDriver
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
        super(SeleniumTestCase, self).tearDown()


class MockPythonTool(RequiredTool):
    tool_name = "MockPythonTool"
    called = False

    def __init__(self, engine, settings, **kwargs):
        self.version = settings.get("version", "4")

    def check_if_installed(self):
        return False

    def install(self):
        self.called = True

    def get_version(self):
        return self.version

    def post_process(self):
        pass


class MockPythonTool414(RequiredTool):
    tool_name = "MockPythonTool414"
    called = False

    def __init__(self, engine, settings, **kwargs):
        self.version = settings.get("version", "4.1.4")

    def check_if_installed(self):
        return False

    def install(self):
        self.called = True

    def get_version(self):
        return self.version

    def post_process(self):
        pass


class MockPythonTool410(RequiredTool):
    tool_name = "MockPythonTool410"
    called = False

    def __init__(self, engine, settings, **kwargs):
        self.version = settings.get("version", "4.10.0")

    def check_if_installed(self):
        return False

    def install(self):
        self.called = True

    def get_version(self):
        return self.version

    def post_process(self):
        pass


class MockChromeDriver(ChromeDriver):
    def __init__(self, settings, log=None, **kwargs):
        super().__init__(settings=settings, log=log)

    def _get_latest_version_from_inet(self):
        return super().version

    def _get_latest_version(self, driver_name):
        pass
