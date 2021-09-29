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

    def setUp(self):
        super(SeleniumTestCase, self).setUp()

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
        super(SeleniumTestCase, self).tearDown()


class MockPythonTool(RequiredTool):
    tool_name = "MockPythonTool"

    def __init__(self, engine, settings, **kwargs):
        pass

    def check_if_installed(self):
        return False

    def install(self):
        pass

    def post_process(self):
        pass
