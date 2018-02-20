from tests import BZTestCase, local_paths_config
from bzt.modules.selenium import SeleniumExecutor
from tests.mocks import EngineEmul
from bzt.modules.services import VirtualDisplay


class SeleniumTestCase(BZTestCase):
    """
    :type obj: SubprocessedExecutor
    """
    def __init__(self, methodName='runTest'):
        super(SeleniumTestCase, self).__init__(methodName)
        self.obj = None

    def setUp(self):
        super(SeleniumTestCase, self).setUp()
        self.engine = EngineEmul()
        paths = [local_paths_config()]
        self.engine.configure(paths)  # FIXME: avoid using whole engine in particular module test!

        self.virtual_display = VirtualDisplay()
        self.virtual_display.engine = self.engine
        self.virtual_display.startup()

        self.obj = SeleniumExecutor()
        self.obj.engine = self.engine
        self.obj.settings = self.engine.config.get("modules").get("selenium")
        self.obj.env = self.obj.engine.env

    def tearDown(self):
        self.virtual_display.shutdown()
        super(SeleniumTestCase, self).tearDown()

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config.get('execution')
        if isinstance(self.obj.execution, list):
            self.obj.execution = self.obj.execution[0]

