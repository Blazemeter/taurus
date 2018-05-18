import os
import sys

from bzt import NormalShutdown
from bzt.engine import Configuration
from bzt.modules.blazemeter import WDGridProvisioning
from tests import BZTestCase
from tests.mocks import EngineEmul

env = Configuration()
env.load([os.path.expanduser("~/.bzt-rc")])


class TestWDGrid(BZTestCase):
    def setUp(self):
        super(TestWDGrid, self).setUp()
        self.obj = WDGridProvisioning()
        self.obj.engine = EngineEmul()
        # mock = BZMock(obj.user)
        # obj.settings['token'] = "FakeToken"

        self.obj.settings['address'] = env['cli-aliases']['env-vitali']['modules']['blazemeter']['address']
        self.obj.settings['token'] = env['cli-aliases']['env-vitali']['modules']['blazemeter']['token']
        self.obj.settings['request-logging-limit'] = sys.maxsize

    def test_catalog(self):
        self.obj.settings['dump-catalog'] = True

        self.obj.engine.config.merge({"execution": [
            {
                "executor": "mock"
            }
        ]})

        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_engines(self):
        self.obj.settings['dump-status'] = True
        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_flow(self):
        self.obj.engine.config.merge({"execution": [
            {
                "executor": "mock"
            }
        ]})

        self.obj.prepare()
