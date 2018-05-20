import logging
import os
import sys

from bzt import NormalShutdown
from bzt.bza import WDGridImages
from bzt.engine import Configuration
from bzt.modules.blazemeter import WDGridProvisioning
from bzt.utils import to_json
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
        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_engines(self):
        self.obj.settings['dump-status'] = True
        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_cleanup(self):
        self.obj.settings['cleanup-engines'] = True
        self.assertRaises(NormalShutdown, self.obj.prepare)
        client = WDGridImages(self.obj.user)
        self.assertEquals(0, len([x for x in client.get_engines() if x['status'] != 'Terminating']))

    def test_flow(self):
        self.obj.engine.config.merge({"execution": [
            {
                "executor": "mock",
                "scenario": "scen1",
                "grid": [
                    {
                        "platform": "ubuntu/14.04",
                        "browser": "chrome/46.0.12",
                    },
                    # {
                    #    "platform": "ubuntu/14.04",
                    #    "browser": "firefox/46.0.12",
                    # }
                ]
            },
            {
                "executor": "mock",
                "scenario": "scen2",
            }
        ]})
        self.obj.settings['auto-cleanup']=True

        self.obj.prepare()
        #logging.info(to_json(self.obj.engine.config))
        # self.assertEquals(3, len(self.obj.executors))
        self.obj.post_process()
