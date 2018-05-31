import logging
import multiprocessing
import os
import sys
import time

from bzt import NormalShutdown
from bzt.bza import WDGridImages
from bzt.engine import Configuration
from bzt.modules.wdgrid import WDGridProvisioning, start_vnc
from tests import BZTestCase
from tests.mocks import EngineEmul, BZMock

env = Configuration()
env.load([os.path.expanduser("~/.bzt-rc")])


class TestWDGrid(BZTestCase):
    def setUp(self):
        super(TestWDGrid, self).setUp()
        self.obj = WDGridProvisioning()
        self.obj.engine = EngineEmul()
        self.mock = BZMock(self.obj.user)
        self.obj.settings['token'] = "FakeToken"
        self.obj.user.timeout = 5

        # self.obj.settings['address'] = env['cli-aliases']['env-vitali']['modules']['blazemeter']['address']
        # self.obj.settings['token'] = env['cli-aliases']['env-vitali']['modules']['blazemeter']['token']
        self.obj.settings['request-logging-limit'] = sys.maxsize

        self.mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/grid/images': {
                "result": [
                    {
                        "id": "taurus-chrome-selenium",
                        "name": "Chrome on Ubuntu",
                        "tag": "taurus-chrome-selenium",
                        "operatingSystem": "ubuntu",
                        "operatingSystemVersion": "14.04",
                        "browser": "chrome",
                        "browserVersion": "46.0.12",
                        "enabled": True,
                        "description": "Chrome webdriver",
                        "created": 1526909235,
                        "updated": 1526909235
                    },
                    {
                        "id": "taurus-firefox-selenium",
                        "name": "Firefox on Ubuntu",
                        "tag": "taurus-firefox-selenium",
                        "operatingSystem": "ubuntu",
                        "operatingSystemVersion": "14.04",
                        "browser": "firefox",
                        "browserVersion": "46.0.12",
                        "enabled": True,
                        "description": "firefox on ubuntu",
                        "created": 1526909235,
                        "updated": 1526909235
                    }
                ],
            }
        })

    def test_catalog(self):
        self.obj.settings['dump-catalog'] = True
        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_engines(self):
        self.mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/grid/engines?limit=1000': [
                {"result": [{
                    "id": "5afd6c2518cb70ef4a711bb0",
                    "name": "Thu May 17 14:48:23 IDT 2018 - 2",
                    "status": "RUNNING",
                    "expiration": None,
                    "publicIp": None,
                    "created": 1526557733,
                    "updated": 1526557733,
                    "userId": 1,
                    "imageId": "taurus-firefox-selenium",
                    "endpoint": None,
                    "bookingId": "booked",
                    "bookingExpiration": None,
                }, {
                    "id": "5afd6c2518cb70ef4a711bb1",
                    "name": "Thu May 17 14:48:23 IDT 2018 - 2",
                    "status": "Terminating",
                    "expiration": None,
                    "publicIp": None,
                    "created": 1526557733,
                    "updated": 1526557733,
                    "userId": 1,
                    "imageId": "taurus-firefox-selenium",
                    "endpoint": None,
                    "bookingId": "booked",
                    "bookingExpiration": None,
                }
                ]}
            ]})
        self.obj.settings['dump-status'] = True
        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_cleanup(self):
        self.mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/grid/engines?limit=1000': [
                {"result": [{
                    "id": "5afd6c2518cb70ef4a711bb0",
                    "name": "Thu May 17 14:48:23 IDT 2018 - 2",
                    "status": "RUNNING",
                    "expiration": None,
                    "publicIp": None,
                    "created": 1526557733,
                    "updated": 1526557733,
                    "userId": 1,
                    "imageId": "taurus-firefox-selenium",
                    "endpoint": None,
                    "bookingId": "booked",
                    "bookingExpiration": None,
                }]},
                {"result": [{
                    "id": "5afd6c2518cb70ef4a711bb0",
                    "name": "Thu May 17 14:48:23 IDT 2018 - 2",
                    "status": "Terminating",
                    "expiration": None,
                    "publicIp": None,
                    "created": 1526557733,
                    "updated": 1526557733,
                    "userId": 1,
                    "imageId": "taurus-firefox-selenium",
                    "endpoint": None,
                    "bookingId": "booked",
                    "bookingExpiration": None,
                }
                ]}
            ]})
        self.mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2518cb70ef4a711bb0/stop': {}
        })
        self.obj.settings['cleanup-engines'] = True
        self.assertRaises(NormalShutdown, self.obj.prepare)
        client = WDGridImages(self.obj.user)
        self.assertEquals(0, len([x for x in client.get_engines() if x['status'] != 'Terminating']))

    def test_flow(self):
        self.mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/grid/engines?limit=1000': [
                {"result": [{
                    "id": "5afd6c2518cb70ef4a711bb0",
                    "name": "Thu May 17 14:48:23 IDT 2018 - 2",
                    "status": "RUNNING",
                    "expiration": None,
                    "publicIp": None,
                    "created": 1526557733,
                    "updated": 1526557733,
                    "userId": 1,
                    "imageId": "taurus-firefox-selenium",
                    "endpoint": None,
                    "bookingId": "booked",
                    "bookingExpiration": None,
                }, {
                    "id": "5afd6c2518cb70ef4a711bbb",
                    "name": "Thu May 17 14:48:23 IDT 2018 - 2",
                    "status": "RUNNING",
                    "expiration": None,
                    "publicIp": None,
                    "created": 1526557733,
                    "updated": 1526557733,
                    "userId": 1,
                    "imageId": "taurus-firefox-selenium",
                    "endpoint": "http://endpoint",
                    "bookingId": None,
                    "bookingExpiration": None,
                }]}
            ],
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2418cb70ef4a711bba': [
                {
                    "result": {
                        "id": "5afd6c2418cb70ef4a711bba",
                        "name": "Thu May 17 14:48:23 IDT 2018 - 1",
                        "status": "PENDING",
                        "expiration": 1526557732,
                        "publicIp": None,
                        "created": 1526557732,
                        "updated": 1526557732,
                        "userId": 1,
                        "imageId": "taurus-chrome-selenium",
                        "endpoint": None,
                        "bookingId": None,
                        "bookingExpiration": None,
                    }
                },
                {
                    "result": {
                        "id": "5afd6c2418cb70ef4a711bba",
                        "name": "Thu May 17 14:48:23 IDT 2018 - 1",
                        "status": "RUNNING",
                        "expiration": 1526557732,
                        "publicIp": None,
                        "created": 1526557732,
                        "updated": 1526557732,
                        "userId": 1,
                        "imageId": "taurus-chrome-selenium",
                        "endpoint": "http://localhost",
                        "bookingId": None,
                        "bookingExpiration": None,
                    }
                },
            ]

        })

        self.mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/grid/engines': {
                "result": [
                    {
                        "id": "5afd6c2418cb70ef4a711bba",
                        "name": "Thu May 17 14:48:23 IDT 2018 - 1",
                        "status": None,
                        "expiration": None,
                        "publicIp": None,
                        "created": 1526557732,
                        "updated": 1526557732,
                        "userId": 1,
                        "imageId": "taurus-chrome-selenium",
                        "endpoint": None,
                        "bookingId": None,
                        "bookingExpiration": None,
                    },
                ],
            },
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2518cb70ef4a711bbb/book': {"result": {}},
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2518cb70ef4a711bbb/unbook': {"result": {}},
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2418cb70ef4a711bba/book': {"result": {}},
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2418cb70ef4a711bba/unbook': {"result": {}},
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2518cb70ef4a711bbb/stop': {"result": {}},
            'https://a.blazemeter.com/api/v4/grid/engines/5afd6c2418cb70ef4a711bba/stop': {"result": {}},
        })

        self.obj.engine.config.merge({"execution": [
            {
                "executor": "mock",
                "scenario": "scen1",
                "grid": [
                    {
                        "platform": "ubuntu/14.04",
                        "browser": "chrome/46.0.12",
                        #"vnc": True
                    },
                    # {
                    #    "platform": "ubuntu/14.04",
                    #    "browser": "firefox/46.0.12",
                    # }
                ]
            }, {
                "executor": "mock",
                "scenario": "scen1",
                "grid": [
                    {
                        "platform": "ubuntu/14.04",
                        "browser": "firefox/46.0.12",
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
        self.obj.settings['auto-cleanup'] = True

        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        # logging.info(to_json(self.obj.engine.config))
        # self.assertEquals(3, len(self.obj.executors))
        self.obj.shutdown()
        self.obj.post_process()

    def test_vnc(self):
        self.skipTest("for local debug")
        vncs = [("localhost", "secret", "test", 1)]
        _vncs_pool = multiprocessing.Pool(len(vncs), maxtasksperchild=1)
        _vncs_pool.map_async(start_vnc, vncs)
        time.sleep(1)
        _vncs_pool.close()
        _vncs_pool.terminate()
        _vncs_pool.join()
