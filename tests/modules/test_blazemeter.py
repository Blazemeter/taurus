import logging

import requests

from bzt import TaurusNetworkError
from bzt.bza import BZAObject, User
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning
from bzt.utils import to_json
from tests import BZTestCase
from tests.mocks import EngineEmul, ModuleMock


class BZMock(object):
    def __init__(self, obj=None):
        """
        :type obj: bzt.bza.BZAObject
        """
        super(BZMock, self).__init__()
        locs = [{'id': 'aws', 'sandbox': False, 'title': 'AWS'},
                {'id': 'us-east-1', 'sandbox': False, 'title': 'East'},
                {'id': 'us-west', 'sandbox': False, 'title': 'Dallas (Rackspace)'},
                {'id': 'harbor-sandbox', 'sandbox': True, 'title': 'Sandbox'},
                {'id': 'non-harbor-sandbox', 'sandbox': True, 'title': 'Sandbox Neverexisting'}, ]
        self.mock_get = {
            'https://a.blazemeter.com/api/v4/web/version': {},
            'https://a.blazemeter.com/api/v4/user': {'defaultProject': {'id': None}, "locations": locs},
            'https://a.blazemeter.com/api/v4/accounts': {"result": [{'id': 1}]},
            'https://a.blazemeter.com/api/v4/workspaces?accountId=1': {"result": [{'id': 1}]},
            'https://a.blazemeter.com/api/v4/multi-tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=99999': {"result": []},
            'https://a.blazemeter.com/api/v4/web/elfinder/1?cmd=open&target=s1_Lw': {"files": []},
            'https://a.blazemeter.com/api/v4/web/elfinder/1?target=s1_Lw&cmd=open': {"files": []},
            'https://a.blazemeter.com/api/v4/workspaces/1': {"result": {"locations": locs}},
        }

        self.mock_post = {}
        self.mock_patch = {}
        self.requests = []

        if obj is not None:
            self.apply(obj)

    def apply(self, obj):
        obj.http_request = self._request_mock

    def _request_mock(self, method, url, **kwargs):
        """
        :param method:
        :param url:
        :param kwargs:
        :rtype: requests.Response
        """
        # TODO: make it simplier, mocking and replacing requests.request of BZAObject
        if method == 'GET':
            resp = self.mock_get[url]
        elif method == 'POST':
            resp = self.mock_post[url]
        elif method == 'PATCH':
            resp = self.mock_patch[url]
        else:
            raise ValueError()

        response = requests.Response()

        if isinstance(resp, list):
            resp = resp.pop(0)

        data = kwargs['data']
        logging.debug("Emulated %s %s %s: %s", method, url, data, resp)
        self.requests.append({"method": method, "url": url, "data": data})
        if isinstance(resp, BaseException):
            raise resp
        response._content = to_json(resp)
        response.status_code = 200
        return response


class TestBZAObject(BZTestCase):
    def test_ping(self):
        obj = User()
        obj.ping()

    def test_request(self):
        obj = BZAObject()
        try:
            obj._request('https://a.blazemeter.com/api/v4/web/version', data={"test": 1})
            self.fail()
        except TaurusNetworkError:
            pass


class TestCloudProvisioningOld(BZTestCase):
    def test_case1(self):
        mock = BZMock()

        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {"sessions": []}},
            'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {"sessions": []}},
            'https://a.blazemeter.com/api/v4/masters/1': {"result": {"note": "message"}},
            'https://a.blazemeter.com/api/v4/masters/1/status': [
                {"result": {"id": 1, "status": "CREATE"}},
                {"result": {"id": 1, "status": "ENDED", "progress": 101}}
            ],
        })

        mock.mock_post = {
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests/1/files': {"result": None},
            'https://a.blazemeter.com/api/v4/tests/1/start': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/masters/1/stop': {"result": None},
            'https://a.blazemeter.com/api/v4/masters/1/public-token': {"result": {"publicToken": "token"}},
        }

        mock.mock_patch = {
            'https://a.blazemeter.com/api/v4/tests/1': {"result": {}}
        }

        prov = CloudProvisioning()
        prov.browser_open = None
        prov.public_report = True
        prov.user.token = "test"
        prov.engine = EngineEmul()
        prov.engine.aggregator = ConsolidatingAggregator()
        # prov.engine.config.merge({"modules": {"blazemeter": {"browser-open": False}}})
        prov.engine.config[ScenarioExecutor.EXEC] = [{
            "executor": "mock",
            "locations": {
                "aws": 1
            },
            "files": ModuleMock().get_resource_files()
        }]
        mock.apply(prov.user)

        prov.prepare()
        prov.startup()
        prov.check()
        prov._last_check_time = 0
        prov.check()
        prov.shutdown()
        prov.post_process()
