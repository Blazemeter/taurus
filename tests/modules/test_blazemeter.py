import logging

from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning
from tests import BZTestCase
from tests.mocks import EngineEmul, ModuleMock


class BZMock(object):
    """
    :type mock_get: dict
    :type mock_post: dict
    """

    def __init__(self, obj=None):
        """
        :type obj: bzt.bza.BZAObject
        """
        super(BZMock, self).__init__()
        self.mock_get = {
            'https://a.blazemeter.com/api/v4/web/version': {},
            'https://a.blazemeter.com/api/v4/user': {'defaultProject': {'id': None}, "locations": [{'id': 'aws'}]},
            'https://a.blazemeter.com/api/v4/accounts': {"result": [{'id': 1}]},
            'https://a.blazemeter.com/api/v4/workspaces?accountId=1': {"result": [{'id': 1}]},
            'https://a.blazemeter.com/api/v4/multi-tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': {"result": []},
        }

        self.mock_post = {}
        self.mock_patch = {}
        self.requests = []

        if obj:
            self.apply(obj)

    def _request_mock(self, url, data=None, headers=None, method=None):
        if method == 'GET' or (not method and not data):
            method = 'GET'
            resp = self.mock_get[url]
        elif method == 'POST' or (not method and data):
            method = 'POST'
            resp = self.mock_post[url]
        elif method == 'PATCH':
            resp = self.mock_patch[url]
        else:
            raise ValueError()

        if isinstance(resp, list):
            ret = resp.pop(0)
        else:
            ret = resp

        logging.debug("Emulated %s %s %s: %s", method, url, data, ret)
        self.requests.append({"url": url, "data": data})
        return ret

    def apply(self, obj):
        obj._request = self._request_mock


class TestCloudProvisioningOld(BZTestCase):
    def test_case1(self):
        mock = BZMock()

        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {"sessions": []}},
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
            'https://a.blazemeter.com/api/v4/masters/1/publicToken': {"result": {"publicToken": "token"}},
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
        prov.user._request = mock._request_mock

        prov.prepare()
        prov.startup()
        prov.check()
        prov._last_check_time = 0
        prov.check()
        prov.shutdown()
        prov.post_process()
