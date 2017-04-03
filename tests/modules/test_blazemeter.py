from bzt import TaurusNetworkError
from tests import BZTestCase

from bzt.bza import BZAObject, User
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning
from tests.mocks import EngineEmul, ModuleMock, BZMock


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
