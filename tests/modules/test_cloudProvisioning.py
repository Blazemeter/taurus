import json

from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ModuleMock


class TestCloudProvisioning(BZTestCase):
    def test_simple(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            "execution": {
                "executor": "mock",
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2
                }
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            }
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings["token"] = "FakeToken"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({})  # upload files
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj)}})  # get master
        client.results.append({})  # terminate

        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()

    def __get_user_info(self):
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())
