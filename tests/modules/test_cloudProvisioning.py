import json
import logging

from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul, ResultsFromBZA
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ModuleMock


class TestCloudProvisioning(BZTestCase):
    def test_simple(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            "execution": {
                "executor": "mock",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2
                }
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']
        obj.engine.aggregator = ConsolidatingAggregator()

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
        widget = obj.get_widget()
        self.assertEquals(2, obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(4, obj.executors[0].execution['locations']['us-west'])

        obj.startup()
        obj.check()
        widget.render((200,), False)
        txt = widget.text.get_text()[0]
        self.assertTrue(txt.endswith("\n  mock machines:\n    us-east-1: 2\n    us-west: 4\n"))
        obj.shutdown()
        obj.post_process()

    def test_no_settings(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            "execution": {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings["token"] = "FakeToken"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({})  # upload files

        obj.prepare()
        self.assertEquals(1, obj.executors[0].execution['locations']['harbor-5591335d8588531f5cde3a04'])

    def __get_user_info(self):
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())


class TestResultsFromBZA(BZTestCase):
    def test_simple(self):
        client = BlazeMeterClientEmul(logging.getLogger())
        client.results.append({
            "api_version": 2,
            "error": None,
            "result": [
                {
                    "id": "ALL",
                    "name": "ALL"
                },
                {
                    "id": "e843ff89a5737891a10251cbb0db08e5",
                    "name": "http://blazedemo.com/"
                }
            ]
        })
        client.results.append({
            "api_version": 2,
            "error": None,
            "result": [
                {
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "label": "ALL",
                    "kpis": [
                        {
                            "n": 15,
                            "na": 2,
                            "ec": 0,
                            "ts": 1442497724,
                            "t_avg": 558,
                            "lt_avg": 25.7,
                            "by_avg": 0,
                            "n_avg": 15,
                            "ec_avg": 0
                        },
                        {
                            "n": 7,
                            "na": 4,
                            "ec": 0,
                            "ts": 1442497725,
                            "t_avg": 88.1,
                            "lt_avg": 11.9,
                            "by_avg": 0,
                            "n_avg": 7,
                            "ec_avg": 0
                        }
                    ]
                },
                {
                    "labelId": "e843ff89a5737891a10251cbb0db08e5",
                    "labelName": "http://blazedemo.com/",
                    "label": "http://blazedemo.com/",
                    "kpis": [
                        {
                            "n": 15,
                            "na": 2,
                            "ec": 0,
                            "ts": 1442497724,
                            "t_avg": 558,
                            "lt_avg": 25.7,
                            "by_avg": 0,
                            "n_avg": 15,
                            "ec_avg": 0
                        },
                        {
                            "n": 7,
                            "na": 4,
                            "ec": 0,
                            "ts": 1442497725,
                            "t_avg": 88.1,
                            "lt_avg": 11.9,
                            "by_avg": 0,
                            "n_avg": 7,
                            "ec_avg": 0
                        }
                    ]
                }
            ]
        })
        obj = ResultsFromBZA(client)
        obj.master_id = "master"
        results = [x for x in obj.datapoints(True)]
        self.assertEquals(1, len(results))
