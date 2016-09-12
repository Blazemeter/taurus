import json
import logging

from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul, ResultsFromBZA
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ModuleMock, RecordingHandler


class TestCloudProvisioning(BZTestCase):
    def test_simple(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
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
        obj.settings["browser-open"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj)}})  # get master
        client.results.append({"result": []})  # get master sessions
        client.results.append({})  # terminate

        obj.prepare()
        self.assertEquals(1, obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, obj.executors[0].execution['locations']['us-west'])

        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_detach(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 55,
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

        obj.settings["token"] = "FakeToken"
        obj.settings["detach"] = True
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        client.results.append({"result": {"id": id(obj)}})  # start

        obj.prepare()
        self.assertEqual(1, len(client.results))
        obj.startup()
        self.assertEqual([], client.results)
        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_no_settings(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings["token"] = "FakeToken"
        obj.settings['default-location'] = "us-west-1"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # test files
        client.results.append({})  # upload files

        obj.prepare()
        self.assertEquals(1, obj.executors[0].execution['locations']['us-west-1'])

    def test_skip_reporting(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__,
                "blazemeter": ModuleMock.__module__ + "." + ModuleMock.__name__,
                "second_reporter": ModuleMock.__module__ + "." + ModuleMock.__name__,
                "third_reporter": ModuleMock.__module__ + "." + ModuleMock.__name__,
            },
            "provisioning": "mock",
            "reporting": ["blazemeter",
                          {"module": "blazemeter", "option": "value"},
                          "second_reporter",
                          {"module": "third_reporter"}]
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings["token"] = "FakeToken"
        obj.settings['default-location'] = "us-west-1"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # test files
        client.results.append({})  # upload files

        obj.prepare()
        modules = [reporter['module'] for reporter in obj.engine.config['reporting']]
        self.assertEquals(modules, ['second_reporter', 'third_reporter'])

    def __get_user_info(self):
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def test_widget(self):
        obj = CloudProvisioning()
        obj.client = BlazeMeterClientEmul(logging.getLogger(''))
        obj.client.results.append({"result": []})
        obj.client.results.append({"result": {"sessions": [
            {
                "name": "executor/scenario/location",
                "configuration": {}
            }
        ]}})

        obj.client.results.append({"result": {"sessions": [
            {
                "name": "executor/scenario/location",
                "configuration": {
                    "location": "loc-name",
                    "serversCount": "10"
                }
            }
        ]}})

        widget = obj.get_widget()
        widget.update()
        widget.update()
        widget.update()
        widget.update()

        self.assertEqual("None #None\n executor scenario:\n  Agents in loc-name: 10\n", widget.text.get_text()[0])

    def test_delete_test_files(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            "delete-test-files": True,
                            'default-location': "us-west-1",
                            })
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"},}]})  # find test
        client.results.append({"files": [{"hash": "hash1", "name": "file1"},
                                         {"hash": "hash1", "name": "file2"}]})  # test files
        client.results.append({"removed": ["hash1", "hash2"]})  # remove test files
        client.results.append({})  # upload files

        obj.prepare()
        self.assertTrue(client.delete_files_before_test)

    def test_cloud_config_cleanup(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "concurrency": {
                    "local": 1,
                    "cloud": 10,
                },
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2
                }
            },
        })
        obj.parameters = obj.engine.config['execution']
        cloud_config = obj.get_config_for_cloud()
        execution = cloud_config["execution"][0]
        self.assertNotIn("throughput", execution)
        self.assertNotIn("ramp-up", execution)
        self.assertNotIn("hold-for", execution)
        self.assertNotIn("steps", execution)

    def test_default_test_type_cloud(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"},}]})  # find test
        client.results.append({})  # upload files

        obj.prepare()
        self.assertEquals(obj.finder.test_type, obj.finder.TEST_TYPE_CLOUD)

    def test_type_forced(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "test-type": "cloud-collection"})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "items": [{"configuration": {"type": "taurus"}}]}]})  # find collection
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({})  # update collection

        obj.prepare()
        self.assertEquals(obj.finder.test_type, obj.finder.TEST_TYPE_COLLECTION)

    def test_detect_test_type_collection(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "detect-test-type": True})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "items": [{"configuration": {"type": "taurus"}}]}]})  # detect collection
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "items": [{"configuration": {"type": "taurus"}}]}]})  # find collection
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({})  # update collection

        obj.prepare()
        self.assertEquals(obj.finder.test_type, obj.finder.TEST_TYPE_COLLECTION)

    def test_detect_test_type_cloud(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "detect-test-type": True})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # detect collection
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # detect test
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({})  # update collection

        obj.prepare()
        self.assertEquals(obj.finder.test_type, obj.finder.TEST_TYPE_CLOUD)

    def test_full_collection(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
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
        obj.settings["browser-open"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # collections
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj)}})  # get master
        client.results.append({"result": []})  # get master sessions
        client.results.append({})  # terminate

        obj.prepare()
        self.assertEquals(1, obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, obj.executors[0].execution['locations']['us-west'])

        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_create_project(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "test-type": "cloud-test",
                            "project": "myproject"})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # projects
        client.results.append({"result": {"id": 1428}})  # create project
        client.results.append({"result": [{"id": 5174715,
                                           "projectId": 1428,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append({})  # upload files
        obj.prepare()

    def test_reuse_project(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "test-type": "cloud-test",
                            "project": "myproject"})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 1428, "name": "myproject"}]})  # projects
        client.results.append({"result": [{"id": 5174715,
                                           "projectId": 1428,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append({})  # upload files
        obj.prepare()

    def test_reuse_project_id(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "test-type": "cloud-test",
                            "project": 1428})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 5174715,
                                           "projectId": 1428,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append({})  # upload files
        obj.prepare()

    def test_create_collection(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        obj.settings.merge({"token": "FakeToken",
                            'default-location': "us-west-1",
                            "delete-test-files": False,
                            "test-type": "cloud-collection"})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # find collection
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection

        obj.prepare()
        self.assertEqual(obj.finder.test_type, obj.finder.TEST_TYPE_COLLECTION)

    def test_toplevel_locations(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "locations": {
                "us-east-1": 1,
                "us-west": 2
            },
            "weighted-locations": True,
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']
        obj.engine.aggregator = ConsolidatingAggregator()

        obj.settings["token"] = "FakeToken"
        obj.settings["browser-open"] = False
        obj.settings["test-type"] = "cloud-test"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files

        obj.prepare()
        conf = obj.get_config_for_cloud()

        self.assertIn('locations', conf)
        self.assertIn('weighted-locations', conf)
        self.assertEqual(conf['locations']['us-east-1'], 1)
        self.assertEqual(conf['locations']['us-west'], 2)
        self.assertNotIn('locations', conf['execution'][0])

    def test_nonexistent_location(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "locations": {
                "us-not-found": 1,
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']
        obj.engine.aggregator = ConsolidatingAggregator()

        obj.settings["token"] = "FakeToken"
        obj.settings["browser-open"] = False
        obj.settings["test-type"] = "cloud-test"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user

        self.assertRaises(ValueError, obj.prepare)

    def test_sandbox_default_location(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']
        obj.engine.aggregator = ConsolidatingAggregator()

        obj.settings["token"] = "FakeToken"
        obj.settings["browser-open"] = False
        obj.settings["test-type"] = "cloud-test"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        obj.prepare()
        self.assertEquals(1, obj.executors[0].execution['locations']['harbor-5591335d8588531f5cde3a04'])

    def test_locations_on_both_levels(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: [{
                "executor": "mock",
                "concurrency": 5500,
                "locations": {
                    "eu-west-1": 1,
                }
            }],
            "locations": {
                "ams3": 1,
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution'][0]
        obj.engine.aggregator = ConsolidatingAggregator()
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.settings["token"] = "FakeToken"
        obj.settings["browser-open"] = False
        obj.settings["test-type"] = "cloud-test"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        obj.prepare()
        cloud_config = obj.get_config_for_cloud()
        self.assertNotIn("locations", cloud_config)
        for execution in cloud_config["execution"]:
            self.assertIn("locations", execution)
        log_buff = log_recorder.warn_buff.getvalue()
        self.assertIn("Each execution has locations specified, global locations won't have any effect", log_buff)


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
        client.results.append({
            "api_version": 2,
            "error": None,
            "result": [
                {
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "samples": 152,
                    "avgResponseTime": 786,
                    "90line": 836,
                    "95line": 912,
                    "99line": 1050,
                    "minResponseTime": 531,
                    "maxResponseTime": 1148,
                    "avgLatency": 81,
                    "geoMeanResponseTime": None,
                    "stDev": 108,
                    "duration": 119,
                    "avgBytes": 0,
                    "avgThroughput": 1.2773109243697,
                    "medianResponseTime": 0,
                    "errorsCount": 0,
                    "errorsRate": 0,
                    "hasLabelPassedThresholds": None
                },
                {
                    "labelId": "e843ff89a5737891a10251cbb0db08e5",
                    "labelName": "http://blazedemo.com/",
                    "samples": 152,
                    "avgResponseTime": 786,
                    "90line": 836,
                    "95line": 912,
                    "99line": 1050,
                    "minResponseTime": 531,
                    "maxResponseTime": 1148,
                    "avgLatency": 81,
                    "geoMeanResponseTime": None,
                    "stDev": 108,
                    "duration": 119,
                    "avgBytes": 0,
                    "avgThroughput": 1.2773109243697,
                    "medianResponseTime": 0,
                    "errorsCount": 0,
                    "errorsRate": 0,
                    "hasLabelPassedThresholds": None
                }
            ]
        })
        obj = ResultsFromBZA(client)
        obj.master_id = "master"
        results = [x for x in obj.datapoints(True)]
        self.assertEquals(2, len(results))
        cumulative = results[-1][DataPoint.CUMULATIVE]['']
        self.assertTrue(0 <= cumulative[KPISet.AVG_LATENCY] < 1)
        self.assertEqual(cumulative[KPISet.CONCURRENCY], 4)
        self.assertEqual(cumulative[KPISet.PERCENTILES]['90.0'], .836)
        self.assertEqual(cumulative[KPISet.PERCENTILES]['95.0'], .912)
        self.assertEqual(cumulative[KPISet.PERCENTILES]['99.0'], 1.050)
