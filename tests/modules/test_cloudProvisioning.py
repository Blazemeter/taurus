import json
import os
import logging
import time

import yaml

from bzt import TaurusConfigError
from bzt.engine import ScenarioExecutor, ManualShutdown
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul, ResultsFromBZA
from bzt.modules.blazemeter import CloudTaurusTest, CloudCollectionTest
from tests import BZTestCase, __dir__
from bzt.utils import get_full_path
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
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
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
        obj.settings["browser-open"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
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
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
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
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # test files
        client.results.append({})  # upload files

        obj.prepare()
        modules = [reporter['module'] for reporter in obj.engine.config['reporting']]
        self.assertEquals(modules, ['second_reporter', 'third_reporter'])

    def __get_user_info(self):
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def test_widget_cloud_test(self):
        obj = CloudProvisioning()
        obj.client = BlazeMeterClientEmul(logging.getLogger(''))
        obj.test = CloudTaurusTest(obj.client, None, None, None, None, logging.getLogger(''))
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

    def test_widget_cloud_collection(self):
        obj = CloudProvisioning()
        obj.client = BlazeMeterClientEmul(logging.getLogger(''))
        obj.test = CloudCollectionTest(obj.client, None, None, None, None, logging.getLogger(''))
        obj.client.results.append({"result": {"sessions": [
            {
                "id": "session-id",
                "locationId": "loc-name",
                "readyStatus": {
                    "servers": ["server" for _ in range(10)]
                },
            }
        ]}})
        obj.client.results.append({"result": {"sessions": [
            {
                "id": "session-id",
                "name": "loc-name/scenario",
                "configuration": {}
            }
        ]}})
        obj.test.get_master_status()
        widget = obj.get_widget()
        widget.update()

        self.assertEqual("None #None\n scenario:\n  Agents in loc-name: 10\n", widget.text.get_text()[0])

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
        client.results.append({"result": []})  # collections
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"},}]})  # find test
        client.results.append(self.__get_user_info())  # user
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
        obj.test = CloudTaurusTest(obj.client, None, None, "name", None, logging.getLogger(''))
        cloud_config = obj.test.prepare_cloud_config(obj.engine.config)
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
        client.results.append({"result": []})
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"},}]})  # find test
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files

        obj.prepare()
        self.assertIsInstance(obj.test, CloudTaurusTest)

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
                            "delete-test-files": False})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "items": [{"configuration": {"type": "taurus"}}]}]})  # find collection
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({})  # update collection

        obj.prepare()
        self.assertIsInstance(obj.test, CloudCollectionTest)

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
                            "delete-test-files": False})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "items": [{"configuration": {"type": "taurus"}}]}]})  # detect collection
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({})  # update collection

        obj.prepare()
        self.assertIsInstance(obj.test, CloudCollectionTest)

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
                            "delete-test-files": False})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # detect collection
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # detect test
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": [{"id": 5174715,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({})  # update collection

        obj.prepare()
        self.assertIsInstance(obj.test, CloudTaurusTest)

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
        obj.settings["use-deprecated-api"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({"files": []})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": id(client)}})  # create collection
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj), "sessions": []}})  # get master
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
                            "project": "myproject"})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # projects
        client.results.append({"result": {"id": 1428}})  # create project
        client.results.append({"result": []})  # collections
        client.results.append({"result": [{"id": 5174715,
                                           "projectId": 1428,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append(self.__get_user_info())  # locations
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
                            "project": "myproject"})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": [{"id": 1428, "name": "myproject"}]})  # projects
        client.results.append({"result": []})  # collections
        client.results.append({"result": [{"id": 5174715,
                                           "projectId": 1428,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append(self.__get_user_info())  # user
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
                            "project": 1428})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collections
        client.results.append({"result": [{"id": 5174715,
                                           "projectId": 1428,
                                           "name": "Taurus Cloud Test",
                                           "configuration": {"type": "taurus"}}]})  # find test
        client.results.append(self.__get_user_info())  # user
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
                            "use-deprecated-api": False})
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # find collection
        client.results.append({"result": []})  # find test
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection

        obj.prepare()
        self.assertIsInstance(obj.test, CloudCollectionTest)

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
            "locations-weighted": True,
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']
        obj.engine.aggregator = ConsolidatingAggregator()

        obj.settings["token"] = "FakeToken"
        obj.settings["browser-open"] = False
        obj.settings["use-deprecated-api"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection
        obj.prepare()

        conf = yaml.load(open(os.path.join(obj.engine.artifacts_dir, "cloud.yml")))
        self.assertIn('locations', conf)
        self.assertIn('locations-weighted', conf)
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
        obj.settings["use-deprecated-api"] = False

        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        self.assertRaises(TaurusConfigError, obj.prepare)

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
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collections
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        obj.prepare()
        exec_locations = obj.executors[0].execution['locations']
        self.assertEquals(1, exec_locations['us-west-1'])

    def test_collection_defloc_sandbox(self):
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
        obj.settings["use-deprecated-api"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # find collection
        client.results.append({"result": []})  # find test
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection
        obj.prepare()
        exec_locations = obj.executors[0].execution['locations']
        expected_location = 'harbor-5591335d8588531f5cde3a04'
        self.assertIn(expected_location, exec_locations)
        self.assertEquals(1, exec_locations[expected_location])

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
        obj.settings["use-deprecated-api"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # find test
        client.results.append({"result": []})  # find collection
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection
        obj.prepare()

        cloud_config = yaml.load(open(os.path.join(obj.engine.artifacts_dir, "cloud.yml")))
        self.assertNotIn("locations", cloud_config)
        for execution in cloud_config["execution"]:
            self.assertIn("locations", execution)
        log_buff = log_recorder.warn_buff.getvalue()
        self.assertIn("Each execution has locations specified, global locations won't have any effect", log_buff)

    def test_collection_simultaneous_start(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 1,
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
        obj.settings["check-interval"] = "0ms"  # do not skip checks
        obj.settings["use-deprecated-api"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # find collection
        client.results.append({"result": []})  # find test
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj), "sessions": [{"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                                                                      {"id": "s2",
                                                                       "status": "INIT_SCRIPT"}]}})  # status
        client.results.append({"result": []})  # sessions
        client.results.append({"result": {"id": id(obj), "sessions": [{"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                                                                      {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}})
        client.results.append({"result": []})  # sessions
        client.results.append({"result": {}})  # force start
        client.results.append({"result": {"id": id(obj)}})  # master status
        client.results.append({"result": []})  # sessions
        client.results.append({})  # graceful shutdown
        client.results.append({"result": {"status": "ENDED"}})  # master status

        obj.prepare()
        obj.startup()
        obj.check()
        obj.check()  # this one should trigger force start
        obj.check()
        obj.shutdown()
        obj.post_process()
        self.assertEqual(client.results, [])

    def test_terminate_only(self):
        "test is terminated only when it was started and didn't finished"
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 1,
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
        obj.settings["check-interval"] = "0ms"  # do not skip checks
        obj.settings["use-deprecated-api"] = False
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # find collection
        client.results.append({"result": []})  # find test
        client.results.append(self.__get_user_info())  # user
        client.results.append({})  # upload files
        client.results.append({"result": {"name": "Taurus Collection", "items": []}})  # transform config to collection
        client.results.append({"result": {"id": 42}})  # create collection
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj), "sessions": [{"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                                                                      {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}})
        client.results.append({"result": []})  # sessions
        client.results.append({"result": {}})  # force start
        client.results.append({"result": {"progress": 120, "status": "ENDED"}})  # status should trigger shutdown
        client.results.append({"result": []})  # sessions

        obj.prepare()
        obj.startup()
        obj.check()  # this one should trigger force start
        self.assertTrue(obj.check())
        obj.shutdown()
        obj.post_process()
        self.assertEqual(client.results, [])

    def test_acloud_paths(self):
        obj = CloudProvisioning()
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)
        obj.engine = EngineEmul()
        obj.engine.configure([
            __dir__() + '/../../bzt/10-base.json',
            __dir__() + '/../yaml/resource_files.yml'])
        obj.parameters = obj.engine.config['execution'][0]
        obj.engine.aggregator = ConsolidatingAggregator()
        obj.settings = obj.engine.config['modules']['cloud']

        client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collection
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        obj.client = client

        # list of existing files in $HOME
        files_in_home = ['file-in-home-1.csv', 'file-in-home-2.res']

        files_in_home = [{'shortname': '~/'+_file,
                          'fullname': get_full_path('~/'+_file),
                          'created': False} for _file in files_in_home]

        for _file in files_in_home:
            if not os.path.exists(_file['fullname']):   # real file is required by Engine.find_file()
                _file['created'] = True
                with open(_file['fullname'], 'w') as fd:  # real file is required by Engine.find_file()
                    fd.write('')
        obj.engine.file_search_paths = ['tests']  # config not in cwd
        obj.engine.config[ScenarioExecutor.EXEC][0]['files'] = [     # 'files' are treated similar in all
            os.getcwd() + '/tests/test_CLI.py',  # full path         # executors so check only one
            files_in_home[1]['shortname'],       # path from ~
            'jmeter/jmeter-loader.bat',          # relative path
            'mocks.py']                          # only basename (look at file_search_paths)

        obj.prepare()

        for _file in files_in_home:
            if _file['created']:
                os.remove(_file['fullname'])

        debug = str(log_recorder.debug_buff.getvalue()).split('\n')
        str_files = [line for line in debug if 'Uploading files into the test' in line]
        self.assertEqual(1, len(str_files))
        res_files = [_file for _file in str_files[0].split('\'')[1::2]]
        with open(obj.engine.artifacts_dir + '/cloud.yml') as cl_file:
            str_cfg = cl_file.read()
        self.assertEqual(7, len(res_files))
        names = {os.path.basename(file_name): file_name for file_name in res_files}

        for new_name in names:
            old_name = names[new_name]
            self.assertIn(new_name, str_cfg)
            if new_name != old_name:
                self.assertNotIn(old_name, str_cfg)

        new_names = set(names.keys())
        self.assertEqual(new_names, {                                               # source:
            'files_paths.jmx',                                                      # execution 0 (script)
            'test_CLI.py', 'file-in-home-2.res', 'jmeter-loader.bat', 'mocks.py',   # execution 0 (files)
            'file-in-home-1.csv', 'body-file.dat'                                   # execution 0 (from jmx)
            #'BlazeDemo.java',                                                      # execution 1 (script)
        })

    def test_check_interval(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 1,
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
        obj.settings["check-interval"] = "1s"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collection
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        client.results.append({"result": {"id": id(obj)}})  # start test
        client.results.append({"result": {"id": id(obj)}})  # status
        client.results.append({"result": []})  # sessions
        client.results.append({"result": {"id": id(obj)}})  # status
        client.results.append({"result": []})  # sessions

        obj.prepare()
        obj.startup()
        obj.check()  # this one should work
        obj.check()  # this one should be skipped
        time.sleep(1)
        obj.check()  # this one should work
        obj.check()  # this one should skip

        self.assertEqual(client.results, [])

    def test_dump_locations(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.settings["dump-locations"] = True
        obj.settings["token"] = "FakeToken"
        obj.settings["use-deprecated-api"] = True
        obj.client = BlazeMeterClientEmul(obj.log)
        obj.client.results.append(self.__get_user_info())
        self.assertRaises(ManualShutdown, obj.prepare)

        warnings = log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = log_recorder.info_buff.getvalue()
        self.assertIn("Location: DFW	Dallas (Rackspace)", info)
        self.assertIn("Location: us-west-2	US West (Oregon)", info)
        self.assertNotIn("Location: harbor-5591335d8588531f5cde3a04	Sandbox", info)

        obj.post_process()

    def test_dump_locations_new_style(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.settings["dump-locations"] = True
        obj.settings["token"] = "FakeToken"
        obj.settings["use-deprecated-api"] = False
        obj.client = BlazeMeterClientEmul(obj.log)
        obj.client.results.append(self.__get_user_info())
        self.assertRaises(ManualShutdown, obj.prepare)

        warnings = log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = log_recorder.info_buff.getvalue()
        self.assertIn("Location: DFW	Dallas (Rackspace)", info)
        self.assertIn("Location: us-west-2	US West (Oregon)", info)
        self.assertIn("Location: harbor-5591335d8588531f5cde3a04	Sandbox", info)

        obj.post_process()

    def test_settings_from_blazemeter_mod(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "mock",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 1,
                }
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__,
                "blazemeter": {
                    "class": ModuleMock.__module__ + "." + ModuleMock.__name__,
                    "token": "bmtoken",
                    "detach": True,
                    "browser-open": None,
                    "check-interval": 10.0,
                }
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']
        obj.engine.aggregator = ConsolidatingAggregator()

        # these should override 'blazemeter' settings
        obj.settings["check-interval"] = 20.0
        obj.settings["browser-open"] = "both"

        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append({"result": []})  # collection
        client.results.append({"result": []})  # tests
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files

        obj.prepare()

        self.assertEqual(obj.detach, True)
        self.assertEqual(obj.browser_open, "both")
        self.assertEqual(obj.client.token, "bmtoken")
        self.assertEqual(obj.check_interval, 20.0)

        self.assertEqual(client.results, [])

    def test_defaults_clean(self):
        conf = {"execution": [{"concurrency": {"local": None}}]}
        res = CloudTaurusTest.cleanup_defaults(conf)
        self.assertEqual({"execution": [{}]}, res)


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
