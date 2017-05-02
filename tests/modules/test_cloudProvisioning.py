import json
import os
import shutil
import tempfile
import time

import yaml

from bzt import TaurusConfigError, TaurusException, NormalShutdown
from bzt.bza import Master, Test, MultiTest
from bzt.engine import ScenarioExecutor, Service
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.blazemeter import CloudProvisioning, ResultsFromBZA, ServiceStubCaptureHAR
from bzt.modules.blazemeter import CloudTaurusTest, CloudCollectionTest
from bzt.utils import get_full_path
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ModuleMock, RecordingHandler
from tests.modules.test_blazemeter import BZMock


class TestCloudProvisioning(BZTestCase):
    @staticmethod
    def __get_user_info():
        with open(__dir__() + "/../resources/json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def setUp(self):
        engine = EngineEmul()
        engine.aggregator = ConsolidatingAggregator()
        self.obj = CloudProvisioning()
        self.obj.settings.merge({'delete-test-files': False})
        self.obj.engine = engine
        self.obj.browser_open = False
        self.mock = BZMock(self.obj.user)
        self.mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests/1/files': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests/1/start': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/masters/1/stop': {"result": True},
        })

    def configure(self, engine_cfg=None, get=None, post=None, patch=None, add_config=True, add_settings=True):
        if engine_cfg is None:
            engine_cfg = {}
        self.obj.engine.config.merge(engine_cfg)

        if add_settings:
            self.obj.settings["token"] = "FakeToken"
            self.obj.settings["browser-open"] = False
            self.obj.settings['default-location'] = "us-east-1"

        if add_config:
            self.obj.engine.config.merge({
                "modules": {"mock": ModuleMock.__module__ + "." + ModuleMock.__name__},
                "provisioning": "mock"})

            self.obj.parameters = self.obj.engine.config.get('execution')

        if isinstance(self.obj.parameters, list):
            self.obj.parameters = self.obj.parameters[0]

        self.mock.mock_get.update(get if get else {})
        self.mock.mock_post.update(post if post else {})
        self.mock.mock_patch.update(patch if patch else {})
        self.mock.mock_patch.update({'https://a.blazemeter.com/api/v4/tests/1': {"result": {}}})

    def test_simple(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},

            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": []},
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {}},
            },
            post={
            }
        )  # terminate

        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, self.obj.executors[0].execution['locations']['us-west'])

        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_no_results(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},

            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": []},
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {"sessions": [{
                    "errors": [
                        {
                            "code": 70404,
                            "message": "Session ended without load report data",
                            "details": None
                        }
                    ],
                }]}},
            },
            post={
            }
        )  # terminate

        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, self.obj.executors[0].execution['locations']['us-west'])

        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.assertRaises(TaurusException, self.obj.post_process)

    def test_detach(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 55,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},
            get={
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {}},
            }
        )

        self.obj.settings["detach"] = True

        self.obj.prepare()
        self.assertEqual(11, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(12, len(self.mock.requests))
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_no_settings(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, )
        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-east-1'])

    def test_skip_reporting(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                },
                "modules": {
                    "blazemeter": ModuleMock.__module__ + "." + ModuleMock.__name__,
                    "second_reporter": ModuleMock.__module__ + "." + ModuleMock.__name__,
                    "third_reporter": ModuleMock.__module__ + "." + ModuleMock.__name__,
                },
                "reporting": ["blazemeter",
                              {"module": "blazemeter", "option": "value"},
                              "second_reporter",
                              {"module": "third_reporter"}]
            },
        )

        self.obj.prepare()
        modules = [reporter['module'] for reporter in self.obj.engine.config['reporting']]
        self.assertEquals(modules, ['second_reporter', 'third_reporter'])

    def test_widget_cloud_test(self):
        test = Test(self.obj.user, {"id": 1, 'name': 'testname'})
        self.obj.router = CloudTaurusTest(self.obj.user, test, None, None, None, self.obj.log)
        self.configure(get={
            'https://a.blazemeter.com/api/v4/masters/1/sessions': [
                {"result": {"sessions": []}},
                {"result": {"sessions": [{
                    "name": "executor/scenario/location",
                    "configuration": {
                        "location": "loc-name",
                        "serversCount": "10"}}]}}
            ]
        })

        self.obj.startup()
        widget = self.obj.get_widget()
        widget.update()
        widget.update()

        self.assertEqual("testname #1\n executor scenario:\n  Agents in loc-name: 10\n", widget.text.get_text()[0])

    def test_widget_cloud_collection(self):
        test = MultiTest(self.obj.user, {"id": 1, 'name': 'testname'})
        self.obj.router = CloudCollectionTest(self.obj.user, test, None, None, None, self.obj.log)
        self.configure(post={
            'https://a.blazemeter.com/api/v4/multi-tests/1/start?delayedStart=true': {"result": {
                "id": 1,
                "sessions": [{
                    "id": "session-id",
                    "locationId": "loc-name",
                    "readyStatus": {"servers": ["server" for _ in range(10)]}}]}}
        }, get={
            'https://a.blazemeter.com/api/v4/masters/1/status': {"result": {"status": "CREATED", "sessions": [{
                "id": "session-id",
                "locationId": "loc-name",
                "readyStatus": {"servers": ["server" for _ in range(10)]},
                "name": "loc-name/scenario",
                "configuration": {}}]}},
            'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {
                "sessions": [{
                    "id": "session-id",
                    "name": "loc-name/scenario",
                    "configuration": {}}]}}
        })
        self.obj.startup()
        self.obj.router.get_master_status()
        widget = self.obj.get_widget()
        widget.update()

        self.assertEqual("testname #1\n scenario:\n  Agents in loc-name: 10\n", widget.text.get_text()[0])

    def test_delete_test_files(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                'https://a.blazemeter.com/api/v4/web/elfinder/1?cmd=open&target=s1_Lw': {"files": [
                    {
                        "hash": "hash1",
                        "name": "file1"
                    }, {
                        "hash": "hash1",
                        "name": "file2"}]
                },
                'https://a.blazemeter.com/api/v4/web/elfinder/1?target=s1_Lw&cmd=open': {"files": [
                    {
                        "hash": "hash1",
                        "name": "file1"
                    }, {
                        "hash": "hash1",
                        "name": "file2"}]
                },
                # deleting files with GET - ...!
                'https://a.blazemeter.com/api/v4/web/elfinder/1?cmd=rm&targets[]=hash1&targets[]=hash1': {
                    "removed": ["hash1", "hash2"]
                }
            }
        )

        self.obj.settings.merge({'delete-test-files': True})
        self.obj.prepare()
        self.obj.log.info("Made requests: %s", self.mock.requests)
        self.assertEquals('https://a.blazemeter.com/api/v4/web/elfinder/1?cmd=rm&targets[]=hash1&targets[]=hash1',
                          self.mock.requests[10]['url'])

    def test_cloud_config_cleanup(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "concurrency": {
                        "local": 1,
                        "cloud": 10},
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},
        )

        self.obj.router = CloudTaurusTest(self.obj.user, None, None, "name", None, self.obj.log)
        cloud_config = self.obj.router.prepare_cloud_config(self.obj.engine.config)
        execution = cloud_config["execution"][0]
        self.assertNotIn("throughput", execution)
        self.assertNotIn("ramp-up", execution)
        self.assertNotIn("hold-for", execution)
        self.assertNotIn("steps", execution)

    def test_default_test_type_cloud(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, )
        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudTaurusTest)

    def test_type_forced(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                'https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=99999': {'result': [{'id': 1}]},
                'https://a.blazemeter.com/api/v4/multi-tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [{
                    "id": 1,
                    "projectId": 1,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]}
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
            },
            patch={
                'https://a.blazemeter.com/api/v4/multi-tests/1': {}
            }
        )
        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudCollectionTest)

    def test_detect_test_type_collection(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                'https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=99999': {'result': [{'id': 1}]},
                'https://a.blazemeter.com/api/v4/multi-tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [{
                    "id": 1,
                    "projectId": 1,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]}
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
            },
            patch={
                'https://a.blazemeter.com/api/v4/multi-tests/1': {}
            }
        )

        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudCollectionTest)

    def test_detect_test_type_cloud(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, )
        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudTaurusTest)

    def test_full_collection(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}
            },
            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': {"result": {"status": "CREATED"}},
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {"sessions": []}},
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {}},
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/multi-tests/1/start?delayedStart=true': {"result": {"id": 1}}
            }
        )

        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, self.obj.executors[0].execution['locations']['us-west'])

        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_create_project(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, )
        self.obj.settings.merge({"delete-test-files": False, "project": "myproject"})
        self.obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/projects', self.mock.requests[5]['url'])
        self.assertEquals('POST', self.mock.requests[5]['method'])

    def test_create_project_test_exists(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, get={
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [
                {"id": 1, 'projectId': 1, 'name': 'Taurus Cloud Test', 'configuration': {'type': 'taurus'}}
            ]},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=99999': [
                {'result': []},
                {'result': [{'id': 1}]}
            ],
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
        })
        self.obj.settings.merge({"delete-test-files": False, "project": "myproject"})
        self.obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/projects', self.mock.requests[5]['url'])
        self.assertEquals('POST', self.mock.requests[5]['method'])
        self.assertEquals('https://a.blazemeter.com/api/v4/tests', self.mock.requests[7]['url'])
        self.assertEquals('POST', self.mock.requests[7]['method'])

    def test_reuse_project(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                "https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=99999": {
                    "result": [{"id": 1, "name": "myproject"}]},
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {"result": [{
                    "id": 1,
                    "projectId": 1,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]}
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
            },
            patch={
                'https://a.blazemeter.com/api/v4/multi-tests/1': {}
            }
        )
        self.obj.settings.merge({"delete-test-files": False, "project": "myproject"})
        self.obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test',
                          self.mock.requests[3]['url'])

    def test_reuse_project_id(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                "https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=99999": {
                    "result": [{"id": 1, "name": "myproject"}]},
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {
                    "result": [{"id": 1, "name": "Taurus Cloud Test"}]
                }
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
            },
            patch={
                'https://a.blazemeter.com/api/v4/multi-tests/1': {}
            }
        )

        self.obj.settings.merge({"delete-test-files": False, "project": 1})
        self.obj.prepare()
        for request in self.mock.requests:
            self.assertFalse(
                request['url'] == 'https://a.blazemeter.com/api/v4/projects' and request['method'] == 'POST')

    def test_create_collection(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests/1': {},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {}}
            }
        )

        self.obj.settings.merge({"delete-test-files": False, "use-deprecated-api": False})

        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudCollectionTest)

    def test_toplevel_locations(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500},
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2},
                "locations-weighted": True},
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests/1': {},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {}}
            }
        )

        self.obj.settings["use-deprecated-api"] = False
        self.obj.prepare()

        conf = yaml.load(open(os.path.join(self.obj.engine.artifacts_dir, "cloud.yml")))
        self.assertIn('locations', conf)
        self.assertIn('locations-weighted', conf)
        self.assertEqual(conf['locations']['us-east-1'], 1)
        self.assertEqual(conf['locations']['us-west'], 2)
        self.assertNotIn('locations', conf['execution'][0])

    def test_nonexistent_location(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock", }, "locations": {"us-not-found": 1}},
        )
        self.obj.settings["use-deprecated-api"] = False
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_sandbox_default_location(self):
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
        )
        self.obj.user.token = "key"
        self.obj.prepare()
        exec_locations = self.obj.executors[0].execution['locations']
        self.assertEquals(1, exec_locations['non-harbor-sandbox'])

    def test_collection_defloc_sandbox(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock", }},
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests/1': {},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {}}
            }
        )

        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        exec_locations = self.obj.executors[0].execution['locations']
        expected_location = 'harbor-sandbox'
        self.assertIn(expected_location, exec_locations)
        self.assertEquals(1, exec_locations[expected_location])

    def test_locations_on_both_levels(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: [{
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {"us-east-1": 1}}],
                "locations": {"aws": 1}},
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests/1': {},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {}}
            }
        )

        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)

        self.obj.settings["use-deprecated-api"] = False
        self.obj.prepare()

        cloud_config = yaml.load(open(os.path.join(self.obj.engine.artifacts_dir, "cloud.yml")))
        self.assertNotIn("locations", cloud_config)
        for execution in cloud_config["execution"]:
            self.assertIn("locations", execution)
        log_buff = log_recorder.warn_buff.getvalue()
        self.assertIn("Each execution has locations specified, global locations won't have any effect", log_buff)

    def test_collection_simultaneous_start(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 1}}},
            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': [
                    {"result": {
                        "id": id(self.obj),
                        "sessions": [
                            {"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                            {"id": "s2", "status": "INIT_SCRIPT"}]}},
                    {"result": {
                        "id": id(self.obj),
                        "sessions": [
                            {"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                            {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}},
                    {"result": {
                        "id": id(self.obj),
                        "sessions": [
                            {"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                            {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}},
                    {"result": {
                        "id": id(self.obj),
                        "status": "ENDED",
                        "sessions": [
                            {"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                            {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}},
                ],
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {"sessions": []}},
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {"sessions": []}},
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests/1': {},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/multi-tests/1/start?delayedStart=true': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/masters/1/force-start': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/multi-tests/1/stop': {"result": {"id": 1}}
            }
        )

        self.obj.settings["check-interval"] = "0ms"  # do not skip checks
        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.check()  # this one should trigger force start
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIn('masters/1/force-start', ''.join([x['url'] for x in self.mock.requests]))

    def test_terminate_only(self):
        """  test is terminated only when it was started and didn't finished """
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 1}}},

            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': [
                    {"result": {
                        "id": id(self.obj),
                        "sessions": [
                            {"id": "s1", "status": "JMETER_CONSOLE_INIT", "locationId": "some"},
                            {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}},
                    {"result": {"progress": 120, "status": "ENDED"}},  # status should trigger shutdown
                ],
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {"sessions": [{'id': "s1"}]}},
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {
                    "note": "msg",
                    "sessions": [{'id': "s1"}]}
                },
                'https://a.blazemeter.com/api/v4/masters/1': {"result": {"id": 1, "note": "msg"}},
                'https://a.blazemeter.com/api/v4/sessions/s1/reports/logs': {"result": {"data": [
                    {
                        'filename': "artifacts.zip",
                        'dataUrl': "file://" + __dir__() + '/../resources/artifacts-1.zip'
                    }
                ]}}
            },
            post={
                'https://a.blazemeter.com/api/v4/web/elfinder/taurus_%s' % id(self.obj.user.token): {},
                'https://a.blazemeter.com/api/v4/multi-tests/taurus-import': {"result": {
                    "name": "Taurus Collection", "items": []
                }},
                'https://a.blazemeter.com/api/v4/multi-tests/1': {},
                'https://a.blazemeter.com/api/v4/multi-tests': {"result": {'id': 1, 'name': 'testname'}},
                'https://a.blazemeter.com/api/v4/multi-tests/1/start?delayedStart=true': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/masters/1/force-start': {"result": {"id": 1}},
                'https://a.blazemeter.com/api/v4/multi-tests/1/stop': {"result": {"id": 1}}
            }
        )

        self.obj.settings["check-interval"] = "0ms"  # do not skip checks
        self.obj.settings["use-deprecated-api"] = False
        cls = ServiceStubCaptureHAR.__module__ + "." + ServiceStubCaptureHAR.__name__
        self.obj.engine.config.get("modules").get('capturehar')['class'] = cls
        self.obj.engine.config.get(Service.SERV, []).append('capturehar')

        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()  # this one should trigger force start
        self.assertTrue(self.obj.check())
        self.obj.shutdown()
        self.obj.post_process()
        self.assertEqual(19, len(self.mock.requests))
        self.assertIn("Cloud test has probably failed with message: msg", log_recorder.warn_buff.getvalue())

    def test_cloud_paths(self):
        """
        Test different executor/path combinations for correct return values of get_resources_files
        """
        self.configure(
            add_config=False, add_settings=False,
        )  # upload files

        # FIXME: refactor this method!
        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)
        self.obj.engine.configure([
            __dir__() + '/../../bzt/resources/base-config.yml',
            __dir__() + '/../resources/yaml/resource_files.yml'], read_config_files=False)
        self.obj.settings = self.obj.engine.config['modules']['cloud']
        self.obj.settings.merge({'delete-test-files': False})

        # list of existing files in $HOME
        pref = 'file-in-home-'
        files_in_home = ['00.jmx', '01.csv', '02.res', '03.java', '04.scala', '05.jar', '06.py',
                         '07.properties', '08.py', '09.siege', '10.xml', '11.ds', '12.xml', '13.src']
        files_in_home = [pref + _file for _file in files_in_home]

        back_home = os.environ.get('HOME', '')
        temp_home = tempfile.mkdtemp()
        try:
            os.environ['HOME'] = temp_home
            files_in_home = [{'shortname': os.path.join('~', _file),
                              'fullname': get_full_path(os.path.join('~', _file))}
                             for _file in files_in_home]

            shutil.copyfile(__dir__() + '/../resources/jmeter/jmx/dummy.jmx', files_in_home[0]['fullname'])

            dir_path = get_full_path(os.path.join('~', 'example-of-directory'))
            os.mkdir(dir_path)

            for _file in files_in_home[1:]:
                open(_file['fullname'], 'a').close()

            self.obj.engine.file_search_paths = ['tests']  # config not in cwd

            # 'files' are treated similar in all executors so check only one
            self.obj.engine.config[ScenarioExecutor.EXEC][0]['files'] = [
                os.path.join(os.getcwd(), 'tests', 'test_CLI.py'),  # full path
                files_in_home[2]['shortname'],  # path from ~
                os.path.join('resources', 'jmeter', 'jmeter-loader.bat'),  # relative path
                'mocks.py',  # only basename (look at file_search_paths)
                '~/example-of-directory']  # dir

            self.obj.prepare()

            debug = log_recorder.debug_buff.getvalue().split('\n')
            str_files = [line for line in debug if 'Replace file names in config' in line]
            self.assertEqual(1, len(str_files))
            res_files = [_file for _file in str_files[0].split('\'')[1::2]]
            half = int(len(res_files) / 2)
            old_names = res_files[:half]
            new_names = res_files[half:]
            names = list(zip(old_names, new_names))

            with open(self.obj.engine.artifacts_dir + '/cloud.yml') as cl_file:
                str_cfg = cl_file.read()

            archive_found = False
            for old_name, new_name in names:
                if new_name.endswith('.zip'):
                    archive_found = True

                # all resources on the disk, dir has been packed
                path_to_file = get_full_path(self.obj.engine.find_file(old_name))
                msg = 'File %s (%s) not found on disk' % (old_name, path_to_file)
                self.assertTrue(os.path.exists(path_to_file), msg)
                msg = 'Short name %s not found in modified config' % new_name
                self.assertIn(new_name, str_cfg, msg)  # all short names in config
                if new_name != old_name:
                    msg = 'Long name %s found in config' % old_name
                    self.assertNotIn(old_name, str_cfg, msg)  # no one long name in config

            self.assertTrue(archive_found)

            self.assertEqual(set(new_names), {  # source:
                'dummy.jmx',  # execution 0 (script)
                'test_CLI.py', 'file-in-home-02.res',  # 0 (files)
                'jmeter-loader.bat', 'mocks.py',  # 0 (files)
                'example-of-directory.zip',  # 0 (files)
                'files_paths.jmx',  # 1 (script)
                'file-in-home-01.csv', 'body-file.dat',  # 1 (from jmx)
                'BlazeDemo.java',  # 2 (script)
                'file-in-home-05.jar', 'dummy.jar',  # 2 (additional-classpath)
                'testng.xml',  # 2 (testng-xml)
                'file-in-home-03.java',  # 3 (script)
                'file-in-home-12.xml',  # 3 (testng-xml)
                'BasicSimulation.scala',  # 4 (script)
                'file-in-home-04.scala',  # 5 (script)
                'helloworld.py',  # 6 (script)
                'grinder.base.properties',  # 6 (properties-file)
                'file-in-home-06.py',  # 7 (script)
                'file-in-home-07.properties',  # 7 (properties-file)
                'simple.py',  # 8 (script)
                'file-in-home-08.py',  # 9 (script)
                'jmeter-loader.bat',  # 10 (data-sources)
                'file-in-home-11.ds',  # 10 (data-sources)
                'url-file',  # 11 (script)
                'file-in-home-09.siege',  # 12 (script)
                'http_simple.xml',  # 13 (script)
                'file-in-home-10.xml',  # 14 (script)
                'pbench.src',  # 15 (script)
                'file-in-home-13.src',  # 16 (script)
                'file-in-home-00.jmx'  # 17 (script)
            })
        finally:
            os.environ['HOME'] = back_home
            shutil.rmtree(temp_home)

    def test_check_interval(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {"executor": "mock", }},
            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': [
                    {"result": {"id": id(self.obj)}},
                    {"result": {"id": id(self.obj), 'progress': 100}},
                ],
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": []},
                'https://a.blazemeter.com/api/v4/data/labels?master_id=1': {"result": [
                    {"id": "ALL", "name": "ALL"},
                    {"id": "e843ff89a5737891a10251cbb0db08e5", "name": "http://blazedemo.com/"}
                ]},
                'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=0&master_ids%5B%5D=1&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
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
                                }, {
                                    "n": 7,
                                    "na": 4,
                                    "ec": 0,
                                    "ts": 1442497725,
                                    "t_avg": 88.1,
                                    "lt_avg": 11.9,
                                    "by_avg": 0,
                                    "n_avg": 7,
                                    "ec_avg": 0
                                }]
                        }, {
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
                                }, {
                                    "n": 7,
                                    "na": 4,
                                    "ec": 0,
                                    "ts": 1442497725,
                                    "t_avg": 88.1,
                                    "lt_avg": 11.9,
                                    "by_avg": 0,
                                    "n_avg": 7,
                                    "ec_avg": 0
                                }]}]},
                'https://a.blazemeter.com/api/v4/masters/1/reports/aggregatereport/data': {
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
                        }, {
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
                        }]}
            }
        )

        self.obj.settings["check-interval"] = "1s"

        self.obj.prepare()
        self.obj.startup()
        self.obj.check()  # this one should work
        self.obj.engine.aggregator.check()
        self.obj.check()  # this one should be skipped
        self.obj.engine.aggregator.check()
        time.sleep(1.5)
        self.obj.check()  # this one should work
        self.obj.engine.aggregator.check()
        self.obj.check()  # this one should skip
        self.obj.results_reader.min_ts = 0  # to make it request same URL
        self.obj.engine.aggregator.check()

        self.assertEqual(22, len(self.mock.requests))

    def test_dump_locations(self):
        self.configure()
        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)

        self.obj.settings["dump-locations"] = True
        self.obj.settings["use-deprecated-api"] = True
        try:
            self.assertRaises(NormalShutdown, self.obj.prepare)
        except KeyboardInterrupt as exc:
            raise AssertionError(type(exc))

        warnings = log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = log_recorder.info_buff.getvalue()
        self.assertIn("Location: us-west\tDallas (Rackspace)", info)
        self.assertIn("Location: us-east-1\tEast", info)
        self.assertNotIn("Location: harbor-sandbox\tSandbox", info)
        self.obj.post_process()

    def test_dump_locations_new_style(self):
        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)
        self.configure()
        self.obj.settings["dump-locations"] = True
        self.obj.settings["use-deprecated-api"] = False
        try:
            self.assertRaises(NormalShutdown, self.obj.prepare)
        except KeyboardInterrupt as exc:
            raise AssertionError(type(exc))

        warnings = log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = log_recorder.info_buff.getvalue()
        self.assertIn("Location: us-west\tDallas (Rackspace)", info)
        self.assertIn("Location: us-east-1\tEast", info)
        self.assertIn("Location: harbor-sandbox\tSandbox", info)

        self.obj.post_process()

    def test_settings_from_blazemeter_mod(self):
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 1}},
                "modules": {
                    "blazemeter": {
                        "class": ModuleMock.__module__ + "." + ModuleMock.__name__,
                        "token": "bmtoken",
                        "detach": True,
                        "browser-open": None,
                        "check-interval": 10.0}}},
        )  # upload files

        # these should override 'blazemeter' settings
        self.obj.settings["check-interval"] = 20.0
        self.obj.settings["browser-open"] = "both"

        self.obj.prepare()

        self.assertEqual(self.obj.detach, True)
        self.assertEqual(self.obj.browser_open, "both")
        self.assertEqual(self.obj.user.token, "bmtoken")
        self.assertEqual(self.obj.check_interval, 20.0)
        self.assertEqual(11, len(self.mock.requests))

    def test_public_report(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 1,
                    "locations": {
                        "us-west": 2
                    }}},
            post={
                'https://a.blazemeter.com/api/v4/masters/1/public-token': {"result": {"publicToken": "publicToken"}}
            },
            get={
                'https://a.blazemeter.com/api/v4/masters/1/status': {"result": {"status": "CREATED"}},
                'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": {"sessions": []}},
                'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {}},
            }
        )

        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)

        self.obj.settings['public-report'] = True
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

        log_buff = log_recorder.info_buff.getvalue()
        log_line = "Public report link: https://a.blazemeter.com/app/?public-token=publicToken#/masters/1/summary"
        self.assertIn(log_line, log_buff)


class TestCloudTaurusTest(BZTestCase):
    def test_defaults_clean(self):
        conf = {"execution": [{"concurrency": {"local": None}}]}
        res = CloudTaurusTest.cleanup_defaults(conf)
        self.assertEqual({"execution": [{}]}, res)


class TestResultsFromBZA(BZTestCase):
    def test_simple(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/data/labels?master_id=1': {"result": [
                {"id": "ALL", "name": "ALL"},
                {"id": "e843ff89a5737891a10251cbb0db08e5", "name": "http://blazedemo.com/"}
            ]},
            'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=0&master_ids%5B%5D=1&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
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
                            }, {
                                "n": 7,
                                "na": 4,
                                "ec": 0,
                                "ts": 1442497725,
                                "t_avg": 88.1,
                                "lt_avg": 11.9,
                                "by_avg": 0,
                                "n_avg": 7,
                                "ec_avg": 0
                            }]
                    }, {
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
                            }, {
                                "n": 7,
                                "na": 4,
                                "ec": 0,
                                "ts": 1442497725,
                                "t_avg": 88.1,
                                "lt_avg": 11.9,
                                "by_avg": 0,
                                "n_avg": 7,
                                "ec_avg": 0
                            }]}]},
            'https://a.blazemeter.com/api/v4/masters/1/reports/aggregatereport/data': {
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
                    }, {
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
                    }]}
        })

        obj = ResultsFromBZA()
        obj.master = Master(data={'id': 1})
        mock.apply(obj.master)
        results = [x for x in obj.datapoints(True)]
        self.assertEquals(2, len(results))
        cumulative = results[-1][DataPoint.CUMULATIVE]['']
        self.assertTrue(0 <= cumulative[KPISet.AVG_LATENCY] < 1)
        self.assertEqual(cumulative[KPISet.CONCURRENCY], 4)
        self.assertEqual(cumulative[KPISet.PERCENTILES]['90.0'], .836)
        self.assertEqual(cumulative[KPISet.PERCENTILES]['95.0'], .912)
        self.assertEqual(cumulative[KPISet.PERCENTILES]['99.0'], 1.050)
