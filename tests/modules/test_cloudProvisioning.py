import json
import logging
import os
import shutil
import tempfile
import time

import yaml

from bzt import TaurusConfigError, TaurusException, NormalShutdown, AutomatedShutdown
from bzt.bza import Master, Test, MultiTest
from bzt.engine import ScenarioExecutor, Service
from bzt.modules import FunctionalAggregator
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.blazemeter import CloudProvisioning, ResultsFromBZA, ServiceStubCaptureHAR, FunctionalBZAReader, \
    FUNC_TEST_TYPE
from bzt.modules.blazemeter import CloudTaurusTest, CloudCollectionTest
from bzt.utils import get_full_path
from tests import BZTestCase, RESOURCES_DIR, BASE_CONFIG
from tests.mocks import EngineEmul, ModuleMock
from tests.modules.test_blazemeter import BZMock


class TestCloudProvisioning(BZTestCase):
    @staticmethod
    def __get_user_info():
        with open(RESOURCES_DIR + "json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def setUp(self):
        super(TestCloudProvisioning, self).setUp()
        engine = EngineEmul()
        engine.aggregator = ConsolidatingAggregator()
        self.obj = CloudProvisioning()
        self.obj.settings.merge({'delete-test-files': False})
        self.obj.engine = engine
        self.obj.browser_open = False
        self.mock = BZMock(self.obj.user)
        self.mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {"result": []},
        })
        self.mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1, 'workspaceId': 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {"id": 1, "configuration": {"type": "taurus"}}},
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
        self.assertEqual(15, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(16, len(self.mock.requests))
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
        self.obj.router = CloudTaurusTest(self.obj.user, test, None, None, None, False, self.obj.log)
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
        self.obj.router = CloudCollectionTest(self.obj.user, test, None, None, None, False, self.obj.log)
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
                          self.mock.requests[13]['url'])

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

        self.obj.router = CloudTaurusTest(self.obj.user, None, None, "name", None, False, self.obj.log)
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
                'https://a.blazemeter.com/api/v4/projects?projectId=1': {'result': [{'id': 1,
                                                                                     'workspaceId': 1}]},
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
        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudCollectionTest)

    def test_detect_test_type_collection(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                'https://a.blazemeter.com/api/v4/projects?workspaceId=1': {'result': [{'id': 1,
                                                                                       'workspaceId': 1}]},
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
        self.assertEquals('https://a.blazemeter.com/api/v4/projects', self.mock.requests[6]['url'])
        self.assertEquals('POST', self.mock.requests[6]['method'])

    def test_create_project_test_exists(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, get={
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [
                {"id": 1, 'projectId': 1, 'name': 'Taurus Cloud Test', 'configuration': {'type': 'taurus'}}
            ]},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': [
                {'result': []},
                {'result': [{'id': 1}]}
            ],
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
        })
        self.obj.settings.merge({"delete-test-files": False, "project": "myproject"})
        self.obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/projects', self.mock.requests[6]['url'])
        self.assertEquals('POST', self.mock.requests[6]['method'])
        self.assertEquals('https://a.blazemeter.com/api/v4/tests', self.mock.requests[10]['url'])
        self.assertEquals('POST', self.mock.requests[10]['method'])

    def test_reuse_project(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                "https://a.blazemeter.com/api/v4/projects?workspaceId=1": {
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
                          self.mock.requests[7]['url'])

    def test_reuse_project_id(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                "https://a.blazemeter.com/api/v4/projects?workspaceId=1": {
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

        self.obj.settings.merge({"delete-test-files": False, "use-deprecated-api": False, 'dedicated-ips': True})

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

        self.sniff_log(self.obj.log)

        self.obj.settings["use-deprecated-api"] = False
        self.obj.prepare()

        cloud_config = yaml.load(open(os.path.join(self.obj.engine.artifacts_dir, "cloud.yml")))
        self.assertNotIn("locations", cloud_config)
        for execution in cloud_config["execution"]:
            self.assertIn("locations", execution)
        log_buff = self.log_recorder.warn_buff.getvalue()
        self.assertIn("Each execution has locations specified, global locations won't have any effect", log_buff)

    def test_locations_global(self):
        self.obj.user.token = object()
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: [{
                    "executor": "mock",
                    "concurrency": 5500,
                }],
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

        self.sniff_log(self.obj.log)

        self.obj.settings["use-deprecated-api"] = True
        self.obj.settings["cloud-mode"] = "taurusCloud"
        self.obj.prepare()

        self.assertIsInstance(self.obj.router, CloudTaurusTest)

        cloud_config = yaml.load(open(os.path.join(self.obj.engine.artifacts_dir, "cloud.yml")))
        self.assertIn("locations", cloud_config)
        for execution in cloud_config["execution"]:
            self.assertNotIn("locations", execution)

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
                        'dataUrl': "file://" + RESOURCES_DIR + 'artifacts-1.zip'
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
        self.obj.engine.config.get("modules", force_set=True).get("capturehar", force_set=True)["class"] = cls
        self.obj.engine.config.get(Service.SERV, [], force_set=True).append("capturehar")

        self.sniff_log(self.obj.log)
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()  # this one should trigger force start
        self.assertTrue(self.obj.check())
        self.obj.shutdown()
        self.obj.post_process()
        self.assertEqual(22, len(self.mock.requests))
        self.assertIn("Cloud test has probably failed with message: msg", self.log_recorder.warn_buff.getvalue())

    def test_cloud_paths(self):
        """
        Test different executor/path combinations for correct return values of get_resources_files
        """
        self.configure(
            add_config=False, add_settings=False,
        )  # upload files

        # FIXME: refactor this method!
        self.sniff_log(self.obj.log)
        self.obj.engine.configure([BASE_CONFIG, RESOURCES_DIR + 'yaml/resource_files.yml'], read_config_files=False)
        self.obj.settings = self.obj.engine.config['modules']['cloud']
        self.obj.settings.merge({'delete-test-files': False})

        # list of existing files in $HOME
        pref = 'file-in-home-'
        files_in_home = ['00.jmx', '01.csv', '02.res', '03.java', '04.scala', '05.jar', '06.py',
                         '07.properties', '08.py', '09.siege', '10.xml', '11.ds', '12.xml',
                         '13.src', '14.java', '15.xml', '16.java', '17.js', '18.rb', '19.jar', '20.jar']
        files_in_home = [pref + _file for _file in files_in_home]

        back_home = os.environ.get('HOME', '')
        temp_home = tempfile.mkdtemp()
        try:
            os.environ['HOME'] = temp_home
            files_in_home = [{'shortname': os.path.join('~', _file),
                              'fullname': get_full_path(os.path.join('~', _file))}
                             for _file in files_in_home]

            shutil.copyfile(RESOURCES_DIR + 'jmeter/jmx/dummy.jmx', files_in_home[0]['fullname'])

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

            debug = self.log_recorder.debug_buff.getvalue().split('\n')
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

            target_names = {  # source:
                'dummy.jmx',  # execution 0 (script)
                'test_CLI.py', 'file-in-home-02.res',  # 0 (files)
                'jmeter-loader.bat', 'mocks.py',  # 0 (files)
                'example-of-directory.zip',  # 0 (files)
                'files_paths.jmx',  # 1 (script)
                'file-in-home-01.csv', 'body-file.dat',  # 1 (from jmx)
                'BlazeDemo.java',  # 2 (script)
                'file-in-home-05.jar', 'dummy.jar',  # 2 (additional-classpath)
                'not-jmx.xml',  # 2 (testng-xml)
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
                'file-in-home-00.jmx',  # 17 (script)
                'TestBlazemeterFail.java',  # 18 (script)
                'file-in-home-20.jar',  # 18 (additional-classpath)
                'file-in-home-14.java',  # 19 (script)
                'TestNGSuite.java',  # 20 (script)
                'testng.xml',  # 20 (detected testng-xml from 'files')
                'file-in-home-15.xml',  # 21 (testng-xml)
                'file-in-home-16.java',  # 21 (script)
                'bd_scenarios.js',  # 22 (script)
                'file-in-home-17.js',  # 23 (sript)
                'example_spec.rb',  # 24 (script)
                'file-in-home-18.rb',  # 25 (sript)
                'file-in-home-19.jar',  # global testng settings (additional-classpath)
                'variable_file_upload.jmx',
            }
            self.assertEqual(set(new_names), target_names)
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
                        }]},
                'https://a.blazemeter.com/api/v4/masters/1/reports/errorsreport/data?noDataError=false': {
                    'result': []}})

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

        self.assertEqual(28, len(self.mock.requests))

    def test_dump_locations(self):
        self.configure()
        self.sniff_log(self.obj.log)

        self.obj.settings["dump-locations"] = True
        self.obj.settings["use-deprecated-api"] = True
        try:
            self.assertRaises(NormalShutdown, self.obj.prepare)
        except KeyboardInterrupt as exc:
            raise AssertionError(type(exc))

        warnings = self.log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = self.log_recorder.info_buff.getvalue()
        self.assertIn("Location: us-west\tDallas (Rackspace)", info)
        self.assertIn("Location: us-east-1\tEast", info)
        self.assertIn("Location: harbor-sandbox\tSandbox", info)
        self.obj.post_process()

    def test_dump_locations_new_style(self):
        self.sniff_log(self.obj.log)
        self.configure()
        self.obj.settings["dump-locations"] = True
        self.obj.settings["use-deprecated-api"] = False
        try:
            self.assertRaises(NormalShutdown, self.obj.prepare)
        except KeyboardInterrupt as exc:
            raise AssertionError(type(exc))

        warnings = self.log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = self.log_recorder.info_buff.getvalue()
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
        self.assertEqual(15, len(self.mock.requests))

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

        self.sniff_log(self.obj.log)

        self.obj.settings['public-report'] = True
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

        log_buff = self.log_recorder.info_buff.getvalue()
        log_line = "Public report link: https://a.blazemeter.com/app/?public-token=publicToken#/masters/1/summary"
        self.assertIn(log_line, log_buff)

    def test_functional_test_creation(self):
        self.obj.engine.aggregator = FunctionalAggregator()

        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, get={
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [
                {"id": 1, 'projectId': 1, 'name': 'Taurus Cloud Test', 'configuration': {'type': 'taurus'}}
            ]},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': [
                {'result': []},
                {'result': [{'id': 1}]}
            ],
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
        }, post={
            'https://a.blazemeter.com/api/v4/tests/1/start?functionalExecution=true': {'result': {'id': 'mid'}}
        })
        self.obj.settings.merge({"delete-test-files": False, "project": "myproject"})
        self.obj.prepare()
        self.obj.startup()
        reqs = self.mock.requests
        self.assertEqual(reqs[10]['url'], 'https://a.blazemeter.com/api/v4/tests')
        self.assertEqual(reqs[10]['method'], 'POST')
        data = json.loads(reqs[10]['data'])
        self.assertEqual(data['configuration']['type'], FUNC_TEST_TYPE)

    def test_functional_cloud_failed_shutdown(self):
        self.obj.engine.aggregator = FunctionalAggregator()
        func_summary = {"isFailed": True}
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, get={
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [
                {"id": 1, 'projectId': 1, 'name': 'Taurus Cloud Test', 'configuration': {'type': 'taurus'}}
            ]},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': [
                {'result': []},
                {'result': [{'id': 1}]}
            ],
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/masters/1/status': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/masters/1/sessions': {"result": []},
            'https://a.blazemeter.com/api/v4/masters/1/full': {"result": {"functionalSummary": func_summary}},
        }, post={
            'https://a.blazemeter.com/api/v4/tests/1/start?functionalExecution=true': {'result': {'id': 1}}
        })
        self.obj.settings.merge({"delete-test-files": False, "project": "myproject"})
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        with self.assertRaises(AutomatedShutdown):
            self.obj.post_process()

    def test_launch_existing_test(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=foo': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=1&name=foo': {"result": [
                    {"id": 1, "name": "foo", "configuration": {"type": "taurus"}}
                ]},
            }
        )

        self.obj.settings["test"] = "foo"
        self.obj.settings["launch-existing-test"] = True

        self.obj.prepare()
        self.assertEqual(10, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(11, len(self.mock.requests))

    def test_launch_existing_test_by_id(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&id=1': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=1&id=1': {"result": [
                    {"id": 1, "name": "foo", "configuration": {"type": "taurus"}}
                ]},
            }
        )

        self.obj.settings["test"] = 1
        self.obj.settings["launch-existing-test"] = True

        self.obj.prepare()
        self.assertEqual(10, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(11, len(self.mock.requests))

    def test_launch_existing_test_not_found_by_id(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&id=1': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=1&id=1': {"result": []},
            }
        )

        self.obj.settings["test"] = 1
        self.obj.settings["launch-existing-test"] = True

        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_launch_existing_test_not_found(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=foo': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=1&name=foo': {"result": []},
            }
        )

        self.obj.settings["test"] = "foo"
        self.obj.settings["launch-existing-test"] = True

        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_launch_existing_multi_test(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&id=1': {"result": [
                    {"id": 1, "name": 1}
                ]}
            },
            post={
                'https://a.blazemeter.com/api/v4/multi-tests/1/start?delayedStart=true': {"result": {"id": 1}}
            }
        )

        self.obj.settings["test"] = 1
        self.obj.settings["launch-existing-test"] = True

        self.obj.prepare()
        self.assertEqual(9, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(10, len(self.mock.requests))

    def test_launch_existing_test_non_taurus(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=foo': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=1&name=foo': {"result": [
                    {"id": 1, "name": "foo", "configuration": {"type": "jmeter"}}
                ]},
            }
        )

        self.obj.settings["test"] = "foo"

        self.obj.prepare()
        self.assertEqual(10, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(11, len(self.mock.requests))

    def test_launch_test_by_link(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/accounts': {"result": [{"id": 1, "name": "Acc name"}]},
                'https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100': {
                    "result": [{"id": 2, "name": "Wksp name", "enabled": True, "accountId": 1}]
                },
                'https://a.blazemeter.com/api/v4/projects?workspaceId=2': {
                    "result": [{"id": 3, "name": "Project name", "workspaceId": 2}]
                },
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=3&id=4': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=3&id=4': {"result": [
                    {"id": 4, "name": "foo", "configuration": {"type": "taurus"}}
                ]},
            },
            post={
                'https://a.blazemeter.com/api/v4/tests/4/start': {"result": {"id": 5}},
            }
        )

        self.obj.settings["test"] = "https://a.blazemeter.com/app/#/accounts/1/workspaces/2/projects/3/tests/4"
        self.obj.settings["launch-existing-test"] = True

        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudTaurusTest)
        self.assertEqual(7, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(8, len(self.mock.requests))

    def test_update_test_by_link(self):
        self.configure(
            engine_cfg={
                "execution": [
                    {"executor": "mock",
                     "iterations": 1,
                     "locations": {"eu-west-1": 1},
                     "scenario": {"requests": ["http://blazedemo.com/"]}}
                ]
            },
            get={
                'https://a.blazemeter.com/api/v4/accounts': {"result": [{"id": 1, "name": "Acc name"}]},
                'https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100': {
                    "result": [{"id": 2, "name": "Wksp name", "enabled": True, "accountId": 1,
                                "locations": [{"id": "eu-west-1"}]}]
                },
                'https://a.blazemeter.com/api/v4/workspaces/2': {
                    "result": {"id": 2, "name": "Wksp name", "enabled": True, "accountId": 1,
                               "locations": [{"id": "eu-west-1"}]}
                },
                'https://a.blazemeter.com/api/v4/projects?workspaceId=2': {
                    "result": [{"id": 3, "name": "Project name", "workspaceId": 2}]
                },
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=3&id=4': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=3&id=4': {"result": [
                    {"id": 4, "name": "foo", "configuration": {"type": "taurus"}}
                ]},
            },
            post={
                'https://a.blazemeter.com/api/v4/tests/4/files': {"result": {}},
                'https://a.blazemeter.com/api/v4/tests/4/start': {"result": {"id": 5}},
            },
            patch={
                'https://a.blazemeter.com/api/v4/tests/4': {"result": {}},
            }
        )

        self.obj.settings["test"] = "https://a.blazemeter.com/app/#/accounts/1/workspaces/2/projects/3/tests/4"
        self.obj.prepare()
        self.assertEqual(11, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(12, len(self.mock.requests))
        self.assertEqual(self.obj.router.master['id'], 5)

    def test_lookup_account_workspace(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/accounts': {"result": [{"id": 1, "name": "Acc name"}]},
                'https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100': {
                    "result": [{"id": 2, "name": "Wksp name", "enabled": True, "accountId": 1}]
                },
                'https://a.blazemeter.com/api/v4/projects?workspaceId=2&name=Project+name': {
                    "result": [{"id": 3, "name": "Project name", "workspaceId": 2}]
                },
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=3&name=Test+name': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=3&name=Test+name': {"result": [
                    {"id": 4, "name": "Test name", "configuration": {"type": "taurus"}}
                ]}},
            post={
                'https://a.blazemeter.com/api/v4/tests/4/start': {"result": {"id": 5}},
            })

        self.obj.settings["account"] = "Acc name"
        self.obj.settings["workspace"] = "Wksp name"
        self.obj.settings["project"] = "Project name"
        self.obj.settings["test"] = "Test name"
        self.obj.settings["launch-existing-test"] = True

        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudTaurusTest)
        self.assertEqual(7, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(8, len(self.mock.requests))

    def test_lookup_test_ids(self):
        self.configure(
            get={
                'https://a.blazemeter.com/api/v4/accounts': {"result": [{"id": 1, "name": "Acc name"}]},
                'https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100': {
                    "result": [{"id": 2, "name": "Wksp name", "enabled": True, "accountId": 1}]
                },
                'https://a.blazemeter.com/api/v4/projects?workspaceId=2': {
                    "result": [{"id": 3, "name": "Project name", "workspaceId": 2}]
                },
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=3&id=4': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=3&id=4': {"result": [
                    {"id": 4, "name": "Test name", "configuration": {"type": "taurus"}}
                ]},
            },
            post={
                'https://a.blazemeter.com/api/v4/tests/4/start': {"result": {"id": 5}},
            }
        )

        self.obj.settings["account"] = 1
        self.obj.settings["workspace"] = 2
        self.obj.settings["project"] = 3
        self.obj.settings["test"] = 4
        self.obj.settings["launch-existing-test"] = True

        self.obj.prepare()
        self.assertIsInstance(self.obj.router, CloudTaurusTest)
        self.assertEqual(7, len(self.mock.requests))
        self.obj.startup()
        self.assertEqual(8, len(self.mock.requests))

    def test_lookup_test_different_type(self):
        self.configure(
            engine_cfg={
                "execution": [
                    {"executor": "mock",
                     "iterations": 1,
                     "locations": {"eu-west-1": 1},
                     "scenario": {"requests": ["http://blazedemo.com/"]}}
                ]
            },
            get={
                'https://a.blazemeter.com/api/v4/accounts': {
                    "result": [{"id": 1, "name": "Acc name", "owner": {"id": 1}}]},
                'https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100': {
                    "result": [{"id": 2, "name": "Wksp name", "enabled": True, "accountId": 1,
                                "locations": [{"id": "eu-west-1"}]},
                               {"id": 1, "name": "Dflt name", "enabled": True, "accountId": 1,
                                "locations": [{"id": "eu-west-1"}]}]
                },
                'https://a.blazemeter.com/api/v4/workspaces/1': {
                    "result": {"id": 1, "name": "Wksp name", "enabled": True, "accountId": 1,
                               "locations": [{"id": "eu-west-1"}]}
                },
                'https://a.blazemeter.com/api/v4/projects?workspaceId=2': {
                    "result": [{"id": 3, "name": "Project name", "workspaceId": 2}]
                },
                'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=ExternalTest': {"result": []},
                'https://a.blazemeter.com/api/v4/tests?projectId=1&name=ExternalTest': {"result": [
                    {"id": 4, "name": "ExternalTest", "configuration": {"type": "external"}},
                ]},
            },
            post={
                'https://a.blazemeter.com/api/v4/tests/4/files': {"result": {}},
            },
            patch={
                'https://a.blazemeter.com/api/v4/tests/4': {"result": {}},
            }
        )

        self.obj.settings["test"] = "ExternalTest"
        self.obj.prepare()
        self.assertEqual(14, len(self.mock.requests))

    def test_send_report_email_default(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, get={
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [
                {"id": 1, 'projectId': 1, 'name': 'Taurus Cloud Test', 'configuration': {'type': 'taurus'}}
            ]},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': [
                {'result': []},
                {'result': [{'id': 1}]}
            ],
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
        }, post={
            'https://a.blazemeter.com/api/v4/tests/1/start': {'result': {'id': 'mid'}}
        })
        self.obj.settings.merge({"project": "myproject"})
        self.obj.prepare()
        self.obj.startup()
        reqs = self.mock.requests
        self.assertEqual(reqs[13]['url'], 'https://a.blazemeter.com/api/v4/tests/1')
        self.assertEqual(reqs[13]['method'], 'PATCH')
        data = json.loads(reqs[13]['data'])
        plugins = data['configuration']['plugins']
        self.assertEqual(plugins["reportEmail"], {"enabled": False})

    def test_send_report_email(self):
        self.configure(engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}}, get={
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": [
                {"id": 1, 'projectId': 1, 'name': 'Taurus Cloud Test', 'configuration': {'type': 'taurus'}}
            ]},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': [
                {'result': []},
                {'result': [{'id': 1}]}
            ],
            'https://a.blazemeter.com/api/v4/multi-tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {'result': []},
        }, post={
            'https://a.blazemeter.com/api/v4/tests/1/start': {'result': {'id': 'mid'}}
        })
        self.obj.settings.merge({"project": "myproject", "send-report-email": True})
        self.obj.prepare()
        self.obj.startup()
        reqs = self.mock.requests
        self.assertEqual(reqs[13]['url'], 'https://a.blazemeter.com/api/v4/tests/1')
        self.assertEqual(reqs[13]['method'], 'PATCH')
        data = json.loads(reqs[13]['data'])
        plugins = data['configuration']['plugins']
        self.assertEqual(plugins["reportEmail"], {"enabled": True})

    def test_multi_account_choice(self):
        with open(RESOURCES_DIR + "json/blazemeter-api-accounts.json") as fhd:
            accs = json.loads(fhd.read())

        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            get={
                'https://a.blazemeter.com/api/v4/accounts': accs,
            },
        )

        self.obj.settings['launch-existing-test'] = False
        self.obj.prepare()
        exp = "https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100"
        self.assertEqual(exp, self.mock.requests[6]['url'])
        self.assertEqual(17, len(self.mock.requests))


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
                    }]},
            'https://a.blazemeter.com/api/v4/masters/1/reports/errorsreport/data?noDataError=false': {
                'result': []}})

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


class TestFunctionalBZAReader(BZTestCase):
    def test_simple(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/masters/1/reports/functional/groups': {"result": [{
                "groupId": "gid",
                "sessionId": "sid",
                "summary": {
                    "testsCount": 3,
                    "requestsCount": 3,
                    "errorsCount": 2,
                    "assertions": {
                        "count": 0,
                        "passed": 0
                    },
                    "responseTime": {
                        "sum": 0
                    },
                    "isFailed": True,
                    "failedCount": 2,
                    "failedPercentage": 100
                },
                "id": "gid",
                "name": None,
            }]},
            'https://a.blazemeter.com/api/v4/masters/1/reports/functional/groups/gid': {
                "api_version": 2,
                "error": None,
                "result": {
                    "groupId": "gid",
                    "samples": [
                        {
                            "id": "s1",
                            "label": "test_breaking",
                            "created": 1505824780,
                            "responseTime": None,
                            "assertions": [{
                                "isFailed": True,
                                "errorMessage": "Ima failed",
                            }],
                            "error": True,
                        },
                        {
                            "id": "s2",
                            "label": "test_failing",
                            "created": 1505824780,
                            "responseTime": None,
                            "assertions": None,
                            "error": True,
                        },
                        {
                            "id": "s3",
                            "label": "test_passing",
                            "created": 1505824780,
                            "responseTime": None,
                            "assertions": None,
                            "error": False,
                        }
                    ]

                }}})

        obj = FunctionalBZAReader(logging.getLogger(''))
        obj.master = Master(data={'id': 1})
        mock.apply(obj.master)
        samples = [x for x in obj.read(True)]
        self.assertEquals(3, len(samples))
        self.assertEqual(["BROKEN", "FAILED", "PASSED"], [sample.status for sample in samples])
        self.assertEqual(samples[0].error_msg, "Ima failed")
        self.assertEqual(samples[0].start_time, 1505824780)
        self.assertEqual(samples[0].duration, 0.0)
        self.assertEqual(["test_breaking", "test_failing", "test_passing"], [sample.test_case for sample in samples])
        self.assertEqual(samples[0].test_suite, "Tests")
