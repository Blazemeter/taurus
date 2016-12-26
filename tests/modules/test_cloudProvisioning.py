import json
import os
import logging
import time
import tempfile
import shutil

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
    @staticmethod
    def __get_user_info():
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def setUp(self):
        engine = EngineEmul()
        engine.aggregator = ConsolidatingAggregator()
        self.obj = CloudProvisioning()
        self.obj.engine = engine
        self.obj.client = BlazeMeterClientEmul(self.obj.log)

    def configure(self, engine_cfg=None, client_results=None, add_config=True, add_settings=True):
        if engine_cfg is None:
            engine_cfg = {}
        self.obj.engine.config.merge(engine_cfg)

        if add_settings:
            self.obj.settings["token"] = "FakeToken"
            self.obj.settings["browser-open"] = False
            self.obj.settings['default-location'] = "us-west-1"

        if add_config:
            self.obj.engine.config.merge({
                "modules": {"mock": ModuleMock.__module__ + "." + ModuleMock.__name__},
                "provisioning": "mock"})

            self.obj.parameters = self.obj.engine.config.get('execution')

        if isinstance(self.obj.parameters, list):
            self.obj.parameters = self.obj.parameters[0]

        self.obj.client.results = client_results

    def test_simple(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {},  # upload files
                {"result": {"id": id(self.obj)}},  # start
                {"result": {"id": id(self.obj)}},  # get master
                {"result": []},  # get master sessions
                {}])  # terminate

        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, self.obj.executors[0].execution['locations']['us-west'])

        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_detach(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 55,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {},  # upload files
                {"result": {"id": id(self.obj)}}])  # start

        self.obj.settings["detach"] = True

        self.obj.prepare()
        self.assertEqual(1, len(self.obj.client.results))
        self.obj.startup()
        self.assertEqual([], self.obj.client.results)
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_no_settings(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {},  # upload files
                {"result": {"id": id(self.obj)}}])  # start

        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-west-1'])

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
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {}])  # upload files

        self.obj.prepare()
        modules = [reporter['module'] for reporter in self.obj.engine.config['reporting']]
        self.assertEquals(modules, ['second_reporter', 'third_reporter'])

    def test_widget_cloud_test(self):
        self.obj.test = CloudTaurusTest(self.obj.client, None, None, None, None, self.obj.log)
        self.configure(
            client_results=[
                {"result": []},
                {"result": {
                    "sessions": [{
                        "name": "executor/scenario/location",
                        "configuration": {}}]}},
                {"result": {"sessions": [{
                        "name": "executor/scenario/location",
                        "configuration": {
                            "location": "loc-name",
                            "serversCount": "10"}}]}}])

        widget = self.obj.get_widget()
        widget.update()
        widget.update()
        widget.update()
        widget.update()

        self.assertEqual("None #None\n executor scenario:\n  Agents in loc-name: 10\n", widget.text.get_text()[0])

    def test_widget_cloud_collection(self):
        self.obj.test = CloudCollectionTest(self.obj.client, None, None, None, None, self.obj.log)
        self.configure(
            client_results=[
                {"result": {
                    "sessions": [{
                        "id": "session-id",
                        "locationId": "loc-name",
                        "readyStatus": {"servers": ["server" for _ in range(10)]}}]}},
                {"result": {
                    "sessions": [{
                        "id": "session-id",
                        "name": "loc-name/scenario",
                        "configuration": {}}]}}])
        self.obj.test.get_master_status()
        widget = self.obj.get_widget()
        widget.update()

        self.assertEqual("None #None\n scenario:\n  Agents in loc-name: 10\n", widget.text.get_text()[0])

    def test_delete_test_files(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},  # collections
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                self.__get_user_info(),  # user
                {"files": [
                    {
                        "hash": "hash1",
                        "name": "file1"
                    }, {
                        "hash": "hash1",
                        "name": "file2"}]},  # test files
                {"removed": [
                    "hash1", "hash2"]},  # remove test files
                {}])  # upload files

        self.obj.settings.merge({'delete-test-files': True})

        self.obj.prepare()
        self.assertTrue(self.obj.client.delete_files_before_test)

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
            client_results=[
                {"result": []},
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"},}]},  # find test
                self.__get_user_info(),  # user
                {}])  # upload files

        self.obj.test = CloudTaurusTest(self.obj.client, None, None, "name", None, self.obj.log)
        cloud_config = self.obj.test.prepare_cloud_config(self.obj.engine.config)
        execution = cloud_config["execution"][0]
        self.assertNotIn("throughput", execution)
        self.assertNotIn("ramp-up", execution)
        self.assertNotIn("hold-for", execution)
        self.assertNotIn("steps", execution)

    def test_default_test_type_cloud(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                self.__get_user_info(),  # user
                {}])  # upload files

        self.obj.settings.merge({"delete-test-files": False})

        self.obj.prepare()
        self.assertIsInstance(self.obj.test, CloudTaurusTest)

    def test_type_forced(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {}])  # update collection

        self.obj.settings.merge({'delete-test-files': False})

        self.obj.prepare()
        self.assertIsInstance(self.obj.test, CloudCollectionTest)

    def test_detect_test_type_collection(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "items": [{"configuration": {"type": "taurus"}}]}]},  # detect collection
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {}])  # update collection

        self.obj.settings.merge({"delete-test-files": False})

        self.obj.prepare()
        self.assertIsInstance(self.obj.test, CloudCollectionTest)

    def test_detect_test_type_cloud(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},  # detect collection
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # detect test
                self.__get_user_info(),  # user
                {"result": [{
                    "id": 5174715,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                {},  # upload files
                {"result": {
                    "name": "Taurus Collection", "items": []}},  # transform config to collection
                {}])  # update collection

        self.obj.settings.merge({"delete-test-files": False})

        self.obj.prepare()
        self.assertIsInstance(self.obj.test, CloudTaurusTest)

    def test_full_collection(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 2}}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"files": []},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": id(self.obj.client)}},  # create collection
                {"result": {"id": id(self.obj)}},  # start
                {"result": {"id": id(self.obj), "sessions": []}},  # get master
                {"result": []},  # get master sessions
                {}])  # terminate

        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        self.assertEquals(1, self.obj.executors[0].execution['locations']['us-east-1'])
        self.assertEquals(2, self.obj.executors[0].execution['locations']['us-west'])

        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_create_project(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},  # projects
                {"result": {"id": 1428}},  # create project
                {"result": []},  # collections
                {"result": [{
                    "id": 5174715,
                    "projectId": 1428,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                self.__get_user_info(),  # locations
                {}])  # upload files

        self.obj.settings.merge({
            "delete-test-files": False,
            "project": "myproject"})

        self.obj.prepare()

    def test_reuse_project(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": [{"id": 1428, "name": "myproject"}]},  # projects
                {"result": []},  # collections
                {"result": [{
                    "id": 5174715,
                    "projectId": 1428,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                self.__get_user_info(),  # user
                {}])  # upload files

        self.obj.settings.merge({
            "delete-test-files": False,
            "project": "myproject"})

        self.obj.prepare()

    def test_reuse_project_id(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},  # collections
                {"result": [{
                    "id": 5174715,
                    "projectId": 1428,
                    "name": "Taurus Cloud Test",
                    "configuration": {"type": "taurus"}}]},  # find test
                self.__get_user_info(),  # user
                {}])  # upload files

        self.obj.settings.merge({
            "delete-test-files": False,
            "project": 1428})

        self.obj.prepare()

    def test_create_collection(self):
        self.configure(
            engine_cfg={ScenarioExecutor.EXEC: {"executor": "mock"}},
            client_results=[
                {"result": []},  # find collection
                {"result": []},  # find test
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": 42}}])  # create collection

        self.obj.settings.merge({
            "delete-test-files": False,
            "use-deprecated-api": False})

        self.obj.prepare()
        self.assertIsInstance(self.obj.test, CloudCollectionTest)

    def test_toplevel_locations(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500},
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2},
                "locations-weighted": True},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": 42}}])  # create collection

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
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500},
                "locations": {"us-not-found": 1}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info()])  # user

        self.obj.settings["use-deprecated-api"] = False

        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_sandbox_default_location(self):
        self.configure(
            add_settings=False,
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {}])  # upload files

        self.obj.settings["token"] = "FakeToken"
        self.obj.settings["browser-open"] = False
        self.obj.prepare()
        exec_locations = self.obj.executors[0].execution['locations']
        self.assertEquals(1, exec_locations['us-west-1'])

    def test_collection_defloc_sandbox(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500}},
            client_results=[
                {"result": []},  # find collection
                {"result": []},  # find test
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": 42}}])  # create collection

        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        exec_locations = self.obj.executors[0].execution['locations']
        expected_location = 'harbor-5591335d8588531f5cde3a04'
        self.assertIn(expected_location, exec_locations)
        self.assertEquals(1, exec_locations[expected_location])

    def test_locations_on_both_levels(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: [{
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {"eu-west-1": 1}}],
                "locations": {"ams3": 1}},
            client_results=[
                {"result": []},  # find test
                {"result": []},  # find collection
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": 42}}])  # create collection

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
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 1}}},
            client_results=[
                {"result": []},  # find collection
                {"result": []},  # find test
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {"name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": 42}},  # create collection
                {"result": {"id": id(self.obj)}},  # start
                {"result": {
                    "id": id(self.obj),
                    "sessions": [
                        {"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                        {"id": "s2", "status": "INIT_SCRIPT"}]}},  # status
                {"result": []},  # sessions
                {"result": {
                    "id": id(self.obj),
                    "sessions": [
                        {"id": "s1", "status": "JMETER_CONSOLE_INIT"},
                        {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}},
                {"result": []},  # sessions
                {"result": {}},  # force start
                {"result": {"id": id(self.obj)}},  # master status
                {"result": []},  # sessions
                {},  # graceful shutdown
                {"result": {"status": "ENDED"}}])  # master status

        self.obj.settings["check-interval"] = "0ms"  # do not skip checks
        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.check()  # this one should trigger force start
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertEqual(self.obj.client.results, [])

    def test_terminate_only(self):
        """  test is terminated only when it was started and didn't finished """
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 1}}},
            client_results=[
                {"result": []},  # find collection
                {"result": []},  # find test
                self.__get_user_info(),  # user
                {},  # upload files
                {"result": {
                    "name": "Taurus Collection", "items": []}},  # transform config to collection
                {"result": {"id": 42}},  # create collection
                {"result": {"id": id(self.obj)}},  # start
                {"result": {
                    "id": id(self.obj),
                    "sessions": [{
                        "id": "s1", "status": "JMETER_CONSOLE_INIT"},
                        {"id": "s2", "status": "JMETER_CONSOLE_INIT"}]}},
                {"result": []},  # sessions
                {"result": {}},  # force start
                {"result": {"progress": 120, "status": "ENDED"}},  # status should trigger shutdown
                {"result": []}])  # sessions

        self.obj.settings["check-interval"] = "0ms"  # do not skip checks
        self.obj.settings["use-deprecated-api"] = False

        self.obj.prepare()
        self.obj.startup()
        self.obj.check()  # this one should trigger force start
        self.assertTrue(self.obj.check())
        self.obj.shutdown()
        self.obj.post_process()
        self.assertEqual(self.obj.client.results, [])

    def test_cloud_paths(self):
        """
        Test different executor/path combinations for correct return values of get_resources_files

        :return:
        """
        self.configure(
            add_config=False, add_settings=False,
            client_results=[
                {"result": []},  # collection
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {}])  # upload files

        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)
        self.obj.engine.configure([
            __dir__() + '/../../bzt/10-base.json',
            __dir__() + '/../yaml/resource_files.yml'])
        self.obj.settings = self.obj.engine.config['modules']['cloud']

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

            shutil.copyfile(__dir__() + '/../jmeter/jmx/dummy.jmx', files_in_home[0]['fullname'])

            dir_path = get_full_path(os.path.join('~', 'example-of-directory'))
            os.mkdir(dir_path)

            for _file in files_in_home[1:]:
                open(_file['fullname'], 'a').close()

            self.obj.engine.file_search_paths = ['tests']  # config not in cwd

            # 'files' are treated similar in all executors so check only one
            self.obj.engine.config[ScenarioExecutor.EXEC][0]['files'] = [
                os.path.join(os.getcwd(), 'tests', 'test_CLI.py'),  # full path
                files_in_home[2]['shortname'],  # path from ~
                os.path.join('jmeter', 'jmeter-loader.bat'),  # relative path
                'mocks.py',  # only basename (look at file_search_paths)
                '~/example-of-directory']  # dir

            self.obj.prepare()

            debug = log_recorder.debug_buff.getvalue().split('\n')
            str_files = [line for line in debug if 'Replace file names in config' in line]
            self.assertEqual(1, len(str_files))
            res_files = [_file for _file in str_files[0].split('\'')[1::2]]
            half = int(len(res_files)/2)
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
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 5500,
                    "locations": {
                        "us-east-1": 1,
                        "us-west": 1}}},
            client_results=[
                {"result": []},  # collection
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {},  # upload files
                {"result": {"id": id(self.obj)}},  # start test
                {"result": {"id": id(self.obj)}},  # status
                {"result": []},  # sessions
                {"result": {"id": id(self.obj)}},  # status
                {"result": []}])  # sessions

        self.obj.settings["check-interval"] = "1s"

        self.obj.prepare()
        self.obj.startup()
        self.obj.check()  # this one should work
        self.obj.check()  # this one should be skipped
        time.sleep(1)
        self.obj.check()  # this one should work
        self.obj.check()  # this one should skip

        self.assertEqual(self.obj.client.results, [])

    def test_dump_locations(self):
        self.configure(client_results=[self.__get_user_info()])
        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)

        self.obj.settings["dump-locations"] = True
        self.obj.settings["use-deprecated-api"] = True
        self.assertRaises(ManualShutdown, self.obj.prepare)

        warnings = log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = log_recorder.info_buff.getvalue()
        self.assertIn("Location: DFW	Dallas (Rackspace)", info)
        self.assertIn("Location: us-west-2	US West (Oregon)", info)
        self.assertNotIn("Location: harbor-5591335d8588531f5cde3a04	Sandbox", info)

        self.obj.post_process()

    def test_dump_locations_new_style(self):
        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)
        self.configure(client_results=[self.__get_user_info()])
        self.obj.settings["dump-locations"] = True
        self.obj.settings["use-deprecated-api"] = False
        self.assertRaises(ManualShutdown, self.obj.prepare)

        warnings = log_recorder.warn_buff.getvalue()
        self.assertIn("Dumping available locations instead of running the test", warnings)
        info = log_recorder.info_buff.getvalue()
        self.assertIn("Location: DFW	Dallas (Rackspace)", info)
        self.assertIn("Location: us-west-2	US West (Oregon)", info)
        self.assertIn("Location: harbor-5591335d8588531f5cde3a04	Sandbox", info)

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
            client_results=[
                {"result": []},  # collection
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {}])  # upload files

        # these should override 'blazemeter' settings
        self.obj.settings["check-interval"] = 20.0
        self.obj.settings["browser-open"] = "both"

        self.obj.prepare()

        self.assertEqual(self.obj.detach, True)
        self.assertEqual(self.obj.browser_open, "both")
        self.assertEqual(self.obj.client.token, "bmtoken")
        self.assertEqual(self.obj.check_interval, 20.0)
        self.assertEqual(self.obj.client.results, [])

    def test_public_report(self):
        self.configure(
            engine_cfg={
                ScenarioExecutor.EXEC: {
                    "executor": "mock",
                    "concurrency": 1,
                    "locations": {
                        "us-west": 2
                    }}},
            client_results=[
                {"result": []},  # collections
                {"result": []},  # tests
                self.__get_user_info(),  # user
                {"result": {"id": id(self.obj.client)}},  # create test
                {"files": []},  # create test
                {},  # upload files
                {"result": {"id": id(self.obj)}},  # start
                {'result': {'publicToken': 'publicToken'}},  # publish report
                {"result": {"id": id(self.obj)}},  # get master
                {"result": []},  # get master sessions
                {}])  # terminate

        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)

        self.obj.settings['public-report'] = True
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

        log_buff = log_recorder.info_buff.getvalue()
        log_line = "Public report link: https://a.blazemeter.com/app/?public-token=publicToken#/masters/%s/summary"
        self.assertIn(log_line % id(self.obj), log_buff)

        self.assertEqual(self.obj.client.results, [])


class TestCloudTaurusTest(BZTestCase):
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
                }, {
                    "id": "e843ff89a5737891a10251cbb0db08e5",
                    "name": "http://blazedemo.com/"
                }]})
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
                        }]}]})
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
                }]})
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
