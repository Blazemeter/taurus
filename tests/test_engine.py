""" unit test """
import os
import glob
import logging

from bzt.engine import ScenarioExecutor
from bzt.utils import BetterDict, EXE_SUFFIX, is_windows
from bzt.six import string_types
from tests import BZTestCase, __dir__, local_paths_config
from tests.mocks import EngineEmul


class TestEngine(BZTestCase):
    def setUp(self):
        super(TestEngine, self).setUp()
        self.obj = EngineEmul()
        self.paths = local_paths_config()

    def test_requests(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/get-post.json",
            __dir__() + "/json/reporting.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()

        for executor in self.obj.provisioning.executors:
            executor._env['TEST_MODE'] = 'files'

        self.obj.run()
        self.obj.post_process()

    def test_double_exec(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/yaml/triple.yml",
            __dir__() + "/json/reporting.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()

        for executor in self.obj.provisioning.executors:
            executor._env['TEST_MODE'] = 'files'

        self.obj.run()

        files = list(glob.glob(self.obj.artifacts_dir + '/*.jtl'))

        log = logging.getLogger('')
        log.warning('test_double_exec: files list')
        log.warning(list((_file, os.stat(_file).st_size) for _file in files))

        self.obj.post_process()

    def test_unknown_module(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/gatling.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.config["provisioning"] = "unknown"
        self.obj.config["modules"]["unknown"] = BetterDict()

        try:
            self.obj.prepare()
            self.fail()
        except ValueError:
            pass


class TestScenarioExecutor(BZTestCase):
    def setUp(self):
        super(TestScenarioExecutor, self).setUp()
        self.engine = EngineEmul()
        self.executor = ScenarioExecutor()
        self.executor.engine = self.engine

    def test_scenario_extraction_script(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": {
                    "script": "path/to/example.script",
                    "param": "value"
                }}]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.executor.get_scenario()
        config = self.engine.config
        self.assertEqual(config['execution'][0]['scenario'], 'example.script')
        self.assertIn('example.script', config['scenarios'])

    def test_scenario_is_script(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": "path/to/example.script"
                }]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.executor.get_scenario()
        config = self.engine.config
        self.assertEqual(config['execution'][0]['scenario'], 'example.script')
        self.assertIn('example.script', config['scenarios'])

    def test_scenario_extraction_request(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": {
                    "requests": [{"url": "url.example"}],
                    "param": "value"
                }}]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.executor.get_scenario()
        config = self.engine.config
        scenario = config['execution'][0]['scenario']
        self.assertTrue(isinstance(scenario, string_types))
        self.assertIn(scenario, config['scenarios'])

    def test_creates_hostaliases_file(self):
        self.engine.config.merge({
            "settings": {
                "hostaliases": {
                    "demo": "blazedemo.com"
                }
            }
        })

        path = os.path.join(__dir__(), "data", "hostaliases" + EXE_SUFFIX)
        process = self.executor.execute([path])
        stdout, _ = process.communicate()
        hosts_file = os.path.join(self.engine.artifacts_dir, "hostaliases")

        self.assertTrue(os.path.exists(hosts_file))
        self.assertIn(hosts_file, str(stdout))

    def test_doesnt_create_hostaliases(self):
        self.executor.execute(["echo"])
        hosts_file = os.path.join(self.engine.artifacts_dir, "hostaliases")
        self.assertFalse(os.path.exists(hosts_file))

    def test_passes_artifacts_dir(self):
        cmdline = "echo %TAURUS_ARTIFACTS_DIR%" if is_windows() else "echo $TAURUS_ARTIFACTS_DIR"
        process = self.executor.execute(cmdline, shell=True)
        stdout, _ = process.communicate()
        self.assertEquals(self.engine.artifacts_dir, stdout.decode().strip())
