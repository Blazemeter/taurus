""" unit test """
from bzt import TaurusConfigError
from tests import BZTestCase, local_paths_config, RESOURCES_DIR

from bzt.engine import ScenarioExecutor
from bzt.six import string_types, communicate
from bzt.utils import BetterDict, is_windows
from tests.mocks import EngineEmul


class TestEngine(BZTestCase):
    def setUp(self):
        super(TestEngine, self).setUp()
        self.obj = EngineEmul()
        self.paths = local_paths_config()

    def test_missed_config(self):
        configs = ['difinitely_missed.file']
        try:
            self.obj.configure(configs)
            self.fail()
        except TaurusConfigError as exc:
            self.assertIn('reading config file', str(exc))

    def test_requests(self):
        configs = [
            RESOURCES_DIR + "json/get-post.json",
            RESOURCES_DIR + "json/reporting.json",
            self.paths]
        self.obj.configure(configs)
        self.obj.prepare()

        for executor in self.obj.provisioning.executors:
            executor.env.set({"TEST_MODE": "files"})

        self.obj.run()
        self.obj.post_process()

    def test_double_exec(self):
        configs = [
            RESOURCES_DIR + "yaml/triple.yml",
            RESOURCES_DIR + "json/reporting.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()

        self.assertEquals(1, len(self.obj.services))

        for executor in self.obj.provisioning.executors:
            executor.env.set({"TEST_MODE": "files"})

        self.obj.run()
        self.obj.post_process()

    def test_unknown_module(self):
        configs = [
            RESOURCES_DIR + "json/gatling.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.config["provisioning"] = "unknown"
        self.obj.config["modules"]["unknown"] = BetterDict()

        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_null_aggregator(self):
        self.obj.config.merge({
            "execution": [{
                "scenario": {
                    "requests": [{"url": "http://example.com/"}],
                }}],
            "settings": {
                "aggregator": None,
                "default-executor": "jmeter",
            },
            "modules": {
                "local": "bzt.modules.provisioning.Local",
                "jmeter": "tests.modules.jmeter.MockJMeterExecutor",
            }})
        self.obj.prepare()

    def test_yaml_multi_docs(self):
        configs = [
            RESOURCES_DIR + "yaml/multi-docs.yml",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()
        self.assertEqual(len(self.obj.config["execution"]), 2)

    def test_json_format_regression(self):
        configs = [
            RESOURCES_DIR + "json/json-but-not-yaml.json"
        ]
        self.obj.configure(configs)
        self.obj.prepare()

    def test_invalid_format(self):
        configs = [
            RESOURCES_DIR + "jmeter-dist-3.0.zip"
        ]
        self.assertRaises(TaurusConfigError, lambda: self.obj.configure(configs))


class TestScenarioExecutor(BZTestCase):
    def setUp(self):
        super(TestScenarioExecutor, self).setUp()
        self.engine = EngineEmul()
        self.executor = ScenarioExecutor()
        self.executor.engine = self.engine
        self.executor.env = self.executor.engine.env

    def test_scenario_extraction_script(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": {
                    "script": "tests/resources/selenium/python/test_blazemeter_fail.py",
                    "param": "value"
                }}]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.executor.get_scenario()
        config = self.engine.config
        self.assertEqual(config['execution'][0]['scenario'], 'test_blazemeter_fail.py')
        self.assertIn('test_blazemeter_fail.py', config['scenarios'])

    def test_body_files(self):
        body_file1 = RESOURCES_DIR + "jmeter/body-file.dat"
        body_file2 = RESOURCES_DIR + "jmeter/jmx/http.jmx"
        self.engine.config.merge({
            'execution': [{
                'iterations': 1,
                'executor': 'siege',
                'scenario': 'bf'}],
            'scenarios': {
                'bf': {
                    "requests": [
                        {
                            'url': 'http://first.com',
                            'body-file': body_file1
                        }, {
                            'url': 'http://second.com',
                            'body': 'body2',
                            'body-file': body_file2}]}}})
        self.executor.execution = self.engine.config.get('execution')[0]
        scenario = self.executor.get_scenario()

        # check body fields in get_requests() results
        reqs = list(scenario.get_requests())
        body_fields = [req.body for req in reqs]
        self.assertIn('sample of body', body_fields[0])
        self.assertIn('body2', body_fields[1])

        # check body fields and body-files fields after get_requests()
        scenario = self.executor.get_scenario()
        body_files = [req.get('body-file') for req in scenario.get('requests')]
        body_fields = [req.get('body') for req in scenario.get('requests')]
        self.assertTrue(all(body_files))
        self.assertEqual(None, body_fields[0])
        self.assertIn('body2', body_fields[1])

    def test_scenario_is_script(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": "tests/resources/selenium/python/test_blazemeter_fail.py"
            }]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.executor.get_scenario()
        config = self.engine.config
        self.assertEqual(config['execution'][0]['scenario'], 'test_blazemeter_fail.py')
        self.assertIn('test_blazemeter_fail.py', config['scenarios'])

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

    def test_scenario_not_found(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": "non-existent"
            }]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.assertRaises(TaurusConfigError, self.executor.get_scenario)

    def test_scenario_no_requests(self):
        self.engine.config.merge({
            "execution": [{
                "scenario": ["url1", "url2"]
            }]})
        self.executor.execution = self.engine.config.get('execution')[0]
        self.assertRaises(TaurusConfigError, self.executor.get_scenario)

    def test_passes_artifacts_dir(self):
        cmdline = "echo %TAURUS_ARTIFACTS_DIR%" if is_windows() else "echo $TAURUS_ARTIFACTS_DIR"
        self.engine.prepare()
        self.executor.env.set(self.engine.env.get())
        process = self.executor.execute(cmdline, shell=True)
        stdout, _ = communicate(process)
        self.assertEquals(self.engine.artifacts_dir, stdout.strip())

    def test_case_of_variables(self):
        env = {'aaa': 333, 'AAA': 666}
        line_tpl = "echo %%%s%%" if is_windows() else "echo $%s"
        cmdlines = [line_tpl % "aaa", line_tpl % "AAA"]
        results = set()

        for cmdline in cmdlines:
            self.executor.env.set(env)
            process = self.executor.execute(cmdline, shell=True)
            stdout, _ = communicate(process)
            results.add(stdout.strip())
        if is_windows():
            self.assertEqual(1, len(results))
        else:
            self.assertEqual(2, len(results))
