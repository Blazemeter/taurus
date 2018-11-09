""" unit test """
import os

from bzt import TaurusConfigError
from bzt.engine import ScenarioExecutor, Configuration
from bzt.six import string_types, communicate
from bzt.utils import BetterDict, is_windows
from tests import BZTestCase, local_paths_config, RESOURCES_DIR
from tests.mocks import EngineEmul


class TestEngine(BZTestCase):
    def setUp(self):
        super(TestEngine, self).setUp()
        self.obj = EngineEmul()
        self.paths = local_paths_config()

    def test_find_file(self):
        self.sniff_log(self.obj.log)

        config = RESOURCES_DIR + "json/get-post.json"
        configs = [config, self.paths]
        self.obj.configure(configs)
        self.assertEqual(2, len(self.obj.file_search_paths))

        self.obj.find_file(config)
        self.assertEqual("", self.log_recorder.warn_buff.getvalue())

        self.obj.find_file("reporting.json")
        self.assertIn("Guessed location", self.log_recorder.warn_buff.getvalue())

        self.obj.find_file("definitely_missed.file")
        self.assertIn("Could not find", self.log_recorder.warn_buff.getvalue())

        self.obj.find_file("http://localhost:8000/BlazeDemo.html")
        self.assertIn("Downloading http://localhost:8000/BlazeDemo.html", self.log_recorder.info_buff.getvalue())

    def test_missed_config(self):
        configs = ['definitely_missed.file']
        try:
            self.obj.configure(configs)
            self.fail()
        except TaurusConfigError as exc:
            self.assertIn('reading config file', str(exc))

    def test_configuration_smoothness(self):
        def find_ad_dict_ed(*args):
            if isinstance(args[0], dict) and not isinstance(args[0], BetterDict):
                raise BaseException("dict found in Configuration")

        configs = [
            RESOURCES_DIR + "json/get-post.json",
            self.paths]
        self.obj.configure(configs)
        self.assertTrue(isinstance(self.obj.config, Configuration))
        BetterDict.traverse(self.obj.config, find_ad_dict_ed)

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
                "jmeter": {"class": "tests.modules.jmeter.MockJMeterExecutor",
                           "protocol-handlers": {"http": "bzt.jmx.http.HTTPProtocolHandler"}},
            }})
        self.obj.unify_config()
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

    def test_included_configs(self):
        configs = [
            RESOURCES_DIR + "yaml/included-level1.yml",
        ]
        self.obj.configure(configs)
        self.assertTrue(self.obj.config["level1"])
        self.assertTrue(self.obj.config["level2"])
        self.assertTrue(self.obj.config["level3"])
        self.assertListEqual(['included-level2.yml', 'included-level3.yml'], self.obj.config["included-configs"])

    def test_included_configs_cycle(self):
        configs = [
            RESOURCES_DIR + "yaml/included-circular1.yml",
        ]
        self.obj.configure(configs)
        self.assertTrue(self.obj.config["level1"])
        self.assertTrue(self.obj.config["level2"])
        self.assertListEqual(['included-circular2.yml', 'included-circular1.yml', 'included-circular2.yml'],
                             self.obj.config["included-configs"])

    def test_env_eval(self):
        configs = [
            RESOURCES_DIR + "yaml/env-eval.yml",
        ]
        os.environ["BZT_ENV_TEST_UNSET"] = "set"
        try:
            self.obj.configure(configs)
            self.obj.eval_env()
            self.assertEquals("success/top", self.obj.config["toplevel"])
            self.assertEquals("success/test/", self.obj.config["settings"]["artifacts-dir"])
            self.assertEquals("http://success/", self.obj.config["scenarios"]["scen1"]["default-address"])
            self.assertEquals("/success/", self.obj.config["scenarios"]["scen1"]["requests"][0])
            self.assertNotEquals("/${PATH}/", self.obj.config["scenarios"]["scen1"]["requests"][1])
            self.assertEquals("/${TEMP}/", self.obj.config["scenarios"]["scen1"]["requests"][2])
            self.assertEquals("/" + self.obj.artifacts_dir + "/", self.obj.config["scenarios"]["scen1"]["requests"][3])
        finally:
            if "BZT_ENV_TEST" in os.environ:
                os.environ.pop("BZT_ENV_TEST")
            if "BZT_ENV_TEST_UNSET" in os.environ:
                os.environ.pop("BZT_ENV_TEST_UNSET")

    def test_singletone_service(self):
        configs = [
            RESOURCES_DIR + "yaml/singletone-service.yml",
        ]
        self.obj.configure(configs, read_config_files=False)
        self.obj.prepare()
        self.assertEquals(2, len(self.obj.services))
        self.assertEquals(None, self.obj.services[0].parameters['run-at'])
        self.assertEquals("mock", self.obj.services[1].parameters['run-at'])
        self.assertEquals(2, len(self.obj.reporters))
        self.assertEquals("mock", self.obj.reporters[0].parameters['run-at'])
        self.assertEquals(None, self.obj.reporters[1].parameters['run-at'])


class TestScenarioExecutor(BZTestCase):
    def setUp(self):
        super(TestScenarioExecutor, self).setUp()
        self.engine = EngineEmul()
        self.executor = ScenarioExecutor()
        self.executor.engine = self.engine
        self.executor.env = self.executor.engine.env

    def configure(self, config):
        self.engine.config.merge({"settings": {"default-executor": "jmeter"}})
        self.engine.config.merge(config)
        self.engine.unify_config()
        self.executor.execution = self.engine.config.get(ScenarioExecutor.EXEC)[0]

    def test_scenario_extraction_script(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "script": "tests/resources/selenium/python/test_blazemeter_fail.py",
                    "param": "value"
                }}]})
        self.executor.get_scenario()
        config = self.engine.config
        self.assertEqual(config['execution'][0]['scenario'], 'test_blazemeter_fail.py')
        self.assertIn('test_blazemeter_fail.py', config['scenarios'])

    def test_body_files(self):
        body_file1 = RESOURCES_DIR + "jmeter/body-file.dat"
        body_file2 = RESOURCES_DIR + "jmeter/jmx/http.jmx"
        self.configure({
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
        self.assertFalse(body_fields[0])
        self.assertIn('body2', body_fields[1])

    def test_scenario_is_script(self):
        self.configure({
            "execution": [{
                "scenario": "tests/resources/selenium/python/test_blazemeter_fail.py"
            }]})
        self.executor.get_scenario()
        config = self.engine.config
        self.assertEqual(config['execution'][0]['scenario'], 'test_blazemeter_fail.py')
        self.assertIn('test_blazemeter_fail.py', config['scenarios'])

    def test_scenario_extraction_request(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{"url": "url.example"}],
                    "param": "value"
                }}})
        self.executor.get_scenario()
        config = self.engine.config
        scenario = config['execution'][0]['scenario']
        self.assertTrue(isinstance(scenario, string_types))
        self.assertIn(scenario, config['scenarios'])

    def test_scenario_not_found(self):
        self.configure({ScenarioExecutor.EXEC: {
            "execution": {
                "scenario": "non-existent"
            }}})
        self.assertRaises(TaurusConfigError, self.executor.get_scenario)

    def test_scenario_no_requests(self):
        self.configure({
            "execution": {
                "scenario": ["url1", "url2"]
            }})
        self.assertRaises(TaurusConfigError, self.executor.get_scenario)

    def test_passes_artifacts_dir(self):
        cmdline = "echo %TAURUS_ARTIFACTS_DIR%" if is_windows() else "echo $TAURUS_ARTIFACTS_DIR"
        self.engine.eval_env()
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

    def test_get_load_str(self):
        self.configure({ScenarioExecutor.EXEC: {
            "concurrency": "2",
            "hold-for": "3",
            "ramp-up": "4",
            "iterations": "5",
            "throughput": "6",
            "steps": "7",
        }})
        load = self.executor.get_load()
        self.assertEquals(2, load.concurrency)
        self.assertEquals(3, load.hold)
        self.assertEquals(4, load.ramp_up)
        self.assertEquals(5, load.iterations)
        self.assertEquals(6, load.throughput)
        self.assertEquals(7, load.steps)

    def test_get_load_str_fail(self):
        self.configure({ScenarioExecutor.EXEC: {"concurrency": "2VU"}})
        self.assertRaises(TaurusConfigError, self.executor.get_load)
