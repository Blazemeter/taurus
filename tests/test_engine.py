""" unit test """
import os

from bzt.utils import BetterDict
from bzt.engine import ScenarioExecutor
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
        self.obj.prepare()
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
        self.obj.run()
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

    def test_creates_hosts_file(self):
        self.engine.config.merge({
            "settings": {
                "hostaliases": {
                    "demo": "blazedemo.com"
                }
            }
        })

        self.executor._prepare_hosts_file()
        hosts_file = os.path.join(self.engine.artifacts_dir, "hostaliases")
        self.assertTrue(os.path.exists(hosts_file))

    def test_hostaliases_list(self):
        self.engine.config.merge({
            "settings": {
                "hostaliases": ["demo blazedemo.com"],
            }
        })
        self.assertRaises(ValueError, self.executor._prepare_hosts_file)

    def test_hostaliases_file_value(self):
        self.engine.config.merge({
            "settings": {
                "hostaliases": __dir__() + "/data/hostsfile",
            }
        })
        self.executor._prepare_hosts_file()

    def test_hostaliases_file_doesnt_exist(self):
        self.engine.config.merge({
            "settings": {
                "hostaliases": __dir__() + "/***",
            }
        })
        self.assertRaises(ValueError, self.executor._prepare_hosts_file)