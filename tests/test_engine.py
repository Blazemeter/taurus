""" unit test """
import os
import time

from bzt.engine import ScenarioExecutor
from bzt.utils import BetterDict, EXE_SUFFIX
from tests import BZTestCase, __dir__, local_paths_config
from tests.mocks import EngineEmul


class TestEngine(BZTestCase):
    def setUp(self):
        super(TestEngine, self).setUp()
        self.obj = EngineEmul()
        self.paths = local_paths_config()

    def test_delay(self):
        configs = [__dir__() + "/../bzt/10-base.json",
                   __dir__() + "/yaml/delay_0.yml",

                   self.paths]
        self.obj.configure(configs)
        self.obj.prepare()
        start0 = time.time()
        self.obj.run()
        finish0 = time.time()
        self.obj.post_process()

        configs = [__dir__() + "/../bzt/10-base.json",
                   __dir__() + "/yaml/delay_1.yml",
                   self.paths]
        self.obj.configure(configs)
        self.obj.prepare()
        start1 = time.time()
        self.obj.run()
        finish1 = time.time()
        self.obj.post_process()

        self.assertTrue(finish1 - start1 > finish0 - start0)

    def test_requests(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/get-post.json",
            __dir__() + "/json/reporting.json",
            self.paths
        ]
        self.obj.configure(configs)
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
