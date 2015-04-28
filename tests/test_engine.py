""" unit test """
import os
import tempfile
import shutil

from tests import BZTestCase, __dir__, local_paths_config
from tests.mocks import EngineEmul


class TestEngine(BZTestCase):
    def setUp(self):
        super(TestEngine, self).setUp()
        self.obj = EngineEmul()
        self.obj.artifacts_base_dir = tempfile.gettempdir() + "/bzt"
        self.paths = local_paths_config()

    def test_jmx(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/jmx.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()
        self.obj.run()
        self.obj.post_process()

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

    def test_grinder(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/grinder.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()
        self.obj.run()
        self.obj.post_process()

    def test_gatling(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/gatling.json",
            self.paths
        ]
        self.obj.configure(configs)
        self.obj.prepare()
        self.obj.run()
        self.obj.post_process()

    def test_gatling_spaces(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/gatling.json",
            self.paths
        ]
        self.obj.configure(configs)
        space = os.path.join(self.obj.artifacts_dir, "with space")
        os.makedirs(space)
        self.obj.artifacts_dir = space

        shutil.copy("tests/gatling/BasicSimulation.scala", space)

        self.obj.config.merge({"execution": {"scenario": {"script": os.path.join(space, "BasicSimulation.scala")}}})

        self.obj.prepare()
        self.obj.run()
        self.obj.post_process()
