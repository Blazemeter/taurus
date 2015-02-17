""" unit test """
import tempfile

from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestEngine(BZTestCase):
    def setUp(self):
        super(TestEngine, self).setUp()
        self.obj = EngineEmul()
        self.obj.artifacts_base_dir = tempfile.gettempdir() + "/bzt"

    def test_jmx(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/jmx.json",
        ]
        # self.obj.merge_local_jmeter_path()
        self.obj.prepare(configs)
        self.obj.run()
        self.obj.post_process()

    def test_requests(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/get-post.json",
            __dir__() + "/json/reporting.json",
        ]
        # self.obj.merge_local_jmeter_path()
        self.obj.prepare(configs)
        self.obj.run()
        self.obj.post_process()

    def test_double_exec(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/yaml/triple.yml",
            __dir__() + "/json/reporting.json",
        ]
        # self.obj.merge_local_jmeter_path()
        self.obj.prepare(configs)
        self.obj.run()
        self.obj.post_process()

    def test_loadosophia(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/get-post.json",
            __dir__() + "/json/loadosophia.json",
        ]
        # self.obj.merge_local_jmeter_path()
        self.obj.prepare(configs)
        self.obj.run()
        try:
            self.obj.post_process()
        except RuntimeError:
            pass

    def test_grinder(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/grinder.json",
        ]
        self.obj.prepare(configs)
        self.obj.merge_local_jmeter_path()
        self.obj.run()
        self.obj.post_process()

    def test_gatling(self):
        configs = [
            __dir__() + "/../bzt/10-base.json",
            __dir__() + "/json/gatling.json",
        ]
        self.obj.prepare(configs)
        self.obj.merge_local_jmeter_path()
        self.obj.run()
        self.obj.post_process()
