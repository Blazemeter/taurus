import time

from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.nose import NoseExecutor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestNoseExecutor(BZTestCase):
    def setUp(self):
        self.obj = NoseExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.aggregator = ConsolidatingAggregator()

    def test_single_file(self):
        self.obj.execution.merge({
            "test-suite": __dir__() + "/../nose/test_testy.py"
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
            self.obj.engine.aggregator.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_run_dir(self):
        raise NotImplementedError()

    def test_run_single_module(self):
        raise NotImplementedError()

    def test_run_package(self):
        raise NotImplementedError()

    def test_resource_files(self):
        raise NotImplementedError()
