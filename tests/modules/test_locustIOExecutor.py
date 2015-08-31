import time

from bzt.modules.locustio import LocustIOExecutor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
    def test_simple(self):
        obj = LocustIOExecutor()
        obj.engine = EngineEmul()
        obj.engine.config['provisioning'] = 'local'
        obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "scenario": {
                "script": __dir__() + "/../locust/simple.py"
            }
        })

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(obj.engine.check_interval)
        obj.shutdown()
        obj.post_process()
