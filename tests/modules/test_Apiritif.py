import os
import time

from bzt.modules.apiritif import ApiritifExecutor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestApiritifExecutor(BZTestCase):
    def test_basic(self):
        obj = ApiritifExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../apiritif/test_api_example.py"
            }
        })
        obj.prepare()
        obj.get_widget()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertNotEquals(obj.process, None)

    def test_requests(self):
        obj = ApiritifExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "requests": [
                    "http://blazedemo.com/",
                    "https://api.github.com/",
                ]
            }
        })
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(obj.engine.artifacts_dir, "test_api.py")))
        obj.get_widget()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertNotEquals(obj.process, None)

