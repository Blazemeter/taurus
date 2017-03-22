import os
import time

from bzt.modules.apirus import ApirusExecutor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestApirusExecutor(BZTestCase):
    def test_basic(self):
        obj = ApirusExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../apirus/test_api_example.py"
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
        obj = ApirusExecutor()
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

