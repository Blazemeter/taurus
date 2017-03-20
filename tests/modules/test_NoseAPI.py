import time
from os import path

from bzt.modules.nose_api import NoseAPIExecutor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'ab', resource)


class TestApacheBenchExecutor(BZTestCase):
    def test_basic(self):
        obj = NoseAPIExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../api/test_api_example.py"
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
