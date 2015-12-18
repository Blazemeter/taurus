import os
import time

from tests import BZTestCase
from bzt.modules.siege import SiegeExecutor
from tests.mocks import EngineEmul


class TestSiegeExecutor(BZTestCase):
    def test_shutdown(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": os.path.join(os.path.dirname(__file__), '..', 'siege', 'siege.sh')})
        obj.execution = ({
            "concurrency": 1,
            "iterations": 10,
        })

        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()

        self.assertNotEquals(obj.process, None)
