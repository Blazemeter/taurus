import os
import time
import logging
from tests import BZTestCase
from bzt.modules.siege import SiegeExecutor, DataLogReader
from tests.mocks import EngineEmul
from bzt.utils import BetterDict


class TestSiegeExecutor(BZTestCase):
    def test_shutdown(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": os.path.join(os.path.dirname(__file__), '..', 'siege', 'siege.sh')})
        obj.execution = BetterDict()
        obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
        })
        obj.execution.merge({"scenario": {"requests": "http://blazedemo.com"}})

        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()

        obj.post_process()
        self.assertNotEquals(obj.process, None)


class TestDataLogReader(BZTestCase):
    def test_read(self):
        obj = DataLogReader(os.path.join(os.path.dirname(__file__), '..', 'siege', 'siege.out'), logging.getLogger(''))
        for values in obj._read():
            if values is not None:
                self.assertEqual(len(values), 9)
                self.assertIsNone(values[7])
