
from tests import BZTestCase
from bzt.modules.ab import ABExecutor
from tests.mocks import EngineEmul


class TestApacheBenchExecutor(BZTestCase):
    def test_startup(self):
        obj = ABExecutor()
        obj.prepare()
        obj.startup()
        self.assertEqual(len(obj.reader.output), 9)

