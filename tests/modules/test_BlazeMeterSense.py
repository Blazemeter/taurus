import logging
import os
import shutil
import time

from tests import BZTestCase, random_datapoint, __dir__
from bzt.modules.sense import BlazeMeterSenseReporter
from tests.mocks import EngineEmul


class TestSenseReporter(BZTestCase):
    def test_works(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings['token'] = 'faketoken'
        obj.engine = EngineEmul()

        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()
