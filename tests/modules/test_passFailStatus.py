import json
import logging
import time

from tests import BZTestCase, __dir__, random_datapoint
from bzt import AutomatedShutdown
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.passfail import PassFailStatus
from tests.mocks import EngineEmul


class TestPassFailStatus(BZTestCase):
    def test_prepare(self):
        pass
