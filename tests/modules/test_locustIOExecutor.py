import logging
import time
import sys

from bzt import six
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.locustio import LocustIOExecutor, SlavesReader
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
    def setUp(self):
        pass