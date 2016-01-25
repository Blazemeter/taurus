""" test """
import time
import logging

from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet
from tests import BZTestCase, r, rc, err
from tests.mocks import MockReader


class TestDefaultAggregator(BZTestCase):
    def setUp(self):
        pass
