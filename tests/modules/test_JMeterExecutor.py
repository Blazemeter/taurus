""" test """
import json
import logging
import os
import shutil
import sys
import time
from math import ceil

import yaml

from bzt.engine import Provisioning
from bzt.jmx import JMX
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.jmeter import JMeterExecutor, JTLErrorsReader, JTLReader, JMeter
from bzt.modules.jmeter import JMeterJTLLoaderExecutor, JMeterScenarioBuilder
from bzt.six import etree
from bzt.utils import BetterDict, EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ResultChecker, RecordingHandler


class TestJMeterExecutor(BZTestCase):
    def test_jmx(self):
       pass