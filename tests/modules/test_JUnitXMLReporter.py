import os
import tempfile
from collections import Counter

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.blazemeter import BlazeMeterUploader
from bzt.modules.passfail import PassFailStatus, DataCriteria
from bzt.modules.reporting import JUnitXMLReporter
from bzt.six import etree
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul


class TestJUnitXML(BZTestCase):
    def test_prepare_filename_in_settings(self):
        pass