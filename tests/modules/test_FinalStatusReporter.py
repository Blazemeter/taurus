from collections import Counter
import time

from tests import BZTestCase, random_datapoint
from tests.mocks import EngineEmul, RecordingHandler
from bzt.modules.reporting import FinalStatus
from bzt.utils import BetterDict
from bzt.modules.aggregator import DataPoint, KPISet


class TestFinalStatusReporter(BZTestCase):
    def test_log_messages_failed_labels(self):
        pass