import time

from bzt.engine import Provisioning, ScenarioExecutor
from bzt.modules.jmeter import JMeterExecutor
from bzt.modules.provisioning import Local
from bzt.modules.console import ConsoleStatusReporter
from bzt.modules.aggregator import DataPoint, KPISet
from tests import BZTestCase, r, rc
from tests.mocks import EngineEmul


class TestConsoleStatusReporter(BZTestCase):
    def __get_datapoint(self, n):
        pass