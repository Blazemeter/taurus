import json
import logging
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul, ResultsFromBZA
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ModuleMock


class TestCloudProvisioning(BZTestCase):
    def test_simple(self):
        pass