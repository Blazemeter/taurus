import csv
import os
import shutil
import time

import yaml

from bzt.engine import ScenarioExecutor, Provisioning
from bzt.modules.selenium import SeleniumExecutor, JUnitJar
from tests import BZTestCase, local_paths_config, __dir__
from tests.mocks import EngineEmul


class SeleniumTestCase(BZTestCase):
    def setUp(self):
        pass