import os
import re
import shutil
import time
import logging

from bzt.modules.gatling import GatlingExecutor, DataLogReader, Gatling
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX


class TestGatlingExecutor(BZTestCase):
    def getGatling(self):
        pass