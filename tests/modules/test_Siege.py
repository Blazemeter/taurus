import logging
import time
from os import path

from bzt.modules.siege import SiegeExecutor, DataLogReader
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX


TOOL_NAME = 'siege' + EXE_SUFFIX


def get_res_path(resource):
    pass