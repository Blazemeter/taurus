import os
import time
from subprocess import CalledProcessError

from bzt.engine import Service
from bzt.modules.shellexec import ShellExecutor
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul, RecordingHandler


class TaskTestCase(BZTestCase):
    def setUp(self):
        pass