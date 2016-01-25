import time
from unittest import TestCase

from urwid.canvas import Canvas

from bzt.modules.console import TaurusConsole

try:
    from bzt.modules.screen import GUIScreen as Screen
except:
    from bzt.utils import DummyScreen as Screen


class TestCanvas(Canvas):
    def __init__(self, value):
        pass