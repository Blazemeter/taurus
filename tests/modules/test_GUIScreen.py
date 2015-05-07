from unittest import TestCase
import time

from urwid.canvas import Canvas

from bzt.modules.console import TaurusConsole

from bzt.modules.screen import GUIScreen


class TestCanvas(Canvas):
    def __init__(self, value):
        super(TestCanvas, self).__init__()
        self.value = value

    def content(self, trim_left=0, trim_top=0, cols=None, rows=None, attr=None):
        for val in self.value:
            yield val


class TestGUIScreen(TestCase):
    def test_draw_screen(self):
        canvas = TestCanvas([[(x[0], None, x[0] + "\n") for x in TaurusConsole.palette]])

        obj = GUIScreen()
        obj.register_palette(TaurusConsole.palette)

        obj.start()
        for _ in range(1, 10):
            obj.draw_screen((1, 1), canvas)
            time.sleep(0.5)
        obj.stop()