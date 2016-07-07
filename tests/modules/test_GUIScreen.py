import time
from unittest import TestCase, skipIf

from urwid.canvas import Canvas

from bzt.engine import ManualShutdown
from bzt.modules.console import TaurusConsole
from bzt.utils import DummyScreen

try:
    from bzt.modules.screen import GUIScreen as Screen
except:
    Screen = DummyScreen


class TestCanvas(Canvas):
    def __init__(self, value):
        super(TestCanvas, self).__init__()
        self.value = value

    def content(self, trim_left=0, trim_top=0, cols=None, rows=None, attr=None):
        for val in self.value:
            yield val

    def rows(self):
        pass

    def content_delta(self):
        pass

    def cols(self):
        pass


class TestGUIScreen(TestCase):
    def test_draw_screen(self):
        lines = [((x[0], None, "%s\n" % x[0]),) for x in TaurusConsole.palette]
        canvas = TestCanvas(lines)

        obj = Screen()
        """
        :type: bzt.modules.screen.GUIScreen
        """
        obj.register_palette(TaurusConsole.palette)

        obj.start()
        for _ in range(1, 10):
            obj.draw_screen((1, 1), canvas)
            time.sleep(0.5)

        if hasattr(obj, 'font'):
            old_font_size = obj.font['size']
            obj.root.event_generate("<Control-4>")
            obj.root.event_generate("<Control-MouseWheel>", delta=120)
            if old_font_size > 0:
                self.assertGreater(obj.font['size'], old_font_size)
            else:
                self.assertLess(obj.font['size'], old_font_size)
            obj.root.event_generate("<Control-5>")
            obj.root.event_generate("<Control-MouseWheel>", delta=-120)

            self.assertEqual(obj.font['size'], old_font_size)

        obj.stop()

    @skipIf(Screen is DummyScreen, "skip test if GUI window isn't available")
    def test_window_closed(self):
        lines = [((x[0], None, "%s\n" % x[0]),) for x in TaurusConsole.palette]
        canvas = TestCanvas(lines)
        obj = Screen()
        obj.register_palette(TaurusConsole.palette)
        obj.start()
        for _ in range(5):
            obj.draw_screen((1, 1), canvas)
            time.sleep(0.1)
        # closing the window
        obj.closed_window()
        # first call to draw_screen should raise ManualShutdown
        self.assertRaises(ManualShutdown, obj.draw_screen, (1, 1), canvas)
        # consecutive calls to draw_screen shouldn't raise
        obj.draw_screen((1, 1), canvas)
        obj.draw_screen((1, 1), canvas)
