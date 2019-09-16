import time

from urwid.canvas import Canvas

from bzt import ManualShutdown
from bzt.modules.console import TaurusConsole
from bzt.utils import is_linux
from tests import BZTestCase

from bzt.modules.screen import GUIScreen as Screen


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


class TestGUIScreen(BZTestCase):
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
            old_font_size = 10
            obj.font['size'] = old_font_size
            self.assertGreater(old_font_size, 0)

            if is_linux():
                obj.root.event_generate("<Control-4>")
            else:
                obj.root.event_generate("<Control-MouseWheel>", delta=120)

            self.assertGreater(obj.font['size'], old_font_size)

            if is_linux():
                obj.root.event_generate("<Control-5>")
            else:
                obj.root.event_generate("<Control-MouseWheel>", delta=-120)

            self.assertEqual(obj.font['size'], old_font_size)

        obj.stop()

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
