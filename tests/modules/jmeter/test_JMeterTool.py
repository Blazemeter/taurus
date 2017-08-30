from . import MockJMeter
from tests import BZTestCase, RESOURCES_DIR


class TestJMeterTool(BZTestCase):
    def setUp(self):
        super(TestJMeterTool, self).setUp()
        self.obj = MockJMeter()

    def test_old_plugin(self):
        self.sniff_log(self.obj.log)
        msg = "Old pmgr can't discover jmx for plugins"
        self.obj.reaction = [{'output': ('one', 'two')}, {'raise': RuntimeError(msg)}]
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/http.jmx")

        # mustn't call old plugin for detection
        self.assertNotIn(msg, self.log_recorder.warn_buff.getvalue())

    def test_new_plugin(self):
        self.sniff_log(self.obj.log)
        msg = "New pmgr tried to discover jmx"
        self.obj.reaction = [{'output': ('Options: help, install-for-jmx', 'two')}, {'raise': RuntimeError(msg)}]
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/http.jmx")

        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())
