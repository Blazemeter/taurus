from . import MockJMeter
from tests import BZTestCase, RESOURCES_DIR


class TestJMeterTool(BZTestCase):
    def setUp(self):
        super(TestJMeterTool, self).setUp()
        self.obj = MockJMeter()

    def test_old_plugin(self):
        self.sniff_log(self.obj.log)
        self.obj.reaction = [{'output': ('one', 'Wrong command: install-for-jmx')}]
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/http.jmx")

        # mustn't call old plugin for detection
        msg = "pmgr can't discover jmx for plugins"
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())

    def test_new_plugin(self):
        self.sniff_log(self.obj.log)

        self.obj.reaction = [{"output": ("one", "two"), "raise": RuntimeError("runtime error")}]
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/http.jmx")

        self.assertIn("Failed to detect plugins", self.log_recorder.warn_buff.getvalue())

    def test_wrong_jmx_name(self):
        self.sniff_log(self.obj.log)

        self.obj.reaction = [{"output": ("one", "two")}]
        jmx_file = RESOURCES_DIR + "/jmeter/jmx/really_wrong_name.jmx"
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/really_wrong_name.jmx")

        self.assertIn(jmx_file + " not found", self.log_recorder.warn_buff.getvalue())

    def test_jvm_params(self):
        self.sniff_log(self.obj.log)

        jmx_file = RESOURCES_DIR + "/jmeter/jmx/really_wrong_name.jmx"
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/really_wrong_name.jmx")

        self.assertIn(jmx_file + " not found", self.log_recorder.warn_buff.getvalue())

