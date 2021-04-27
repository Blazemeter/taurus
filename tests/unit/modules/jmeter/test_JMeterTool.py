import os
from bzt.utils import EXE_SUFFIX

from . import MockJMeter
from bzt.modules.jmeter import JTLReader, FuncJTLReader, JMeter
from tests.unit import BZTestCase, RESOURCES_DIR


class TestJMeterTool(BZTestCase):
    def setUp(self):
        super(TestJMeterTool, self).setUp()
        self.obj = MockJMeter()

    def test_additional_jvm_props(self):
        self.obj.tool_path = os.path.join(RESOURCES_DIR, "jmeter/jmeter-loader" + EXE_SUFFIX)
        props = {"n1": "v1", "n2": "v2"}
        self.obj.env.set({"TEST_MODE": "jvm_args"})
        for key in props:
            self.obj.env.add_java_param({"JVM_ARGS": "-D%s=%s" % (key, props[key])})
        out, _ = self.obj.call([self.obj.tool_path])
        self.assertIn('-Dn1=v1', out)
        self.assertIn('-Dn2=v2', out)

    def test_old_plugin(self):
        self.sniff_log(self.obj.log)
        self.obj.reaction = [{'output': ('one', 'Wrong command: install-for-jmx')}]
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/http.jmx")

        # mustn't call old plugin for detection
        msg = "pmgr can't discover jmx for plugins"
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())

    def test_new_plugin(self):
        self.sniff_log(self.obj.log)

        self.obj.reaction = [{"output": ("one", "two"), "raise": OSError("runtime error")}]
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/http.jmx")

        self.assertIn("Failed to detect plugins", self.log_recorder.warn_buff.getvalue())

    def test_wrong_jmx_name(self):
        self.sniff_log(self.obj.log)

        self.obj.reaction = [{"output": ("one", "two")}]
        jmx_file = RESOURCES_DIR + "/jmeter/jmx/really_wrong_name.jmx"
        self.obj.install_for_jmx(RESOURCES_DIR + "/jmeter/jmx/really_wrong_name.jmx")

        self.assertIn(jmx_file + " not found", self.log_recorder.warn_buff.getvalue())

    def test_plugins_manager_and_command_runner_default_urls(self):
        self.obj.__init__()
        self.assertEqual(self.obj.plugins_manager, JMeter.PLUGINS_MANAGER.format(version=JMeter.PLUGINS_MANAGER_VERSION))
        self.assertEqual(self.obj.command_runner, JMeter.COMMAND_RUNNER.format(version=JMeter.COMMAND_RUNNER_VERSION))

    def test_plugins_manager_and_command_runner_configured_url(self):
        plugins_manager_test_url = "http://somewhere.else/plugins-manager/{version}/plugins-manager.jar"
        plugins_manager_test_version = "1.0"
        command_runner_test_url = "http://somewhere.else/command-runner/{version}/command-runner.jar"
        command_runner_test_version = "1.0"
        self.obj.__init__(config={
            "plugins-manager": {
                "download-link": plugins_manager_test_url,
                "version": plugins_manager_test_version
            },
            "command-runner": {
                "download-link": command_runner_test_url,
                "version": command_runner_test_version
            }
        })
        self.assertEqual(self.obj.plugins_manager, plugins_manager_test_url.format(version=plugins_manager_test_version))
        self.assertEqual(self.obj.command_runner, command_runner_test_url.format(version=command_runner_test_version))
