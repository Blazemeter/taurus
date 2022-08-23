import os
from bzt.utils import EXE_SUFFIX, get_full_path, BetterDict
from bzt.modules.jmeter import JMeter

from . import MockJMeter
from tests.unit import BZTestCase, RESOURCES_DIR


class TestJMeterTool(BZTestCase):
    def setUp(self):
        super(TestJMeterTool, self).setUp()
        config = BetterDict()
        self.obj = MockJMeter(config=config)

    def test_get_jar_fixes(self):
        lib_dir_path = get_full_path(__file__, step_up=1)
        lib_dir_content = [
            "xstream-1.4.15.jar",   # lib with some vulnerability, must be replaced
            "log4j-core-2.16.jar",  # only old jmeter versions contains affected log4j components
            "some-other-lib-0.99.jar"]  # other jar, mustn't be touched
        listdir = lambda _: lib_dir_content
        self.obj.version = '5.0.0'

        saved_listdir = os.listdir
        os.listdir = listdir
        try:
            jar_tools = self.obj._get_jar_fixes(lib_dir_path)
            target_tools_list = [
                [
                    # Needs to be <1.4.18 for now https://stackoverflow.com/questions/30812293/com-thoughtworks-xstream-security-forbiddenclassexception
                    'https://repo1.maven.org/maven2/com/thoughtworks/xstream/xstream/1.4.19/xstream-1.4.19.jar',
                    os.path.join(lib_dir_path, 'xstream-1.4.15.jar')],
                [
                    'https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.17.2/log4j-core-2.17.2.jar',
                    os.path.join(lib_dir_path, 'log4j-core-2.16.jar')]]
            self.assertEqual(target_tools_list, jar_tools)
        finally:
            os.listdir = saved_listdir

    def test_get_jar_fixes2(self):
        lib_dir_path = get_full_path(__file__, step_up=1)
        lib_dir_content = [
            "xstream-1.4.15.jar",   # lib with some vulnerability, must be replaced
            "log4j-core-2.16.jar",  # only old jmeter versions contains affected log4j components
            "some-other-lib-0.99.jar"]  # other jar, mustn't be touched
        listdir = lambda _: lib_dir_content
        self.obj.version = '1.0'

        saved_listdir = os.listdir
        os.listdir = listdir
        try:
            jar_tools = self.obj._get_jar_fixes(lib_dir_path)
            target_tools_list = []
            self.assertEqual(target_tools_list, jar_tools)
        finally:
            os.listdir = saved_listdir



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
        pm_link = JMeter.PLUGINS_MANAGER_LINK.format(version=JMeter.PLUGINS_MANAGER_VERSION)
        cr_link = JMeter.COMMAND_RUNNER_LINK.format(version=JMeter.COMMAND_RUNNER_VERSION)
        self.assertEqual(self.obj.plugins_manager_link, pm_link)
        self.assertEqual(self.obj.command_runner_link, cr_link)

    def test_plugins_manager_and_command_runner_configured_url(self):
        pm_test_url = "http://somewhere.else/plugins-manager/{version}/plugins-manager.jar"
        pm_test_version = "1.0"
        cr_test_url = "http://somewhere.else/command-runner/{version}/command-runner.jar"
        cr_test_version = "1.0"
        self.obj.__init__(config={
            "plugins-manager": {
                "download-link": pm_test_url,
                "version": pm_test_version
            },
            "command-runner": {
                "download-link": cr_test_url,
                "version": cr_test_version
            }
        })
        self.assertEqual(self.obj.plugins_manager_link, pm_test_url.format(version=pm_test_version))
        self.assertEqual(self.obj.command_runner_link, cr_test_url.format(version=cr_test_version))
