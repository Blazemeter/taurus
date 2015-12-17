"""
Module holds selenium stuff
"""
import json
import os
import shutil
import subprocess
import sys
import time
from abc import abstractmethod

import urwid
from pyvirtualdisplay import Display

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.console import WidgetProvider
from bzt.modules.jmeter import JTLReader
from bzt.six import string_types, text_type, etree
from bzt.utils import RequiredTool, shell_exec, shutdown_process, JavaVM, TclLibrary, dehumanize_time, \
    MirrorsManager, is_windows


class SeleniumExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Selenium executor
    :type virtual_display: Display
    """
    SELENIUM_DOWNLOAD_LINK = "http://selenium-release.storage.googleapis.com/{version}/" \
                             "selenium-server-standalone-{version}.0.jar"
    SELENIUM_VERSION = "2.46"

    JUNIT_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=junit/junit/{version}/junit-{version}.jar"
    JUNIT_VERSION = "4.12"
    JUNIT_MIRRORS_SOURCE = "http://search.maven.org/solrsearch/select?q=g%3A%22junit%22%20AND%20a%3A%22junit%22%20" \
                           "AND%20v%3A%22{version}%22&rows=20&wt=json".format(version=JUNIT_VERSION)

    HAMCREST_DOWNLOAD_LINK = "https://hamcrest.googlecode.com/files/hamcrest-core-1.3.jar"

    SUPPORTED_TYPES = [".py", ".jar", ".java"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.virtual_display = None
        self.start_time = None
        self.end_time = None
        self.runner = None
        self.widget = None
        self.reader = None
        self.kpi_file = None
        self.err_jtl = None
        self.runner_working_dir = None
        self.scenario = None

    def prepare(self):
        """
        1) Locate script or folder
        2) detect script type
        3) create runner instance, prepare runner
        """
        self.scenario = self.get_scenario()
        self._verify_script()
        self.kpi_file = self.engine.create_artifact("selenium_tests_report", ".csv")
        self.err_jtl = self.engine.create_artifact("selenium_tests_err", ".xml")
        script_type = self.detect_script_type(self.scenario.get(Scenario.SCRIPT))

        if script_type == ".py":
            self.runner = NoseTester
            runner_config = self.settings.get("selenium-tools").get("nose")
        elif script_type == ".jar" or script_type == ".java":
            self.runner = JunitTester
            runner_config = self.settings.get("selenium-tools").get("junit")
        else:
            raise ValueError("Unsupported script type: %s" % script_type)

        runner_config["script-type"] = script_type
        self.runner_working_dir = self.engine.create_artifact(runner_config.get("working-dir", "classes"), "")
        runner_config["working-dir"] = self.runner_working_dir
        runner_config.get("artifacts-dir", self.engine.artifacts_dir)
        runner_config.get("working-dir", self.runner_working_dir)
        runner_config.get("report-file", self.kpi_file)
        runner_config.get("err-file", self.err_jtl)
        runner_config.get("stdout", self.engine.create_artifact("junit", ".out"))
        runner_config.get("stderr", self.engine.create_artifact("junit", ".err"))

        self._cp_resource_files(self.runner_working_dir)

        self.runner = self.runner(runner_config, self.scenario, self.log)
        self.runner.prepare()
        self.reader = JTLReader(self.kpi_file, self.log, self.err_jtl)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        display_conf = self.settings.get("virtual-display")
        if display_conf:
            if is_windows():
                self.log.warning("Cannot have virtual display on Windows, ignoring")
            else:
                width = display_conf.get("width", 1024)
                height = display_conf.get("height", 768)
                self.virtual_display = Display(size=(width, height))

    def _verify_script(self):
        if not self.scenario.get("script"):
            if self.scenario.get("requests"):
                self.scenario["script"] = self.__tests_from_requests()
            else:
                raise RuntimeError("Nothing to test, no requests were provided in scenario")

    def _cp_resource_files(self, runner_working_dir):
        """
        :return:
        """
        script = self.scenario.get("script")

        if Scenario.SCRIPT in self.scenario:
            if os.path.isdir(script):
                shutil.copytree(script, runner_working_dir)
            else:
                os.makedirs(runner_working_dir)
                shutil.copy2(script, runner_working_dir)

    def detect_script_type(self, script_path):
        """
        checks if script is java or python
        if it's folder or single script
        :return:
        """
        if not isinstance(script_path, string_types) and not isinstance(script_path, text_type):
            raise RuntimeError("Nothing to test, no files were provided in scenario")
        test_files = []
        for dir_entry in os.walk(script_path):
            if dir_entry[2]:
                for test_file in dir_entry[2]:
                    if os.path.splitext(test_file)[1].lower() in SeleniumExecutor.SUPPORTED_TYPES:
                        test_files.append(test_file)

        if os.path.isdir(script_path):
            file_ext = os.path.splitext(test_files[0])[1].lower()
        else:
            file_ext = os.path.splitext(script_path)[1]

        if file_ext not in SeleniumExecutor.SUPPORTED_TYPES:
            raise RuntimeError("Supported tests types %s was not found" % SeleniumExecutor.SUPPORTED_TYPES)
        return file_ext

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        if self.virtual_display:
            msg = "Starting virtual display[%s]: %s"
            self.log.info(msg, self.virtual_display.size, self.virtual_display.new_display_var)
            self.virtual_display.start()
        self.runner.run_tests()

    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            self.widget.update()

        if self.virtual_display and not self.virtual_display.is_alive():
            self.log.info("Virtual display out: %s", self.virtual_display.stdout)
            self.log.warning("Virtual display err: %s", self.virtual_display.stderr)
            raise RuntimeError("Virtual display failed: %s" % self.virtual_display.return_code)

        return self.runner.is_finished()

    def shutdown(self):
        """
        shutdown test_runner
        :return:
        """
        try:
            self.runner.shutdown()
        finally:
            if self.virtual_display and self.virtual_display.is_alive():
                self.virtual_display.stop()

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium tests ran for %s seconds", self.end_time - self.start_time)

    def post_process(self):
        if self.reader and not self.reader.buffer:
            raise RuntimeWarning("Empty results, most likely Selenium failed")

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.scenario.get("script"), self.runner.settings.get("stdout"))
        return self.widget

    def resource_files(self):
        if not self.scenario:
            self.scenario = self.get_scenario()

        if "script" not in self.scenario:
            return []

        script = self.scenario.get("script")
        script_type = self.detect_script_type(script)

        if script_type == ".py":
            runner_config = self.settings.get("selenium-tools").get("nose")
        elif script_type == ".jar" or script_type == ".java":
            runner_config = self.settings.get("selenium-tools").get("junit")
        else:
            raise ValueError("Unsupported script type: %s" % script_type)

        if self.runner_working_dir is None:
            self.runner_working_dir = self.engine.create_artifact(runner_config.get("working-dir", "classes"), "")

        self._cp_resource_files(self.runner_working_dir)

        return [os.path.basename(self.runner_working_dir)]

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        nose_test = SeleniumScriptBuilder(self.scenario, self.log)
        nose_test.scenario = self.scenario
        nose_test.gen_test_case()
        nose_test.save(filename)
        return filename


class AbstractTestRunner(object):
    """
    Abstract test runner
    """

    def __init__(self, settings, scenario):
        self.process = None
        self.settings = settings
        self.required_tools = []
        self.scenario = scenario
        self.artifacts_dir = self.settings.get("artifacts-dir")
        self.working_dir = self.settings.get("working-dir")
        self.log = None
        self.opened_descriptors = []
        self.is_failed = False

    @abstractmethod
    def prepare(self):
        pass

    @abstractmethod
    def run_checklist(self):
        pass

    @abstractmethod
    def run_tests(self):
        pass

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                self.log.debug("Test runner exit code: %s", ret_code)
                with open(self.settings.get("stderr")) as fds:
                    std_err = fds.read()
                self.is_failed = True
                raise RuntimeError("Test runner %s has failed: %s" % (self.__class__.__name__, std_err.strip()))
            return True
        return False

    def check_tools(self):
        for tool in self.required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s", tool.tool_name)
                tool.install()

    def shutdown(self):
        shutdown_process(self.process, self.log)
        for desc in self.opened_descriptors:
            desc.close()
        self.opened_descriptors = []


class JunitTester(AbstractTestRunner):
    """
    Allows to test java and jar files
    """

    def __init__(self, junit_config, scenario, parent_logger):
        super(JunitTester, self).__init__(junit_config, scenario)
        self.log = parent_logger.getChild(self.__class__.__name__)
        path_lambda = lambda key, val: os.path.abspath(os.path.expanduser(self.settings.get(key, val)))

        self.junit_path = path_lambda("path", "~/.bzt/selenium-taurus/tools/junit/junit.jar")
        self.hamcrest_path = path_lambda("hamcrest-core", "~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar")
        self.selenium_server_jar_path = path_lambda("selenium-server", "~/.bzt/selenium-taurus/selenium-server.jar")
        self.junit_listener_path = os.path.join(os.path.abspath(os.path.dirname(__file__)), os.pardir, "resources",
                                                "taurus-junit-1.0.jar")

        self.base_class_path = [self.selenium_server_jar_path, self.junit_path, self.junit_listener_path,
                                self.hamcrest_path]
        self.base_class_path.extend(self.scenario.get("additional-classpath", []))

    def prepare(self):
        """
        run checklist, make jar.
        """
        self.run_checklist()

        if self.settings.get("script-type", None) == ".java":
            self.compile_scripts()

    def run_checklist(self):
        """
        java
        javac
        selenium-server.jar
        junit.jar
        junit_listener.jar
        """
        if self.settings.get("script_type", None) == ".java":
            self.required_tools.append(JavaC("", "", self.log))

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(JavaVM("", "", self.log))
        self.required_tools.append(SeleniumServerJar(self.selenium_server_jar_path,
                                                     SeleniumExecutor.SELENIUM_DOWNLOAD_LINK.format(
                                                             version=SeleniumExecutor.SELENIUM_VERSION), self.log))
        self.required_tools.append(JUnitJar(self.junit_path, self.log, SeleniumExecutor.JUNIT_VERSION))
        self.required_tools.append(HamcrestJar(self.hamcrest_path, SeleniumExecutor.HAMCREST_DOWNLOAD_LINK))
        self.required_tools.append(JUnitListenerJar(self.junit_listener_path, ""))

        self.check_tools()

    def compile_scripts(self):
        """
        Compile .java files
        """
        self.log.debug("Compiling .java files started")
        java_files = []

        for dir_entry in os.walk(self.working_dir):
            if dir_entry[2]:
                for test_file in dir_entry[2]:
                    if os.path.splitext(test_file)[1].lower() == ".java":
                        java_files.append(os.path.join(dir_entry[0], test_file))

        compile_cl = ["javac", "-cp", os.pathsep.join(self.base_class_path)]
        compile_cl.extend(java_files)

        with open(os.path.join(self.artifacts_dir, "javac.out"), 'ab') as javac_out:
            with open(os.path.join(self.artifacts_dir, "javac.err"), 'ab') as javac_err:
                self.process = shell_exec(compile_cl, cwd=self.working_dir, stdout=javac_out, stderr=javac_err)
                ret_code = self.process.poll()

                while ret_code is None:
                    self.log.debug("Compiling .java files...")
                    time.sleep(1)
                    ret_code = self.process.poll()

        if ret_code != 0:
            self.log.debug("javac exit code: %s", ret_code)
            with open(javac_err.name) as err_file:
                out = err_file.read()
            raise RuntimeError("Javac exited with error:\n %s" % out.strip())

        self.log.info("Compiling .java files completed")

        self.make_jar()

    def make_jar(self):
        """
        move all .class files to compiled.jar
        """
        self.log.debug("Making .jar started")

        with open(os.path.join(self.artifacts_dir, "jar.out"), 'ab') as jar_out:
            with open(os.path.join(self.artifacts_dir, "jar.err"), 'ab') as jar_err:
                class_files = [java_file for java_file in os.listdir(self.working_dir) if java_file.endswith(".class")]
                jar_name = self.settings.get("jar-name", "compiled.jar")
                if class_files:
                    compile_jar_cl = ["jar", "-cf", jar_name]
                    compile_jar_cl.extend(class_files)
                else:
                    package_dir = os.listdir(self.working_dir)[0]
                    compile_jar_cl = ["jar", "-cf", jar_name, "-C", package_dir, "."]

                self.process = shell_exec(compile_jar_cl, cwd=self.working_dir, stdout=jar_out, stderr=jar_err)
                ret_code = self.process.poll()

                while ret_code is None:
                    self.log.debug("Making jar file...")
                    time.sleep(1)
                    ret_code = self.process.poll()

        if ret_code != 0:
            with open(jar_err.name) as err_file:
                out = err_file.read()
            self.log.info("Making jar failed with code %s", ret_code)
            self.log.info("jar output: %s", out)
            raise RuntimeError("Jar exited with non-zero code")

        self.log.info("Making .jar file completed")

    def run_tests(self):
        # java -cp junit.jar:selenium-test-small.jar:
        # selenium-2.46.0/selenium-java-2.46.0.jar:./../selenium-server.jar
        # org.junit.runner.JUnitCore TestBlazemeterPass

        jar_list = [os.path.join(self.working_dir, jar) for jar in os.listdir(self.working_dir) if jar.endswith(".jar")]
        self.base_class_path.extend(jar_list)

        junit_command_line = ["java", "-cp", os.pathsep.join(self.base_class_path), "taurusjunit.CustomRunner"]

        junit_command_line.extend([self.settings.get("report-file")])
        junit_command_line.extend([self.settings.get("err-file")])
        junit_command_line.extend(jar_list)

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.process = shell_exec(junit_command_line, cwd=self.artifacts_dir,
                                  stdout=std_out,
                                  stderr=std_err)


class NoseTester(AbstractTestRunner):
    """
    Python selenium tests runner
    """

    def __init__(self, nose_config, scenario, parent_logger):
        super(NoseTester, self).__init__(nose_config, scenario)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.plugin_path = os.path.join(os.path.dirname(__file__), os.pardir, "resources", "nose_plugin.py")

    def prepare(self):
        self.run_checklist()

    def run_checklist(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warn("You are using python3, make sure that your scripts are able to run in python3!")

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(TaurusNosePlugin(self.plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, self.settings.get("report-file"),
                             self.settings.get("err-file"), self.working_dir]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.process = shell_exec(nose_command_line, cwd=self.artifacts_dir,
                                  stdout=std_out,
                                  stderr=std_err)


class SeleniumWidget(urwid.Pile):
    def __init__(self, script, runner_output):
        widgets = []
        self.script_name = urwid.Text("Tests: %s" % script)
        self.summary_stats = urwid.Text("")
        self.current_test = urwid.Text("")
        self.runner_output = runner_output
        widgets.append(self.script_name)
        widgets.append(self.summary_stats)
        widgets.append(self.current_test)
        super(SeleniumWidget, self).__init__(widgets)

    def update(self):
        cur_test, reader_summary = ["No data received yet"] * 2
        if os.path.exists(self.runner_output):
            with open(self.runner_output, "rt") as fds:
                lines = fds.readlines()
                if lines:
                    line = lines[-1]
                    if line and "," in line:
                        cur_test, reader_summary = line.split(",")

        self.current_test.set_text(cur_test)
        self.summary_stats.set_text(reader_summary)
        self._invalidate()


class SeleniumServerJar(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(SeleniumServerJar, self).__init__("Selenium server", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug("%s path: %s", self.tool_name, self.tool_path)
        selenium_launch_command = ["java", "-jar", self.tool_path, "-help"]
        selenium_subproc = shell_exec(selenium_launch_command, stderr=subprocess.STDOUT)
        output = selenium_subproc.communicate()
        self.log.debug("%s output: %s", self.tool_name, output)
        if selenium_subproc.returncode == 0:
            self.already_installed = True
            return True
        else:
            return False


class JUnitJar(RequiredTool):
    def __init__(self, tool_path, parent_logger, junit_version):
        super(JUnitJar, self).__init__("JUnit", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = junit_version
        self.mirror_manager = JUnitMirrorsManager(self.log, self.version)

    def install(self):
        dest = os.path.dirname(os.path.expanduser(self.tool_path))
        dest = os.path.abspath(dest)
        junit_dist = super(JUnitJar, self).install_with_mirrors(dest, ".jar")
        self.log.info("Installing %s into %s", self.tool_name, dest)
        junit_dist.close()
        os.makedirs(dest)
        shutil.move(junit_dist.name, self.tool_path)
        self.log.info("Installed JUnit successfully")

        if not self.check_if_installed():
            raise RuntimeError("Unable to run %s after installation!" % self.tool_name)


class HamcrestJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(HamcrestJar, self).__init__("HamcrestJar", tool_path, download_link)


class JavaC(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(JavaC, self).__init__("JavaC", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = subprocess.check_output(["javac", '-version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except BaseException:
            raise RuntimeError("The %s is not operable or not available. Consider installing it" % self.tool_name)

    def install(self):
        raise NotImplementedError()


class JUnitListenerJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JUnitListenerJar, self).__init__("JUnitListener", tool_path, download_link)

    def install(self):
        raise NotImplementedError()


class TaurusNosePlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNosePlugin, self).__init__("TaurusNosePlugin", tool_path, download_link)

    def install(self):
        raise NotImplementedError()


class NoseTest(object):
    IMPORTS = """import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
"""

    def __init__(self):
        self.root = etree.Element("NoseTest")
        self.tree = etree.ElementTree(self.root)

    def add_imports(self):
        imports = etree.Element("imports")
        imports.text = NoseTest.IMPORTS
        return imports

    def gen_class_definition(self, class_name, inherits_from, indent="0"):
        def_tmpl = "class {class_name}({inherits_from}):"
        class_def_element = etree.Element("class_definition", indent=indent)
        class_def_element.text = def_tmpl.format(class_name=class_name, inherits_from="".join(inherits_from))
        return class_def_element

    def gen_method_definition(self, method_name, params, indent="4"):
        def_tmpl = "def {method_name}({params}):"
        method_def_element = etree.Element("method_definition", indent=indent)
        method_def_element.text = def_tmpl.format(method_name=method_name, params=",".join(params))
        return method_def_element

    def gen_method_statement(self, statement, indent="8"):
        statement_elem = etree.Element("statement", indent=indent)
        statement_elem.text = statement
        return statement_elem


class SeleniumScriptBuilder(NoseTest):
    def __init__(self, scenario, parent_logger):
        super(SeleniumScriptBuilder, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.scenario = scenario

    def gen_test_case(self):
        self.log.debug("Generating Test Case test method")
        imports = self.add_imports()
        self.root.append(imports)
        test_class = self.gen_class_definition("TestRequests", ["unittest.TestCase"])
        self.root.append(test_class)
        test_class.append(self.gen_setup_method())
        requests = self.scenario.get_requests()
        test_method = self.gen_test_method()
        test_class.append(test_method)
        scenario_timeout = self.scenario.get("timeout", 30)

        for req in requests:

            test_method.append(self.gen_comment("start request: %s" % req.url))

            if req.timeout is not None:
                test_method.append(self.gen_impl_wait(req.timeout))

            test_method.append(self.gen_method_statement("self.driver.get('%s')" % req.url))
            think_time = req.think_time if req.think_time else self.scenario.get("think-time", None)

            if think_time is not None:
                test_method.append(self.gen_method_statement("sleep(%s)" % dehumanize_time(think_time)))

            if "assert" in req.config:
                test_method.append(self.__gen_assert_page())
                for assert_config in req.config.get("assert"):
                    test_method.extend(self.gen_assertion(assert_config))

            if req.timeout is not None:
                test_method.append(self.gen_impl_wait(scenario_timeout))

            test_method.append(self.gen_comment("end request: %s" % req.url))
            test_method.append(self.__gen_new_line())
        test_class.append(self.gen_teardown_method())

    def gen_setup_method(self):
        self.log.debug("Generating setUp test method")
        browsers = ["Firefox", "Chrome", "Ie", "Opera"]
        browser = self.scenario.get("browser", "Firefox")
        if browser not in browsers:
            raise ValueError("Unsupported browser name: %s" % browser)
        setup_method_def = self.gen_method_definition("setUp", ["self"])
        setup_method_def.append(self.gen_method_statement("self.driver=webdriver.%s()" % browser))
        scenario_timeout = self.scenario.get("timeout", 30)
        setup_method_def.append(self.gen_impl_wait(scenario_timeout))
        setup_method_def.append(self.__gen_new_line())
        return setup_method_def

    def gen_impl_wait(self, timeout):
        return self.gen_method_statement("self.driver.implicitly_wait(%s)" % dehumanize_time(timeout))

    def gen_comment(self, comment):
        return self.gen_method_statement("# %s" % comment)

    def gen_test_method(self):
        self.log.debug("Generating test method")
        test_method = self.gen_method_definition("test_method", ["self"])
        return test_method

    def gen_teardown_method(self):
        self.log.debug("Generating tearDown test method")
        tear_down_method_def = self.gen_method_definition("tearDown", ["self"])
        tear_down_method_def.append(self.gen_method_statement("self.driver.quit()"))
        return tear_down_method_def

    def gen_assertion(self, assertion_config):
        self.log.debug("Generating assertion, config: %s", assertion_config)
        assertion_elements = []

        if isinstance(assertion_config, string_types):
            assertion_config = {"contains": [assertion_config]}

        for val in assertion_config["contains"]:
            regexp = assertion_config.get("regexp", True)
            reverse = assertion_config.get("not", False)
            subject = assertion_config.get("subject", "body")
            if subject != "body":
                raise ValueError("Only 'body' subject supported ")

            if regexp:
                assert_method = "self.assertEqual" if reverse else "self.assertNotEqual"
                assertion_elements.append(self.gen_method_statement('re_pattern = re.compile("%s")' % val))
                assertion_elements.append(
                        self.gen_method_statement('%s(0, len(re.findall(re_pattern, body)))' % assert_method))
            else:
                assert_method = "self.assertNotIn" if reverse else "self.assertIn"
                assertion_elements.append(self.gen_method_statement('%s("%s", body)' % (assert_method, val)))
        return assertion_elements

    def __gen_new_line(self, indent="8"):
        return self.gen_method_statement("", indent=indent)

    def __gen_assert_page(self):
        return self.gen_method_statement("body = self.driver.page_source")

    def save(self, filename):
        with open(filename, 'wt') as fds:
            for child in self.root.iter():
                if child.text is not None:
                    indent = int(child.get('indent', "0"))
                    fds.write(" " * indent + child.text + "\n")


class JUnitMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, junit_version):
        self.junit_version = junit_version
        super(JUnitMirrorsManager, self).__init__(SeleniumExecutor.JUNIT_MIRRORS_SOURCE, parent_logger)

    def _parse_mirrors(self):
        links = []
        if self.page_source is not None:
            self.log.debug('Parsing mirrors...')
            try:
                resp = json.loads(self.page_source)
                objects = resp.get("response", {}).get("docs", [])
                if objects:
                    obj = objects[0]
                    group = obj.get("g")
                    artifact = obj.get("a")
                    version = obj.get("v")
                    ext = obj.get("p")
                    link_template = "http://search.maven.org/remotecontent?filepath={group}/{artifact}/" \
                                    "{version}/{artifact}-{version}.{ext}"
                    link = link_template.format(group=group, artifact=artifact, version=version, ext=ext)
                    links.append(link)
            except BaseException as exc:
                self.log.error("Error while parsing mirrors %s", exc)
        default_link = SeleniumExecutor.JUNIT_DOWNLOAD_LINK.format(version=self.junit_version)
        if default_link not in links:
            links.append(default_link)
        self.log.debug('Total mirrors: %d', len(links))
        return links
