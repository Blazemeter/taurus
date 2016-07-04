"""
Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import json
import os
import shutil
import subprocess
import sys
import time
from abc import abstractmethod

import urwid

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.jmeter import JTLReader
from bzt.six import string_types, text_type, etree, parse
from bzt.utils import RequiredTool, shell_exec, shutdown_process, JavaVM, TclLibrary, get_files_recursive
from bzt.utils import dehumanize_time, MirrorsManager, is_windows, BetterDict, get_full_path

try:
    from pyvirtualdisplay.smartdisplay import SmartDisplay as Display
except ImportError:
    from pyvirtualdisplay import Display


class SeleniumExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Selenium executor
    :type virtual_display: Display
    :type runner: AbstractTestRunner
    """
    SELENIUM_DOWNLOAD_LINK = "http://selenium-release.storage.googleapis.com/{version}/" \
                             "selenium-server-standalone-{version}.0.jar"
    SELENIUM_VERSION = "2.53"

    JUNIT_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=junit/junit/" \
                          "{version}/junit-{version}.jar"
    JUNIT_VERSION = "4.12"
    JUNIT_MIRRORS_SOURCE = "http://search.maven.org/solrsearch/select?q=g%3A%22junit%22%20AND%20a%3A%22" \
                           "junit%22%20AND%20v%3A%22{version}%22&rows=20&wt=json".format(version=JUNIT_VERSION)

    HAMCREST_DOWNLOAD_LINK = "https://hamcrest.googlecode.com/files/hamcrest-core-1.3.jar"

    SUPPORTED_TYPES = [".py", ".jar", ".java"]

    SHARED_VIRTUAL_DISPLAY = {}

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.additional_env = {}
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
        self.script = None
        self.self_generated_script = False

    def set_virtual_display(self):
        display_conf = self.settings.get("virtual-display")
        if display_conf:
            if is_windows():
                self.log.warning("Cannot have virtual display on Windows, ignoring")
            else:
                if self.engine in SeleniumExecutor.SHARED_VIRTUAL_DISPLAY:
                    self.virtual_display = SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine]
                else:
                    width = display_conf.get("width", 1024)
                    height = display_conf.get("height", 768)
                    self.virtual_display = Display(size=(width, height))
                    msg = "Starting virtual display[%s]: %s"
                    self.log.info(msg, self.virtual_display.size, self.virtual_display.new_display_var)
                    self.virtual_display.start()
                    SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine] = self.virtual_display

    def free_virtual_display(self):
        if self.virtual_display and self.virtual_display.is_alive():
            self.virtual_display.stop()
        if self.engine in SeleniumExecutor.SHARED_VIRTUAL_DISPLAY:
            del SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine]

    def get_script_path(self, scenario=None):
        if scenario:
            return super(SeleniumExecutor, self).get_script_path(scenario)
        else:
            return self.engine.find_file(self.script)

    def _create_runner(self, working_dir, kpi_file, err_file):
        script_path = self.get_script_path()
        script_type = self.detect_script_type(script_path)
        runner_config = BetterDict()

        if script_type == ".py":
            runner_class = NoseTester
            runner_config.merge(self.settings.get("selenium-tools").get("nose"))
        else:  # script_type == ".jar" or script_type == ".java":
            runner_class = JUnitTester
            runner_config.merge(self.settings.get("selenium-tools").get("junit"))
            runner_config['props-file'] = self.engine.create_artifact("customrunner", ".properties")

        runner_config["script-type"] = script_type
        runner_config["working-dir"] = working_dir
        runner_config.get("artifacts-dir", self.engine.artifacts_dir)
        runner_config.get("report-file", kpi_file)
        runner_config.get("err-file", err_file)
        runner_config.get("stdout", self.engine.create_artifact("junit", ".out"))
        runner_config.get("stderr", self.engine.create_artifact("junit", ".err"))
        return runner_class(runner_config, self)

    def _create_reader(self, kpi_file, err_file):
        return JTLReader(kpi_file, self.log, err_file)

    def prepare(self):
        self.set_virtual_display()
        self.scenario = self.get_scenario()
        self._verify_script()

        self.runner_working_dir = self.engine.create_artifact("classes", "")
        self.kpi_file = self.engine.create_artifact("selenium_tests_report", ".csv")
        self.err_file = self.engine.create_artifact("selenium_tests_err", ".xml")
        self.runner = self._create_runner(self.runner_working_dir, self.kpi_file, self.err_file)

        self._cp_resource_files(self.runner_working_dir)

        self.runner.prepare()
        self.reader = self._create_reader(self.kpi_file, self.err_file)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def _verify_script(self):
        if Scenario.SCRIPT in self.scenario:
            self.script = self.scenario.get(Scenario.SCRIPT)
        elif "requests" in self.scenario:
            self.script = self.__tests_from_requests()
            self.self_generated_script = True
        else:
            raise ValueError("Nothing to test, no requests were provided in scenario")

    def _cp_resource_files(self, runner_working_dir):
        script = self.get_script_path()

        if os.path.isdir(script):
            shutil.copytree(script, runner_working_dir)
        else:
            os.makedirs(runner_working_dir)
            if self.self_generated_script:
                shutil.move(script, runner_working_dir)
            else:
                script_type = self.detect_script_type(script)
                script_name = os.path.basename(script)
                if script_type == ".py" and not script_name.lower().startswith('test'):
                    target_name = 'test_' + script_name
                    msg = "Script '%s' won't be discovered by nosetests, renaming script to %s"
                    self.log.warning(msg, script_name, target_name)
                else:
                    target_name = script_name
                target_path = os.path.join(runner_working_dir, target_name)
                shutil.copy2(script, target_path)

    @staticmethod
    def detect_script_type(script_path):
        if not isinstance(script_path, string_types) and not isinstance(script_path, text_type):
            raise ValueError("Nothing to test, no files were provided in scenario")

        if not os.path.exists(script_path):
            raise ValueError("Script %s doesn't exist" % script_path)

        file_types = set()

        if os.path.isfile(script_path):  # regular file received
            file_types.add(os.path.splitext(script_path)[1].lower())
        else:  # dir received: check contained files
            for file_name in get_files_recursive(script_path):
                file_types.add(os.path.splitext(file_name)[1].lower())

        if '.java' in file_types:
            file_ext = '.java'
        elif '.py' in file_types:
            file_ext = '.py'
        elif '.jar' in file_types:
            file_ext = '.jar'
        else:
            raise ValueError("Unsupported script type: %s" % script_path)

        return file_ext

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        self.runner.env = self.additional_env
        self.runner.run_tests()

    def check_virtual_display(self):
        if self.virtual_display:
            if not self.virtual_display.is_alive():
                self.log.info("Virtual display out: %s", self.virtual_display.stdout)
                self.log.warning("Virtual display err: %s", self.virtual_display.stderr)
                raise RuntimeError("Virtual display failed: %s" % self.virtual_display.return_code)


    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            self.widget.update()

        self.check_virtual_display()

        return self.runner.is_finished()

    def report_test_duration(self):
        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium tests ran for %s seconds", self.end_time - self.start_time)

    def shutdown(self):
        """
        shutdown test_runner
        :return:
        """
        self.runner.shutdown()
        self.report_test_duration()

    def post_process(self):
        self.free_virtual_display()

        if self.reader and not self.reader.read_records:
            raise RuntimeWarning("Empty results, most likely Selenium failed")

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.runner.settings.get("stdout"))
        return self.widget

    def resource_files(self):
        self.scenario = self.get_scenario()
        self._verify_script()
        script_path = self.get_script_path()
        resources = []
        if script_path is not None:
            resources.append(script_path)
        return resources

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        nose_test = SeleniumScriptBuilder(self.scenario, self.log)
        if self.virtual_display:
            nose_test.window_size = self.virtual_display.size
        nose_test.gen_test_case()
        nose_test.save(filename)
        return filename


class AbstractTestRunner(object):
    """
    Abstract test runner
    """

    def __init__(self, settings, executor):
        self.process = None
        self.settings = settings
        self.required_tools = []
        self.executor = executor
        self.scenario = executor.scenario
        self.load = executor.get_load()
        self.artifacts_dir = self.settings.get("artifacts-dir")
        self.working_dir = self.settings.get("working-dir")
        self.log = executor.log.getChild(self.__class__.__name__)
        self.opened_descriptors = []
        self.is_failed = False
        self.env = {}

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


class JUnitTester(AbstractTestRunner):
    """
    Allows to test java and jar files
    """

    def __init__(self, junit_config, executor):
        """
        :type junit_config: BetterDict
        """
        super(JUnitTester, self).__init__(junit_config, executor)
        self.props_file = junit_config['props-file']
        path_lambda = lambda key, val: get_full_path(self.settings.get(key, val))

        self.junit_path = path_lambda("path", "~/.bzt/selenium-taurus/tools/junit/junit.jar")
        self.hamcrest_path = path_lambda("hamcrest-core", "~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar")
        self.selenium_server_jar_path = path_lambda("selenium-server",
                                                    "~/.bzt/selenium-taurus/selenium-server.jar")
        self.junit_listener_path = os.path.join(get_full_path(__file__, step_up=1),
                                                os.pardir,
                                                "resources",
                                                "taurus-junit-1.0.jar")
        self.target_java = str(junit_config.get("compile-target-java", "1.7"))

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
        # only check javac if we need to compile. if we have JAR as script - we don't need javac
        if self.settings.get("script-type", None) == ".java":
            self.required_tools.append(JavaC("", "", self.log))

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(JavaVM("", "", self.log))
        link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK.format(version=SeleniumExecutor.SELENIUM_VERSION)
        self.required_tools.append(SeleniumServerJar(self.selenium_server_jar_path, link, self.log))
        self.required_tools.append(JUnitJar(self.junit_path, self.log, SeleniumExecutor.JUNIT_VERSION))
        self.required_tools.append(HamcrestJar(self.hamcrest_path, SeleniumExecutor.HAMCREST_DOWNLOAD_LINK))
        self.required_tools.append(JUnitListenerJar(self.junit_listener_path, ""))

        self.check_tools()

    def compile_scripts(self):
        """
        Compile .java files
        """
        self.log.debug("Compiling .java files started")

        jar_path = os.path.join(self.executor.engine.artifacts_dir,
                                self.working_dir,
                                self.settings.get("jar-name", "compiled.jar"))
        if os.path.exists(jar_path):
            self.log.debug(".java files are already compiled, skipping")
            return

        java_files = []

        for dir_entry in os.walk(self.working_dir):
            if dir_entry[2]:
                for test_file in dir_entry[2]:
                    if os.path.splitext(test_file)[1].lower() == ".java":
                        java_files.append(os.path.join(dir_entry[0], test_file))

        compile_cl = ["javac", "-source", self.target_java, "-target", self.target_java, ]
        compile_cl.extend(["-cp", os.pathsep.join(self.base_class_path)])
        compile_cl.extend(java_files)

        with open(os.path.join(self.artifacts_dir, "javac.out"), 'ab') as javac_out:
            with open(os.path.join(self.artifacts_dir, "javac.err"), 'ab') as javac_err:
                self.log.debug("running javac: %s", compile_cl)
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

                self.log.debug("running jar: %s", compile_jar_cl)
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

        with open(self.props_file, 'wt') as props:
            props.write("kpi_log=%s\n" % self.settings.get("report-file").replace(os.path.sep, '/'))
            props.write("error_log=%s\n" % self.settings.get("err-file").replace(os.path.sep, '/'))

            if self.load.iterations:
                props.write("iterations=%s\n" % self.load.iterations)

            if self.load.hold:
                props.write("hold_for=%s\n" % self.load.hold)

            for index, item in enumerate(jar_list):
                props.write("target_%s=%s\n" % (index, item.replace(os.path.sep, '/')))

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        env = BetterDict()
        env.merge(dict(os.environ))
        env.merge(self.env)

        junit_command_line = ["java", "-cp", os.pathsep.join(self.base_class_path), "taurusjunit.CustomRunner",
                              self.props_file]
        self.process = self.executor.execute(junit_command_line,
                                             cwd=self.artifacts_dir,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=env)


class NoseTester(AbstractTestRunner):
    """
    Python selenium tests runner
    """

    def __init__(self, nose_config, executor):
        super(NoseTester, self).__init__(nose_config, executor)
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=1),
                                        os.pardir,
                                        "resources",
                                        "nose_plugin.py")

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
        nose_command_line = [executable, self.plugin_path, '-k', self.settings.get("report-file"),
                             '-e', self.settings.get("err-file")]

        if self.load.iterations:
            nose_command_line += ['-i', str(self.load.iterations)]

        if self.load.hold:
            nose_command_line += ['-d', str(self.load.hold)]

        nose_command_line += [self.working_dir]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        env = BetterDict()
        env.merge(dict(os.environ))
        env.merge(self.env)

        self.process = self.executor.execute(nose_command_line,
                                             cwd=self.artifacts_dir,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=env)


class SeleniumWidget(urwid.Pile, PrioritizedWidget):
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
        PrioritizedWidget.__init__(self, priority=10)

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
        dest = get_full_path(self.tool_path, step_up=1)
        dest = os.path.abspath(dest)
        junit_dist = super(JUnitJar, self).install_with_mirrors(dest, ".jar")
        self.log.info("Installing %s into %s", self.tool_name, dest)
        junit_dist.close()
        if not os.path.exists(dest):
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
        self.window_size = None
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
        default_address = self.scenario.get("default-address", None)

        for req in requests:
            parsed_url = parse.urlparse(req.url)
            if default_address is not None and not parsed_url.netloc:
                url = default_address + req.url
            else:
                url = req.url

            test_method.append(self.gen_comment("start request: %s" % url))

            if req.timeout is not None:
                test_method.append(self.gen_impl_wait(req.timeout))

            test_method.append(self.gen_method_statement("self.driver.get('%s')" % url))
            think_time = req.think_time if req.think_time else self.scenario.get("think-time", None)

            if think_time is not None:
                test_method.append(self.gen_method_statement("sleep(%s)" % dehumanize_time(think_time)))

            if "assert" in req.config:
                test_method.append(self.__gen_assert_page())
                for assert_config in req.config.get("assert"):
                    test_method.extend(self.gen_assertion(assert_config))

            if req.timeout is not None:
                test_method.append(self.gen_impl_wait(scenario_timeout))

            test_method.append(self.gen_comment("end request: %s" % url))
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
        if self.window_size:
            setup_method_def.append(self.gen_method_statement("self.driver.set_window_size(%s, %s)" % self.window_size))
        else:
            setup_method_def.append(self.gen_method_statement("self.driver.maximize_window()"))
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
                method = '%s(0, len(re.findall(re_pattern, body)))' % assert_method
                assertion_elements.append(self.gen_method_statement(method))
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
