"""
Module holds selenium stuff
"""
from abc import abstractmethod

import os
import time
import shutil
import sys
import subprocess
import urwid

from collections import Counter
from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.moves import FancyURLopener, string_types, text_type
from bzt.utils import download_progress_hook, shell_exec, shutdown_process, BetterDict
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider


class SeleniumExecutor(ScenarioExecutor, WidgetProvider):
    """
    Selenium executor
    """
    SELENIUM_DOWNLOAD_LINK = "http://selenium-release.storage.googleapis.com/{version}/" \
                             "selenium-server-standalone-{version}.0.jar"
    SELENIUM_VERSION = "2.46"

    JUNIT_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=junit/junit/{version}/junit-{version}.jar"
    JUNIT_VERSION = "4.12"

    SUPPORTED_TYPES = [".py", ".jar", ".java"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.start_time = None
        self.end_time = None
        self.runner = None
        self.widget = None
        self.reader = None
        self.kpi_file = None

    def prepare(self):
        """
        1) Locate script or folder
        2) detect script type
        3) create runner instance, prepare runner
        """
        scenario = self.get_scenario()
        self.kpi_file = self.engine.create_artifact("selenium_tests_report", ".txt")
        script_type, script_is_folder = self.detect_script_type(scenario.get("script"))
        runner_config = BetterDict()

        if script_type == ".py":
            self.runner = NoseTester
            runner_config = self.settings.get("selenium-tools").get("nose")

        elif script_type == ".jar" or script_type == ".java":
            self.runner = JunitTester
            runner_config = self.settings.get("selenium-tools").get("junit")

        runner_config["script-type"] = script_type
        runner_working_dir = self.engine.create_artifact(runner_config.get("working-dir", "classes"), "")
        runner_config["working-dir"] = runner_working_dir
        runner_config.get("artifacts-dir", self.engine.artifacts_dir)
        runner_config.get("working-dir", runner_working_dir)
        runner_config.get("report-file", self.kpi_file)

        if Scenario.SCRIPT in scenario:
            if script_is_folder:
                shutil.copytree(scenario.get("script"), runner_working_dir)
            else:
                os.makedirs(runner_working_dir)
                shutil.copy2(scenario.get("script"), runner_working_dir)

        self.runner = self.runner(runner_config, scenario, self.log)
        self.runner.prepare()
        self.reader = SeleniumDataReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def detect_script_type(self, script_path):
        """
        checks if script is java or python
        if it's folder or single script
        :return:
        """
        if not isinstance(script_path, string_types) and not isinstance(script_path, text_type):
            raise RuntimeError("Nothing to test, no files were provided in scenario")
        script_path_is_directory = False
        test_files = []
        for dir_entry in os.walk(script_path):
            if dir_entry[2]:
                for test_file in dir_entry[2]:
                    if os.path.splitext(test_file)[1].lower() in SeleniumExecutor.SUPPORTED_TYPES:
                        test_files.append(test_file)

        if os.path.isdir(script_path):
            file_ext = os.path.splitext(test_files[0])[1].lower()
            script_path_is_directory = True
        else:
            file_ext = os.path.splitext(script_path)[1]

        if file_ext not in SeleniumExecutor.SUPPORTED_TYPES:
            raise RuntimeError("Supported tests types %s was not found" % SeleniumExecutor.SUPPORTED_TYPES)
        return file_ext, script_path_is_directory

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        self.runner.run_tests()

    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            cur_state = self.reader.get_state()
            self.widget.update(cur_state, self.reader.summary)

        return self.runner.is_finished()

    def shutdown(self):
        """
        shutdown test_runner
        :return:
        """
        self.runner.shutdown()

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium tests ran for %s seconds", self.end_time - self.start_time)

        if self.kpi_file:
            if (not os.path.exists(self.kpi_file) or not os.path.getsize(self.kpi_file)) and not self.runner.is_failed:
                msg = "Empty runner report, most likely runner failed: %s"
                raise RuntimeWarning(msg % self.kpi_file)

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.get_scenario().get("script"))
        return self.widget


class AbstractTestRunner(object):
    """
    Abstract test runner
    """

    def __init__(self, settings, scenario):
        self.process = None
        self.settings = settings
        self.required_tools = []
        self.scenario = scenario
        self.report_file = self.settings.get("report-file")
        self.artifacts_dir = self.settings.get("artifacts-dir")
        self.working_dir = self.settings.get("working-dir")
        self.log = None
        self.opened_descriptors = {"std_err": None, "std_out": None}
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
                with open(self.opened_descriptors["std_err"].name) as fds:
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
        for desc in self.opened_descriptors.values():
            desc.close()
        self.opened_descriptors = {}


class JunitTester(AbstractTestRunner):
    """
    Allows to test java and jar files
    """

    def __init__(self, junit_config, scenario, parent_logger):
        super(JunitTester, self).__init__(junit_config, scenario)
        self.log = parent_logger.getChild(self.__class__.__name__)
        path_lambda = lambda key, val: os.path.abspath(os.path.expanduser(self.settings.get(key, val)))

        self.junit_path = path_lambda("path", "~/.bzt/selenium-taurus/tools/junit/junit.jar")
        self.selenium_server_jar_path = path_lambda("selenium-server", "~/.bzt/selenium-taurus/selenium-server.jar")
        self.junit_listener_path = os.path.join(os.path.dirname(__file__), "resources", "taurus_junit.jar")

        self.base_class_path = [self.selenium_server_jar_path, self.junit_path, self.junit_listener_path]
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
        self.required_tools.append(JavaVM("", "", self.log))
        self.required_tools.append(SeleniumServerJar(self.selenium_server_jar_path,
                                                     SeleniumExecutor.SELENIUM_DOWNLOAD_LINK.format(
                                                         version=SeleniumExecutor.SELENIUM_VERSION), self.log))
        self.required_tools.append(JUnitJar(self.junit_path, SeleniumExecutor.JUNIT_DOWNLOAD_LINK.format(
            version=SeleniumExecutor.JUNIT_VERSION)))
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

        with open(os.path.join(self.artifacts_dir, "javac_out"), 'ab') as javac_out:
            with open(os.path.join(self.artifacts_dir, "javac_err"), 'ab') as javac_err:
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

        with open(os.path.join(self.artifacts_dir, "jar_out"), 'ab') as jar_out:
            with open(os.path.join(self.artifacts_dir, "jar_err"), 'ab') as jar_err:
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

        junit_command_line = ["java", "-cp", os.pathsep.join(self.base_class_path),
                              "taurus_junit_listener.CustomRunner"]
        junit_command_line.extend(jar_list)
        junit_command_line.extend([self.report_file])

        junit_out_path = os.path.join(self.artifacts_dir, "junit_out")
        junit_err_path = os.path.join(self.artifacts_dir, "junit_err")

        junit_out = open(junit_out_path, 'ab')
        junit_err = open(junit_err_path, 'ab')

        self.opened_descriptors["std_out"] = junit_out
        self.opened_descriptors["std_err"] = junit_err

        self.process = shell_exec(junit_command_line, cwd=self.artifacts_dir,
                                  stdout=junit_out,
                                  stderr=junit_err)


class NoseTester(AbstractTestRunner):
    """
    Python selenium tests runner
    """

    def __init__(self, nose_config, scenario, parent_logger):
        super(NoseTester, self).__init__(nose_config, scenario)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.plugin_path = os.path.join(os.path.dirname(__file__), "resources", "nose_plugin.py")

    def prepare(self):
        self.run_checklist()

    def run_checklist(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warn("You are using python3, make sure that your scripts are able to run in python3!")

        self.required_tools.append(
            TaurusNosePlugin(self.plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, self.report_file, self.working_dir]
        nose_out = open(os.path.join(self.artifacts_dir, "nose_out"), 'ab')
        nose_err = open(os.path.join(self.artifacts_dir, "nose_err"), 'ab')

        self.opened_descriptors["std_out"] = nose_out
        self.opened_descriptors["std_err"] = nose_err

        self.process = shell_exec(nose_command_line, cwd=self.artifacts_dir,
                                  stdout=nose_out,
                                  stderr=nose_err)


class SeleniumDataReader(ResultsReader):
    """
    Read KPI from data log
    """

    def __init__(self, filename, parent_logger):
        super(SeleniumDataReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.partial_buffer = ""
        self.test_buffer = TestSample()
        self.offset = 0
        self.trace_buff = ""
        self.err_message_buff = ""
        self.err_codes = {"OK": "200", "SKIPPED": "300", "FAILED": "404", "ERROR": "500"}
        self.summary = Counter({"total": 0, "pass": 0, "fail": 0})

    def _read(self, last_pass=False):
        """
        :param last_pass:
        """

        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet...")
            yield None

        self.log.debug("Reading selenium results")
        self.fds.seek(self.offset)  # without this we have a stuck reads on Mac
        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read
        self.offset = self.fds.tell()
        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""
            line = line.strip("\n")
            # TODO: Optimise it
            if line.startswith("--TIMESTAMP:"):
                self.test_buffer = TestSample()
                self.trace_buff = ""
                self.err_message_buff = ""
                self.test_buffer.T_STAMP = line[12:]
            elif line.startswith("--MODULE:"):
                self.test_buffer.MODULE = line[9:]
            elif line.startswith("--RUN:"):
                self.test_buffer.TEST_NAME = line[6:]
            elif line.startswith("--RESULT:"):
                self.test_buffer.RESULT = line[10:]
            elif line.startswith("--TRACE:"):
                self.trace_buff = line[8:]
            elif line.startswith("--MESSAGE:"):
                self.err_message_buff = line[9:]
            elif line.startswith("--TIME:"):
                self.summary['total'] += 1
                self.test_buffer.TIME = line[7:]
                self.test_buffer.TRACE = self.trace_buff
                self.test_buffer.MESSAGE = self.err_message_buff

                r_code = self.err_codes[self.test_buffer.RESULT]
                concur = 1
                conn_time = 0
                latency = 0

                if self.test_buffer.TRACE or self.test_buffer.MESSAGE:
                    self.summary["fail"] += 1
                    if not self.test_buffer.MESSAGE:
                        error = self.test_buffer.TRACE
                    else:
                        error = self.test_buffer.MESSAGE + "\n" + self.test_buffer.TRACE
                else:
                    self.summary["pass"] += 1
                    error = None
                yield int(self.test_buffer.T_STAMP) / 1000.0, self.test_buffer.TEST_NAME, concur, \
                      int(self.test_buffer.TIME) / 1000.0, conn_time, latency, r_code, error, self.test_buffer.MODULE
            else:
                if not self.err_message_buff:
                    self.trace_buff += line
                else:
                    self.err_message_buff += line

    def get_state(self):
        return self.test_buffer.TEST_NAME

    def __open_fds(self):
        """
        opens results.txt
        """
        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet")
            return False

        if not os.path.getsize(self.filename):
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()


class TestSample(object):
    def __init__(self):
        self.T_STAMP = ""
        self.MODULE = ""
        self.TEST_NAME = ""
        self.RESULT = ""
        self.TRACE = ""
        self.MESSAGE = ""
        self.TIME = ""


class SeleniumWidget(urwid.Pile):
    def __init__(self, script):
        widgets = []
        self.script_name = urwid.Text("Tests: %s" % script)
        self.summary_stats = urwid.Text("")
        self.current_test = urwid.Text("")
        widgets.append(self.script_name)
        widgets.append(self.summary_stats)
        widgets.append(self.current_test)
        super(SeleniumWidget, self).__init__(widgets)

    def update(self, cur_test, reader_summary):
        self.current_test.set_text(cur_test)
        self.summary_stats.set_text(
            "Total:%d Pass:%d Fail:%d" % (reader_summary['total'], reader_summary['pass'], reader_summary['fail']))
        self._invalidate()


class RequiredTool(object):
    """
    Abstract required tool
    """

    def __init__(self, tool_name, tool_path, download_link):
        self.tool_name = tool_name
        self.tool_path = tool_path
        self.download_link = download_link
        self.already_installed = False

    def check_if_installed(self):
        if os.path.exists(self.tool_path):
            self.already_installed = True
            return True
        return False

    def install(self):
        try:
            if not os.path.exists(os.path.dirname(self.tool_path)):
                os.makedirs(os.path.dirname(self.tool_path))
            downloader = FancyURLopener()
            downloader.retrieve(self.download_link, self.tool_path, download_progress_hook)

            if self.check_if_installed():
                return self.tool_path
            else:
                raise RuntimeError("Unable to run %s after installation!" % self.tool_name)
        except BaseException as exc:
            raise exc


class SeleniumServerJar(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(SeleniumServerJar, self).__init__("Selenium server", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug("%s path: %s", self.tool_name, self.tool_path)
        selenium_launch_command = ["java", "-jar", self.tool_path, "-help"]
        selenium_subproc = shell_exec(selenium_launch_command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        output = selenium_subproc.communicate()
        self.log.debug("%s output: %s", self.tool_name, output)
        if selenium_subproc.returncode == 0:
            self.already_installed = True
            return True
        else:
            return False


class JUnitJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JUnitJar, self).__init__("JUnit", tool_path, download_link)


class JavaVM(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(JavaVM, self).__init__("JavaVM", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except BaseException:
            raise RuntimeError("The %s is not operable or not available. Consider installing it" % self.tool_name)

    def install(self):
        raise NotImplementedError()


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
