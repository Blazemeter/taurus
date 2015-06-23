"""
Module holds selenium stuff
"""

import os
import time
import logging
import shutil
import sys
import platform
import inspect
import psutil
import signal
import subprocess

from bzt.engine import ScenarioExecutor, Scenario
from six.moves.urllib.request import FancyURLopener
from bzt.utils import download_progress_hook, shell_exec
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader

CP_SEP = ";" if platform.system() == 'Windows' else ":"

def __dir__():
    filename = inspect.getouterframes(inspect.currentframe())[1][1]
    return os.path.dirname(filename)

class SeleniumExecutor(ScenarioExecutor):
    """
    Selenium executor
    """
    SELENIUM_DOWNLOAD_LINK = "http://selenium-release.storage.googleapis.com/{version}/" \
                             "selenium-server-standalone-{version}.0.jar"
    SELENIUM_VERSION = "2.46"

    JUNIT_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=junit/junit/{version}/junit-{version}.jar"
    JUNIT_VERSION = "4.12"

    JUNIT_LISTENER_LINK = "file://" + os.path.join(__dir__(), "taurus_junit.jar")

    SUPPORTED_TYPES = [".py", ".jar", ".java"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.selenium_log = None
        self.reader = None
        self.is_grid = None
        self.scenario = None
        self.start_time = None
        self.end_time = None
        self.runner = None
        self.kpi_file = ""

    def prepare(self):
        """
        1) Locate script or folder
        2) check type py/java
        3) check if all required tools are installed
        """
        self.selenium_log = self.engine.create_artifact("selenium", ".log")
        self.scenario = self.get_scenario()
        self.is_grid = self.scenario.get("use-grid", False)

        script_type, script_is_folder = self.detect_script_type(self.scenario.get("script"))

        if script_type == ".py":
            nose_config = self.settings.get("selenium-tools").get("nose")
            self.runner = NoseTester(nose_config, self.scenario)
        elif script_type == ".jar" or script_type == ".java":
            junit_config = self.settings.get("selenium-tools").get("junit")
            junit_config["script_type"] = script_type
            self.runner = JunitTester(junit_config, self.scenario)

        self.runner.run_checklist()

        tests_scripts_folder = os.path.join(self.engine.artifacts_dir, "selenium_scripts")  # should be set in config

        if Scenario.SCRIPT in self.scenario:
            if script_is_folder:
                shutil.copytree(self.scenario.get("script"), tests_scripts_folder)
            else:
                os.makedirs(tests_scripts_folder)
                shutil.copy2(self.scenario.get("script"), tests_scripts_folder)

        self.kpi_file = os.path.join(self.engine.artifacts_dir, "report.txt")
        self.reader = SeleniumDataReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def detect_script_type(self, script_path):
        """
        checks if script is java or python
        if it's folder or single script
        :return:
        """
        script_path_is_directory = False

        if os.path.isdir(script_path):
            file_ext = os.path.splitext(os.listdir(script_path)[0])[1].lower()
            script_path_is_directory = True
        else:
            file_ext = os.path.splitext(script_path)[1]

        if file_ext not in SeleniumExecutor.SUPPORTED_TYPES:
            raise RuntimeError("Unsupported script type: %s" % file_ext)
        return file_ext, script_path_is_directory

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        # TODO: implement selenium-server grid

        self.runner.run_tests(self.engine.artifacts_dir)

    def check(self):
        """
        check if test completed
        :return:
        """
        # if self.widget:
        #    self.widget.update()

        return self.runner.is_finished()

    def shutdown(self):
        """
        shutdown test_runner
        :return:
        """
        self.runner.shutdown()

        if self.start_time:
            self.end_time = time.time()
            self.log.info("Selenium tests run for %s seconds",
                          self.end_time - self.start_time)


class AbstractTestRunner(object):
    """
    Abstract test runner
    """

    def __init__(self, settings, scenario):
        self.process = None
        self.settings = settings
        self.log = logging.getLogger('')
        self.report_file = "report.txt"
        self.required_tools = []
        self.scenario = scenario

    def run_checklist(self):
        raise NotImplementedError

    def run_tests(self, artifacts_dir):
        raise NotImplementedError

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                self.log.info("test runner exit code: %s", ret_code)
                raise RuntimeError("test runner exited with non-zero code")
            return True
        return False

    def shutdown(self):
        while self.process and self.process.poll() is None:
            # TODO: find a way to have graceful shutdown, then kill
            self.log.info("Terminating runner PID: %s", self.process.pid)
            time.sleep(1)
            try:
                if platform.system() == 'Windows':
                    cur_pids = psutil.get_pid_list()
                    if self.process.pid in cur_pids:
                        jm_proc = psutil.Process(self.process.pid)
                        for child_proc in jm_proc.get_children(recursive=True):
                            self.log.debug("Terminating child process: %d", child_proc.pid)
                            child_proc.send_signal(signal.SIGTERM)
                        os.kill(self.process.pid, signal.SIGTERM)
                else:
                    os.killpg(self.process.pid, signal.SIGTERM)
            except OSError as exc:
                self.log.debug("Failed to terminate test runner: %s", exc)


class JunitTester(AbstractTestRunner):
    """
    Allows to test java and jar files
    """

    def __init__(self, junit_config, scenario):
        super(JunitTester, self).__init__(junit_config, scenario)
        self.junit_path = os.path.abspath(
            os.path.expanduser(self.settings.get("path", "~/selenium-taurus/tools/junit/junit.jar")))
        self.selenium_server_jar_path = os.path.abspath(
            os.path.expanduser(self.settings.get("selenium-server", "~/selenium-taurus/selenium-server.jar")))
        self.junit_listener_path = os.path.abspath(os.path.expanduser(
            self.settings.get("junit-listener", "~/selenium-taurus/tools/junit_listener/taurus_junit.jar")))
        self.path_to_scripts = self.scenario.get("script")
        self.base_class_path = [self.selenium_server_jar_path, self.junit_path, self.junit_listener_path]
        self.base_class_path.extend(self.scenario.get("custom_jars", []))

    def run_checklist(self):
        """
        java
        javac
        selenium-server.jar
        junit.jar
        junit_listener.jar
        """

        self.required_tools.append(JavaVM("", ""))
        self.required_tools.append(JavaC("", ""))
        self.required_tools.append(SeleniumServerJar(self.selenium_server_jar_path,
                                                     SeleniumExecutor.SELENIUM_DOWNLOAD_LINK.format(
                                                         version=SeleniumExecutor.SELENIUM_VERSION)))
        self.required_tools.append(JUnitJar(self.junit_path, SeleniumExecutor.JUNIT_DOWNLOAD_LINK.format(
            version=SeleniumExecutor.JUNIT_VERSION)))
        self.required_tools.append(
            JUnitListenerJar(self.junit_listener_path, SeleniumExecutor.JUNIT_LISTENER_LINK))

        for tool in self.required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s", tool.tool_name)
                tool.install()

    def compile_scripts(self, artifacts_dir):
        """
        Compile .java files, modify self.complete_class_path
        """
        self.log.debug("Compiling java files...")
        scripts_folder = os.path.join(artifacts_dir, "selenium_scripts")

        java_files = [java_file for java_file in os.listdir(scripts_folder) if java_file.endswith(".java")]
        compile_cl = ["javac", "-cp", CP_SEP.join(self.base_class_path)]
        compile_cl.extend(java_files)

        javac_out = open(os.path.join(artifacts_dir, "javac_out"), 'wb')
        javac_err = open(os.path.join(artifacts_dir, "javac_err"), 'wb')
        self.process = shell_exec(compile_cl, cwd=scripts_folder, stdout=javac_out, stderr=javac_err)

        ret_code = self.process.poll()

        while ret_code is None:
            self.log.debug("Compiling java files...")
            time.sleep(1)
            ret_code = self.process.poll()

        if ret_code != 0:
            self.log.info("Compile failed: %s", ret_code)
            raise RuntimeError("test runner exited with non-zero code")

        self.log.info("Compile completed: %s", ret_code)

        self.make_jar(artifacts_dir)

    def make_jar(self, artifacts_dir):
        """
        move all .class files to compiled.jar
        """
        self.log.debug("Compiling jars...")
        scripts_folder = os.path.join(artifacts_dir, "selenium_scripts")
        jar_out = open(os.path.join(artifacts_dir, "jar_out"), 'wb')
        jar_err = open(os.path.join(artifacts_dir, "jar_err"), 'wb')
        class_files = [java_file for java_file in os.listdir(scripts_folder) if java_file.endswith(".class")]
        compile_jar_cl = ["jar", "-cf", "compiled.jar"]
        compile_jar_cl.extend(class_files)
        self.process = shell_exec(compile_jar_cl, cwd=scripts_folder, stdout=jar_out, stderr=jar_err)

        ret_code = self.process.poll()

        while ret_code is None:
            self.log.debug("Compiling jar file...")
            time.sleep(1)
            ret_code = self.process.poll()

        if ret_code != 0:
            self.log.info("Compile failed: %s", ret_code)
            raise RuntimeError("test runner exited with non-zero code")

        self.log.info("Compile jar file completed: %s", ret_code)

    def run_tests(self, artifacts_dir):
        # java -cp junit.jar:selenium-test-small.jar:
        # selenium-2.46.0/selenium-java-2.46.0.jar:./../selenium-server.jar
        # org.junit.runner.JUnitCore TestBlazemeterPass
        if self.settings.get("script_type", None) == ".java":
            self.compile_scripts(artifacts_dir)
            self.path_to_scripts = "compiled.jar"

        # TODO: implement user input as list of .java/jar files, not only one and folder
        if not os.path.isdir(self.path_to_scripts):
            self.base_class_path.extend([self.path_to_scripts])
            jar_list = [os.path.join("selenium_scripts", os.path.basename(self.path_to_scripts))]
        else:
            jar_list = [os.path.join("selenium_scripts", jar) for jar in
                        os.listdir(os.path.join(artifacts_dir, "selenium_scripts")) if jar.endswith(".jar")]
            self.base_class_path.extend(jar_list)

        junit_command_line = ["java", "-cp", CP_SEP.join(self.base_class_path),
                              "taurus_junit_listener.CustomRunner"]
        junit_command_line.extend(jar_list)

        self.log.debug(" ".join(junit_command_line))

        # self.log.info(junit_command_line)
        junit_out_path = os.path.join(artifacts_dir, "junit_out")
        junit_err_path = os.path.join(artifacts_dir, "junit_err")

        junit_out = open(junit_out_path, 'wb')
        junit_err = open(junit_err_path, 'wb')

        self.process = shell_exec(junit_command_line, cwd=artifacts_dir,
                                  stdout=junit_out,
                                  stderr=junit_err)


class NoseTester(AbstractTestRunner):
    """
    Python selenium tests runner
    """

    def __init__(self, nose_config, scenario):
        super(NoseTester, self).__init__(nose_config, scenario)
        self.plugin_path = os.path.abspath(os.path.expanduser(
            self.settings.get("taurus-nose-plugin", "~/selenium-taurus/tools/nose_plugin/taurus_nose_plugin.tar.gz")))

    def run_checklist(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warn("You are using python3, make sure that your scripts are able to run in python3!")

        self.required_tools.append(
            TaurusNosePlugin(self.plugin_path, "file:///" + os.path.join(__dir__(), "taurus_nose_plugin.tar.gz")))

        for tool in self.required_tools:
            if not tool.check_if_installed():
                tool.install()

    def run_tests(self, artifacts_dir):
        """
        run python tests
        """

        env = os.environ.copy()
        executable = sys.executable
        nose_command_line = [executable, "-m", "nose", "--with-taurus_nose_plugin", "selenium_scripts"]
        self.log.info(nose_command_line)
        nose_out = open(os.path.join(artifacts_dir, "nose_out"), 'ab')
        nose_err = open(os.path.join(artifacts_dir, "nose_err"), 'ab')
        self.process = subprocess.Popen(nose_command_line, cwd=artifacts_dir,
                                        stdout=nose_out,
                                        stderr=nose_err, env=env)
        self.report_file = os.path.join(artifacts_dir, "report.txt")

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code == 0:
                self.log.info("python nose tests exit code: %s", ret_code)
            elif ret_code == 1:
                self.log.info("python nose tests exit code: %s, some tests failed", ret_code)
            return True
        return False


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
        self.err_codes = {"OK": "200", "SKIPPED": "300", "FAILED": "404", "ERROR": "500", "":"999"}  # FIXME: blank err code bug on manual stop

    def _read(self, last_pass=False):
        """
        :param last_pass:
        """

        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading selenium results")
        # self.fds.seek(self.offset)  # without this we have a stuck reads on Mac
        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read
        # self.offset = self.fds.tell()
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
                self.log.debug(line)
                self.log.debug(self.test_buffer.T_STAMP)
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
                self.test_buffer.TIME = line[7:]
                self.test_buffer.TRACE = self.trace_buff
                self.test_buffer.MESSAGE = self.err_message_buff

                r_code = self.err_codes[self.test_buffer.RESULT]
                concur = 1
                conn_time = 0
                latency = 0

                if self.test_buffer.TRACE or self.test_buffer.MESSAGE:
                    if not self.test_buffer.MESSAGE:
                        error = self.test_buffer.TRACE
                    else:
                        error = self.test_buffer.MESSAGE + "\n" + self.test_buffer.TRACE
                else:
                    error = None
                yield int(self.test_buffer.T_STAMP) / 1000.0, self.test_buffer.TEST_NAME, concur, \
                      int(self.test_buffer.TIME) / 1000.0, conn_time, latency, r_code, error, self.test_buffer.MODULE
            else:
                if not self.err_message_buff:
                    self.trace_buff += line
                else:
                    self.err_message_buff += line

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


class TestSample(object):
    def __init__(self):
        self.T_STAMP = ""
        self.MODULE = ""
        self.TEST_NAME = ""
        self.RESULT = ""
        self.TRACE = ""
        self.MESSAGE = ""
        self.TIME = ""


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
    def __init__(self, tool_path, download_link):
        super(SeleniumServerJar, self).__init__("Selenium server", tool_path, download_link)

    def check_if_installed(self):
        selenium_launch_command = ["java", "-jar", self.tool_path, "-help"]
        selenium_subproc = subprocess.Popen(selenium_launch_command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        selenium_subproc.communicate()
        if selenium_subproc.returncode == 0:
            self.already_installed = True
            return True
        else:
            return False


class JUnitJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JUnitJar, self).__init__("JUnit", tool_path, download_link)


class JavaVM(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JavaVM, self).__init__("JavaVM", tool_path, download_link)

    def check_if_installed(self):
        try:
            subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
            return True
        except BaseException:
            raise RuntimeError("The %s is not operable or not available. Consider installing it" % self.tool_name)

    def install(self):
        raise NotImplementedError


class JavaC(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JavaC, self).__init__("JavaC", tool_path, download_link)

    def check_if_installed(self):
        try:
            subprocess.check_output(["javac", '-version'], stderr=subprocess.STDOUT)
            return True
        except BaseException:
            raise RuntimeError("The %s is not operable or not available. Consider installing it" % self.tool_name)

    def install(self):
        raise NotImplementedError


class JUnitListenerJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JUnitListenerJar, self).__init__("JUnitListener", tool_path, download_link)


class TaurusNosePlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNosePlugin, self).__init__("TaurusNosePlugin", tool_path, download_link)

    def check_if_installed(self):
        env = os.environ.copy()
        executable = sys.executable
        nose_command_line = [executable, "-m", "nose", "--with-taurus_nose_plugin", "--collect-only"]
        subproc = subprocess.Popen(nose_command_line, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
        subproc.communicate()
        if subproc.returncode == 0:
            self.already_installed = True
            return True
        else:
            return False

    def install(self):

        if not os.path.exists(os.path.dirname(self.tool_path)):
            os.makedirs(os.path.dirname(self.tool_path))
            downloader = FancyURLopener()
            downloader.retrieve(self.download_link, self.tool_path, download_progress_hook)

        env = os.environ.copy()
        executable = sys.executable
        pip_command_line = [executable, "-m", "pip", "install", self.tool_path]
        pip_subproc = subprocess.Popen(pip_command_line, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
        pip_subproc.communicate()
        if self.check_if_installed():
            return self.tool_path
        else:
            raise RuntimeError("Unable to run %s after installation!" % self.tool_name)
