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
import re
import shutil
import subprocess
import sys
import time
import traceback
from abc import abstractmethod
from subprocess import CalledProcessError

from urwid import Text, Pile

from bzt import TaurusConfigError, ToolError, TaurusInternalException
from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.functional import FunctionalResultsReader, FunctionalAggregator, FunctionalSample
from bzt.modules.services import HavingInstallableTools
from bzt.six import string_types, parse, iteritems
from bzt.utils import RequiredTool, shell_exec, shutdown_process, JavaVM, TclLibrary, get_files_recursive, \
    PythonGenerator
from bzt.utils import dehumanize_time, MirrorsManager, is_windows, BetterDict, get_full_path

try:
    from pyvirtualdisplay.smartdisplay import SmartDisplay as Display
except ImportError:
    from pyvirtualdisplay import Display


class AbstractSeleniumExecutor(ScenarioExecutor, HavingInstallableTools):
    """
    Abstract base class for Selenium executors.

    All executors must implement the following interface.
    """

    SHARED_VIRTUAL_DISPLAY = {}

    @abstractmethod
    def add_env(self, env):
        """
        Add environment variables into selenium process env
        :type env: dict[str,str]
        """
        pass

    @abstractmethod
    def get_virtual_display(self):
        """
        Return virtual display instance used by this executor.
        :rtype: Display
        """
        pass


class SeleniumExecutor(AbstractSeleniumExecutor, WidgetProvider, FileLister):
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

    TESTNG_VERSION = "6.8.5"
    TESTNG_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=org/testng/testng/" \
                           "{version}/testng-{version}.jar".format(version=TESTNG_VERSION)

    HAMCREST_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=org/hamcrest/hamcrest-core" \
                             "/1.3/hamcrest-core-1.3.jar"

    JSON_JAR_DOWNLOAD_LINK = "http://search.maven.org/remotecontent?filepath=org/json/json/20160810/json-20160810.jar"

    MOCHA_NPM_PACKAGE_NAME = "mocha"
    SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = "selenium-webdriver"

    SUPPORTED_RUNNERS = ["nose", "junit", "testng", "rspec", "mocha"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.additional_env = {}
        self.virtual_display = None
        self.end_time = None
        self.runner = None
        self.report_file = None
        self.scenario = None
        self.script = None
        self.self_generated_script = False
        self.generated_methods = BetterDict()
        self.runner_working_dir = None
        self.register_reader = True

    def get_virtual_display(self):
        return self.virtual_display

    def add_env(self, env):
        self.additional_env.update(env)

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

    def get_runner_working_dir(self):
        if self.runner_working_dir is None:
            self.runner_working_dir = self.engine.create_artifact("classes", "")
        return self.runner_working_dir

    def _get_testng_xml(self):
        if 'testng-xml' in self.scenario:
            testng_xml = self.scenario.get('testng-xml')
            if testng_xml:
                return testng_xml
            else:
                return None  # empty value for switch off testng.xml path autodetect

        script_path = self.get_script_path()
        if script_path:
            script_dir = get_full_path(script_path, step_up=1)
            testng_xml = os.path.join(script_dir, 'testng.xml')
            if os.path.exists(testng_xml):
                self.log.info("Detected testng.xml file at %s", testng_xml)
                self.scenario['testng-xml'] = testng_xml
                return testng_xml

        return None

    def _create_runner(self, report_file):
        script_type = self.detect_script_type()

        runner_config = BetterDict()

        if script_type == "nose":
            runner_class = NoseTester
            runner_config.merge(self.settings.get("selenium-tools").get("nose"))
        elif script_type == "junit":
            runner_class = JUnitTester
            runner_config.merge(self.settings.get("selenium-tools").get("junit"))
            runner_config['working-dir'] = self.get_runner_working_dir()
            runner_config['props-file'] = self.engine.create_artifact("runner", ".properties")
        elif script_type == "testng":
            runner_class = TestNGTester
            runner_config.merge(self.settings.get("selenium-tools").get("testng"))
            runner_config['working-dir'] = self.get_runner_working_dir()
            runner_config['props-file'] = self.engine.create_artifact("runner", ".properties")
            testng_config = self._get_testng_xml()
            if testng_config:
                runner_config['testng-xml'] = self.engine.find_file(testng_config)
        elif script_type == "rspec":
            runner_class = RSpecTester
            runner_config.merge(self.settings.get("selenium-tools").get("rspec"))
        elif script_type == "mocha":
            runner_class = MochaTester
            runner_config.merge(self.settings.get("selenium-tools").get("mocha"))
        else:
            raise TaurusConfigError("Unsupported script type: %s" % script_type)

        runner_config["script"] = self.script
        runner_config["script-type"] = script_type
        runner_config["artifacts-dir"] = self.engine.artifacts_dir
        runner_config["report-file"] = report_file
        runner_config["stdout"] = self.engine.create_artifact("selenium", ".out")
        runner_config["stderr"] = self.engine.create_artifact("selenium", ".err")
        return runner_class(runner_config, self)

    def _register_reader(self, report_file):
        if self.engine.is_functional_mode():
            reader = FuncSamplesReader(report_file, self.log, self.generated_methods)
            if isinstance(self.engine.aggregator, FunctionalAggregator):
                self.engine.aggregator.add_underling(reader)
        else:
            reader = LoadSamplesReader(report_file, self.log, self.generated_methods)
            if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                self.engine.aggregator.add_underling(reader)
        return reader

    def prepare(self):
        if self.get_load().concurrency and self.get_load().concurrency > 1:
            msg = 'Selenium supports concurrency in cloud provisioning mode only\n'
            msg += 'For details look at http://gettaurus.org/docs/Cloud.md'
            self.log.warning(msg)
        self.set_virtual_display()
        self.scenario = self.get_scenario()
        self.__setup_script()

        self.report_file = self.engine.create_artifact("selenium_tests_report", ".ldjson")
        self.runner = self._create_runner(self.report_file)

        self.runner.prepare()
        if self.register_reader:
            self.reader = self._register_reader(self.report_file)

    def __setup_script(self):
        self.script = self.get_script_path()
        if not self.script:
            if "requests" in self.scenario:
                self.script = self.__tests_from_requests()
                self.self_generated_script = True
            else:
                raise TaurusConfigError("Nothing to test, no requests were provided in scenario")

    def detect_script_type(self):
        if not os.path.exists(self.script):
            raise TaurusConfigError("Script '%s' doesn't exist" % self.script)

        if "runner" in self.execution:
            runner = self.execution["runner"]
            if runner not in SeleniumExecutor.SUPPORTED_RUNNERS:
                msg = "Runner '%s' is not supported. Supported runners: %s"
                raise TaurusConfigError(msg % (runner, SeleniumExecutor.SUPPORTED_RUNNERS))
            self.log.debug("Using script type: %s", runner)
            return runner

        file_types = set()

        if os.path.isfile(self.script):  # regular file received
            file_types.add(os.path.splitext(self.script)[1].lower())
        else:  # dir received: check contained files
            for file_name in get_files_recursive(self.script):
                file_types.add(os.path.splitext(file_name)[1].lower())

        if '.java' in file_types or '.jar' in file_types:
            if self._get_testng_xml() is not None:
                script_type = 'testng'
            else:
                script_type = 'junit'
        elif '.py' in file_types:
            script_type = 'nose'
        elif '.rb' in file_types:
            script_type = 'rspec'
        elif '.js' in file_types:
            script_type = 'mocha'
        else:
            raise TaurusConfigError("Unsupported script type: %s" % self.script)

        self.log.debug("Detected script type: %s", script_type)

        return script_type

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
                raise TaurusInternalException("Virtual display failed: %s" % self.virtual_display.return_code)

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
        if os.path.exists("geckodriver.log"):
            self.engine.existing_artifact("geckodriver.log", True)
        self.free_virtual_display()

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.runner.settings.get("stdout"))
        return self.widget

    def resource_files(self):
        resources = []

        self.scenario = self.get_scenario()
        script = self.scenario.get(Scenario.SCRIPT, None)
        if script:
            resources.append(script)

        resources.extend(self.scenario.get("additional-classpath", []))
        resources.extend(self.settings.get("additional-classpath", []))

        testng_config = self._get_testng_xml()
        if testng_config:
            resources.append(testng_config)

        return resources

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        wdlog = self.engine.create_artifact('webdriver', '.log')
        nose_test = SeleniumScriptBuilder(self.scenario, self.log, wdlog)
        if self.virtual_display:
            nose_test.window_size = self.virtual_display.size
        self.generated_methods.merge(nose_test.build_source_code())
        nose_test.save(filename)
        return filename

    def install_required_tools(self):
        self.scenario = BetterDict()
        runner_classes = {
            "nose": NoseTester,
            "junit": JUnitTester,
            "testng": TestNGTester,
            "rspec": RSpecTester,
            "mocha": MochaTester,
        }
        for runner, runner_class in iteritems(runner_classes):
            runner_config = BetterDict()
            runner_config.merge(self.settings.get("selenium-tools").get(runner))
            runner_config.merge({"script": ''})
            mod = runner_class(runner_config, self)
            mod.run_checklist()


class AbstractTestRunner(object):
    """
    Abstract test runner
    """

    def __init__(self, settings, executor):
        """

        :type settings: dict
        :type executor: SeleniumExecutor
        """
        self.process = None
        self.settings = settings
        self.required_tools = []
        self.executor = executor
        self.scenario = executor.scenario
        self.load = executor.get_load()
        self.script = self.settings.get("script", TaurusConfigError("Script not passed to runner %s" % self))
        self.artifacts_dir = self.settings.get("artifacts-dir")
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
                with open(self.settings.get("stderr")) as fds:
                    std_err = fds.read()
                self.is_failed = True
                msg = "Test runner %s (%s) has failed with retcode %s \n %s"
                raise ToolError(msg % (self.executor.label, self.__class__.__name__, ret_code, std_err.strip()))
            return True
        return False

    def check_tools(self):
        for tool in self.required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s...", tool.tool_name)
                tool.install()

    def shutdown(self):
        shutdown_process(self.process, self.log)
        for desc in self.opened_descriptors:
            desc.close()
        self.opened_descriptors = []


class JavaTestRunner(AbstractTestRunner):
    """
    Allows to test java and jar files
    """

    def __init__(self, config, base_class_path, executor):
        """
        :type config: BetterDict
        :type executor: SeleniumExecutor
        """
        super(JavaTestRunner, self).__init__(config, executor)
        self.working_dir = self.settings.get("working-dir")
        self.target_java = str(config.get("compile-target-java", "1.7"))
        self.base_class_path = base_class_path
        self.base_class_path.extend(executor.settings.get("additional-classpath", []))
        self.base_class_path.extend(self.scenario.get("additional-classpath", []))
        self.base_class_path = [os.path.abspath(executor.engine.find_file(x)) for x in self.base_class_path]

    def prepare(self):
        """
        run checklist, make jar.
        """
        self.run_checklist()

        if not os.path.exists(self.working_dir):
            os.makedirs(self.working_dir)

        if any(self._collect_script_files({'.java'})):
            self.compile_scripts()

    @abstractmethod
    def run_checklist(self):
        pass

    def _collect_script_files(self, extensions):
        file_list = []
        if os.path.isdir(self.script):
            for root, _, files in os.walk(self.script):
                for test_file in files:
                    if os.path.splitext(test_file)[1].lower() in extensions:
                        path = get_full_path(os.path.join(root, test_file))
                        file_list.append(path)
        else:
            if os.path.splitext(self.script)[1].lower() in extensions:
                file_list.append(get_full_path(self.script))
        return file_list

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

        compile_cl = ["javac",
                      "-source", self.target_java,
                      "-target", self.target_java,
                      "-d", self.working_dir,
                      ]
        compile_cl.extend(["-cp", os.pathsep.join(self.base_class_path)])
        compile_cl.extend(self._collect_script_files({".java"}))

        with open(os.path.join(self.artifacts_dir, "javac.out"), 'ab') as javac_out:
            with open(os.path.join(self.artifacts_dir, "javac.err"), 'ab') as javac_err:
                self.log.debug("running javac: %s", compile_cl)
                self.process = shell_exec(compile_cl, stdout=javac_out, stderr=javac_err)
                ret_code = self.process.poll()

                while ret_code is None:
                    self.log.debug("Compiling .java files...")
                    time.sleep(1)
                    ret_code = self.process.poll()

        if ret_code != 0:
            self.log.debug("javac exit code: %s", ret_code)
            with open(javac_err.name) as err_file:
                out = err_file.read()
            raise ToolError("Javac exited with code: %s\n %s" % (ret_code, out.strip()))

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
                    compile_jar_cl = ["jar", "-cf", jar_name, "."]

                self.process = shell_exec(compile_jar_cl, cwd=self.working_dir, stdout=jar_out, stderr=jar_err)
                ret_code = self.process.poll()

                while ret_code is None:
                    self.log.debug("Making jar file...")
                    time.sleep(1)
                    ret_code = self.process.poll()

        if ret_code != 0:
            with open(jar_err.name) as err_file:
                out = err_file.read()
            raise ToolError("Jar exited with code %s\n%s" % (ret_code, out.strip()))

        self.log.info("Making .jar file completed")

    @abstractmethod
    def run_tests(self):
        pass


class JUnitTester(JavaTestRunner):
    """
    Allows to test java and jar files
    """

    def __init__(self, junit_config, executor):
        """
        :type junit_config: BetterDict
        :type executor: SeleniumExecutor
        """
        self.props_file = junit_config.get('props-file', None)

        path_lambda = lambda key, val: get_full_path(junit_config.get(key, val))
        self.junit_path = path_lambda("path", "~/.bzt/selenium-taurus/tools/junit/junit.jar")
        self.hamcrest_path = path_lambda("hamcrest-core", "~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar")
        self.json_jar_path = path_lambda("json-jar", "~/.bzt/selenium-taurus/tools/junit/json.jar")
        self.selenium_server_jar_path = path_lambda("selenium-server", "~/.bzt/selenium-taurus/selenium-server.jar")
        self.junit_listener_path = os.path.join(get_full_path(__file__, step_up=1),
                                                os.pardir,
                                                "resources",
                                                "taurus-junit-1.0.jar")

        base_class_path = [self.selenium_server_jar_path, self.junit_path, self.junit_listener_path,
                           self.hamcrest_path, self.json_jar_path]
        super(JUnitTester, self).__init__(junit_config, base_class_path, executor)

    def run_checklist(self):
        # only check javac if we need to compile. if we have JAR as script - we don't need javac
        if any(self._collect_script_files({'.java'})):
            self.required_tools.append(JavaC("", "", self.log))

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(JavaVM("", "", self.log))
        link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK.format(version=SeleniumExecutor.SELENIUM_VERSION)
        self.required_tools.append(SeleniumServerJar(self.selenium_server_jar_path, link, self.log))
        self.required_tools.append(JUnitJar(self.junit_path, self.log, SeleniumExecutor.JUNIT_VERSION))
        self.required_tools.append(HamcrestJar(self.hamcrest_path, SeleniumExecutor.HAMCREST_DOWNLOAD_LINK))
        self.required_tools.append(JsonJar(self.json_jar_path, SeleniumExecutor.JSON_JAR_DOWNLOAD_LINK))
        self.required_tools.append(JUnitListenerJar(self.junit_listener_path, ""))

        self.check_tools()

    def run_tests(self):
        # java -cp junit.jar:selenium-test-small.jar:
        # selenium-2.46.0/selenium-java-2.46.0.jar:./../selenium-server.jar
        # taurusjunit.CustomRunner runner.properties

        jar_list = [os.path.join(self.working_dir, jar) for jar in os.listdir(self.working_dir) if jar.endswith(".jar")]
        jar_list.extend(self._collect_script_files({".jar"}))
        self.base_class_path.extend(jar_list)

        with open(self.props_file, 'wt') as props:
            props.write("report_file=%s\n" % self.settings.get("report-file").replace(os.path.sep, '/'))

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

        junit_command_line = ["java", "-cp", os.pathsep.join(self.base_class_path), "taurusjunit.CustomRunner",
                              self.props_file]
        self.process = self.executor.execute(junit_command_line,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=self.env)


class TestNGTester(JavaTestRunner):
    """
    Allows to test java and jar files with TestNG
    """

    __test__ = False  # Hello, nosetests discovery mechanism

    def __init__(self, testng_config, executor):
        """
        :type testng_config: BetterDict
        :type executor: SeleniumExecutor
        """
        self.props_file = testng_config.get('props-file', None)

        path_lambda = lambda key, val: get_full_path(testng_config.get(key, val))
        self.testng_path = path_lambda("path", "~/.bzt/selenium-taurus/tools/testng/testng.jar")
        self.hamcrest_path = path_lambda("hamcrest-core", "~/.bzt/selenium-taurus/tools/testng/hamcrest-core.jar")
        self.json_jar_path = path_lambda("json-jar", "~/.bzt/selenium-taurus/tools/testng/json.jar")
        self.selenium_server_jar_path = path_lambda("selenium-server", "~/.bzt/selenium-taurus/selenium-server.jar")
        self.testng_plugin_path = os.path.join(get_full_path(__file__, step_up=1),
                                               os.pardir,
                                               "resources",
                                               "taurus-testng-1.0.jar")

        base_class_path = [self.selenium_server_jar_path, self.testng_path, self.testng_plugin_path,
                           self.hamcrest_path, self.json_jar_path]
        super(TestNGTester, self).__init__(testng_config, base_class_path, executor)

    def run_checklist(self):
        if any(self._collect_script_files({'.java'})):
            self.required_tools.append(JavaC("", "", self.log))

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(JavaVM("", "", self.log))
        link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK.format(version=SeleniumExecutor.SELENIUM_VERSION)
        self.required_tools.append(SeleniumServerJar(self.selenium_server_jar_path, link, self.log))
        self.required_tools.append(TestNGJar(self.testng_path, SeleniumExecutor.TESTNG_DOWNLOAD_LINK))
        self.required_tools.append(HamcrestJar(self.hamcrest_path, SeleniumExecutor.HAMCREST_DOWNLOAD_LINK))
        self.required_tools.append(JsonJar(self.json_jar_path, SeleniumExecutor.JSON_JAR_DOWNLOAD_LINK))
        self.required_tools.append(TestNGPluginJar(self.testng_plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        # java -classpath
        # testng.jar:selenium-server.jar:taurus-testng-1.0.jar:json.jar:compiled.jar
        # taurustestng.TestNGRunner runner.properties

        jar_list = [os.path.join(self.working_dir, jar) for jar in os.listdir(self.working_dir) if jar.endswith(".jar")]
        jar_list.extend(self._collect_script_files({".jar"}))
        self.base_class_path.extend(jar_list)

        with open(self.props_file, 'wt') as props:
            props.write("report_file=%s\n" % self.settings.get("report-file").replace(os.path.sep, '/'))

            if self.load.iterations:
                props.write("iterations=%s\n" % self.load.iterations)

            if self.load.hold:
                props.write("hold_for=%s\n" % self.load.hold)

            for index, item in enumerate(jar_list):
                props.write("target_%s=%s\n" % (index, item.replace(os.path.sep, '/')))

            if self.settings.get('testng-xml'):
                props.write('testng_config=%s\n' % self.settings.get('testng-xml').replace(os.path.sep, '/'))

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        env = BetterDict()
        env.merge(self.env)

        cmdline = ["java", "-cp", os.pathsep.join(self.base_class_path), "taurustestng.TestNGRunner", self.props_file]
        self.process = self.executor.execute(cmdline, stdout=std_out, stderr=std_err, env=env)


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
            self.log.warning("You are using python3, make sure that your scripts are able to run in python3!")

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(TaurusNosePlugin(self.plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, '--report-file', self.settings.get("report-file")]

        if self.load.iterations:
            nose_command_line += ['-i', str(self.load.iterations)]

        if self.load.hold:
            nose_command_line += ['-d', str(self.load.hold)]

        nose_command_line += [self.script]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.process = self.executor.execute(nose_command_line,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=self.env)


class RSpecTester(AbstractTestRunner):
    """
    RSpec tests runner
    """

    def __init__(self, rspec_config, executor):
        super(RSpecTester, self).__init__(rspec_config, executor)
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=1),
                                        os.pardir,
                                        "resources",
                                        "rspec_taurus_plugin.rb")

    def prepare(self):
        self.run_checklist()

    def run_checklist(self):
        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(Ruby(self.settings.get("interpreter", "ruby"), "", self.log))
        self.required_tools.append(RSpec("", "", self.log))
        self.required_tools.append(TaurusRSpecPlugin(self.plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        """
        run rspec plugin
        """
        interpreter = self.settings.get("interpreter", "ruby")

        rspec_cmdline = [
            interpreter,
            self.plugin_path,
            "--report-file",
            self.settings.get("report-file"),
            "--test-suite",
            self.script
        ]

        if self.load.iterations:
            rspec_cmdline += ['--iterations', str(self.load.iterations)]

        if self.load.hold:
            rspec_cmdline += ['--hold-for', str(self.load.hold)]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.process = self.executor.execute(rspec_cmdline,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=self.env)

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            self.log.debug("Test runner exit code: %s", ret_code)
            # rspec returns non-zero code when some tests fail, no need to throw an exception here
            if ret_code != 0:
                self.is_failed = True
            return True
        return False


class MochaTester(AbstractTestRunner):
    """
    Mocha tests runner

    :type node_tool: Node
    :type mocha_tool: Mocha
    """

    def __init__(self, rspec_config, executor):
        super(MochaTester, self).__init__(rspec_config, executor)
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=1),
                                        os.pardir,
                                        "resources",
                                        "mocha-taurus-plugin.js")
        self.tools_dir = get_full_path(self.settings.get("tools-dir", "~/.bzt/selenium-taurus/mocha"))
        self.node_tool = None
        self.npm_tool = None
        self.mocha_tool = None

    def prepare(self):
        self.run_checklist()

    def run_checklist(self):
        self.required_tools.append(TclLibrary(self.log))
        self.node_tool = Node(self.log)
        self.npm_tool = NPM(self.log)
        self.mocha_tool = Mocha(self.tools_dir, self.node_tool, self.npm_tool, self.log)
        self.required_tools.append(self.node_tool)
        self.required_tools.append(self.npm_tool)
        self.required_tools.append(self.mocha_tool)
        self.required_tools.append(JSSeleniumWebdriverPackage(self.tools_dir, self.node_tool, self.npm_tool, self.log))
        self.required_tools.append(TaurusMochaPlugin(self.plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        mocha_cmdline = [
            self.node_tool.executable,
            self.plugin_path,
            "--report-file",
            self.settings.get("report-file"),
            "--test-suite",
            self.script
        ]

        if self.load.iterations:
            mocha_cmdline += ['--iterations', str(self.load.iterations)]

        if self.load.hold:
            mocha_cmdline += ['--hold-for', str(self.load.hold)]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.env["NODE_PATH"] = self.mocha_tool.get_node_path_envvar()

        self.process = self.executor.execute(mocha_cmdline,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=self.env)

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            self.log.debug("Test runner exit code: %s", ret_code)
            # mocha returns non-zero code when tests fail, no need to throw an exception here
            if ret_code != 0:
                self.is_failed = True
            return True
        return False


class SeleniumWidget(Pile, PrioritizedWidget):
    def __init__(self, script, runner_output):
        widgets = []
        self.script_name = Text("Selenium: %s" % os.path.basename(script))
        self.summary_stats = Text("Delayed...")
        self.runner_output = runner_output
        widgets.append(self.script_name)
        widgets.append(self.summary_stats)
        super(SeleniumWidget, self).__init__(widgets)
        PrioritizedWidget.__init__(self, priority=10)

    def update(self):
        reader_summary = ''
        if os.path.exists(self.runner_output):
            with open(self.runner_output, "rt") as fds:
                lines = fds.readlines()
                if lines:
                    line = lines[-1]
                    if not line.endswith("\n") and len(lines) > 1:
                        line = lines[-2]
                    if line and "," in line:
                        reader_summary = line.split(",")[-1]

        if reader_summary:
            self.summary_stats.set_text(reader_summary)
        else:
            self.summary_stats.set_text('In progress...')

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
        self.log.info("Will install %s into %s", self.tool_name, dest)
        junit_dist = self._download(suffix=".jar")
        if not os.path.exists(dest):
            os.makedirs(dest)
        shutil.move(junit_dist, self.tool_path)
        self.log.info("Installed JUnit successfully")

        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)


class TestNGJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TestNGJar, self).__init__("TestNG", tool_path, download_link)


class HamcrestJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(HamcrestJar, self).__init__("HamcrestJar", tool_path, download_link)


class JsonJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JsonJar, self).__init__("JsonJar", tool_path, download_link)


class JavaC(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(JavaC, self).__init__("JavaC", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = subprocess.check_output(["javac", '-version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class RSpec(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(RSpec, self).__init__("RSpec", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            rspec_exec = "rspec.bat" if is_windows() else "rspec"
            output = subprocess.check_output([rspec_exec, '--version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("RSpec check exception: %s", traceback.format_exc())
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class Ruby(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(Ruby, self).__init__("Ruby", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = subprocess.check_output([self.tool_path, '--version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class Node(RequiredTool):
    def __init__(self, parent_logger):
        super(Node, self).__init__("Node.js", "")
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.executable = None

    def check_if_installed(self):
        node_candidates = ["node", "nodejs"]
        for candidate in node_candidates:
            try:
                self.log.debug("Trying %r", candidate)
                output = subprocess.check_output([candidate, '--version'], stderr=subprocess.STDOUT)
                self.log.debug("%s output: %s", candidate, output)
                self.executable = candidate
                return True
            except (CalledProcessError, OSError):
                self.log.debug("%r is not installed", candidate)
                continue
        return False

    def install(self):
        raise ToolError("Automatic installation of nodejs is not implemented. Install it manually")


class NPM(RequiredTool):
    def __init__(self, parent_logger):
        super(NPM, self).__init__("NPM", "")
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.executable = None

    def check_if_installed(self):
        candidates = ["npm"]
        if is_windows():
            candidates.append("npm.cmd")
        for candidate in candidates:
            try:
                self.log.debug("Trying %r", candidate)
                output = subprocess.check_output([candidate, '--version'], stderr=subprocess.STDOUT)
                self.log.debug("%s output: %s", candidate, output)
                self.executable = candidate
                return True
            except (CalledProcessError, OSError):
                self.log.debug("%r is not installed", candidate)
                continue
        return False

    def install(self):
        raise ToolError("Automatic installation of npm is not implemented. Install it manually")


class NPMPackage(RequiredTool):
    def __init__(self, tool_name, package_name, tools_dir, node_tool, npm_tool, parent_logger):
        super(NPMPackage, self).__init__(tool_name, "")
        self.package_name = package_name
        self.tools_dir = tools_dir
        self.node_tool = node_tool
        self.npm_tool = npm_tool
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.node_modules_dir = os.path.join(tools_dir, "node_modules")

    def get_node_path_envvar(self):
        node_path = os.environ.get("NODE_PATH")
        if node_path:
            new_path = node_path + os.pathsep + self.node_modules_dir
        else:
            new_path = self.node_modules_dir
        return new_path

    def check_if_installed(self):
        try:
            node_binary = self.node_tool.executable
            package = self.package_name
            cmdline = [node_binary, '-e', "require('%s'); console.log('%s is installed');" % (package, package)]
            self.log.debug("%s check cmdline: %s", package, cmdline)
            node_path = self.get_node_path_envvar()
            self.log.debug("NODE_PATH for check: %s", node_path)
            env = os.environ.copy()
            env["NODE_PATH"] = str(node_path)
            output = subprocess.check_output(cmdline, env=env, stderr=subprocess.STDOUT)
            self.log.debug("%s check output: %s", self.package_name, output)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("%s check failed: %s", self.package_name, traceback.format_exc())
            return False

    def install(self):
        try:
            cmdline = [self.npm_tool.executable, 'install', self.package_name, '--prefix', self.tools_dir]
            output = subprocess.check_output(cmdline, stderr=subprocess.STDOUT)
            self.log.debug("%s install output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("%s install failed: %s", self.package_name, traceback.format_exc())
            return False


class Mocha(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(Mocha, self).__init__("Mocha", SeleniumExecutor.MOCHA_NPM_PACKAGE_NAME,
                                    tools_dir, node_tool, npm_tool, parent_logger)


class JSSeleniumWebdriverPackage(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(JSSeleniumWebdriverPackage, self).__init__("selenium-webdriver npm package",
                                                         SeleniumExecutor.SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME,
                                                         tools_dir, node_tool, npm_tool, parent_logger)


class JUnitListenerJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(JUnitListenerJar, self).__init__("JUnitListener", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of JUnitListener isn't implemented")


class TestNGPluginJar(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TestNGPluginJar, self).__init__("TestNGPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("TestNG plugin should be bundled with Taurus distribution")


class TaurusNosePlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNosePlugin, self).__init__("TaurusNosePlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus nose plugin isn't implemented")


class TaurusRSpecPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusRSpecPlugin, self).__init__("TaurusRSpecPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus RSpec plugin isn't implemented")


class TaurusMochaPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusMochaPlugin, self).__init__("TaurusMochaPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus mocha plugin isn't implemented")


class SeleniumScriptBuilder(PythonGenerator):
    IMPORTS = """import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait

"""

    def __init__(self, scenario, parent_logger, wdlog):
        super(SeleniumScriptBuilder, self).__init__(scenario, parent_logger)
        self.window_size = None
        self.wdlog = wdlog

    def build_source_code(self):
        self.log.debug("Generating Test Case test methods")
        imports = self.add_imports()
        self.root.append(imports)
        test_class = self.gen_class_definition("TestRequests", ["unittest.TestCase"])
        self.root.append(test_class)
        test_class.append(self.gen_statement("driver = None", indent=4))
        test_class.append(self.gen_new_line())
        test_class.append(self.gen_setupclass_method())
        test_class.append(self.gen_teardownclass_method())
        test_class.append(self.gen_setup_method())

        counter = 0
        methods = {}
        requests = self.scenario.get_requests(False)
        default_address = self.scenario.get("default-address", None)

        for req in requests:
            if req.label:
                label = req.label
            elif req.url:
                label = req.url
            else:
                raise TaurusConfigError("You must specify at least 'url' or 'label' for each requests item")
            mod_label = re.sub('[^0-9a-zA-Z]+', '_', label[:30])
            method_name = 'test_%05d_%s' % (counter, mod_label)
            test_method = self.gen_test_method(method_name)
            methods[method_name] = label
            counter += 1
            test_class.append(test_method)

            if req.url is not None:
                self._add_url_request(default_address, req, test_method)

            for action_config in req.config.get("actions", []):
                test_method.append(self.gen_action(action_config))

            if "assert" in req.config:
                test_method.append(self.gen_statement("body = self.driver.page_source"))
                for assert_config in req.config.get("assert"):
                    for elm in self.gen_assertion(assert_config):
                        test_method.append(elm)

            think_time = req.think_time if req.think_time else self.scenario.get("think-time", None)
            if think_time is not None:
                test_method.append(self.gen_statement("sleep(%s)" % dehumanize_time(think_time)))

            test_method.append(self.gen_statement("pass"))  # just to stub empty case
            test_method.append(self.gen_new_line())

        return methods

    def _add_url_request(self, default_address, req, test_method):
        parsed_url = parse.urlparse(req.url)
        if default_address is not None and not parsed_url.netloc:
            url = default_address + req.url
        else:
            url = req.url
        if req.timeout is not None:
            test_method.append(self.gen_impl_wait(req.timeout))
        test_method.append(self.gen_statement("self.driver.get('%s')" % url))

    def gen_setup_method(self):
        scenario_timeout = dehumanize_time(self.scenario.get("timeout", 30))
        setup_method_def = self.gen_method_definition('setUp', ['self'])
        setup_method_def.append(self.gen_impl_wait(scenario_timeout))
        setup_method_def.append(self.gen_new_line())
        return setup_method_def

    def gen_setupclass_method(self):
        self.log.debug("Generating setUp test method")
        browsers = ["Firefox", "Chrome", "Ie", "Opera"]
        browser = self.scenario.get("browser", "Firefox")
        if browser not in browsers:
            raise TaurusConfigError("Unsupported browser name: %s" % browser)

        setup_method_def = self.gen_decorator_statement('classmethod')
        setup_method_def.append(self.gen_method_definition("setUpClass", ["cls"]))

        if browser == 'Firefox':
            setup_method_def.append(self.gen_statement("profile = webdriver.FirefoxProfile()"))
            statement = "profile.set_preference('webdriver.log.file', %s)" % repr(self.wdlog)
            log_set = self.gen_statement(statement)
            setup_method_def.append(log_set)
            setup_method_def.append(self.gen_statement("cls.driver = webdriver.Firefox(profile)"))
        elif browser == 'Chrome':
            statement = "cls.driver = webdriver.Chrome(service_log_path=%s)"
            setup_method_def.append(self.gen_statement(statement % repr(self.wdlog)))
        else:
            setup_method_def.append(self.gen_statement("cls.driver = webdriver.%s()" % browser))

        scenario_timeout = self.scenario.get("timeout", 30)
        setup_method_def.append(self.gen_impl_wait(scenario_timeout, target='cls'))
        if self.window_size:
            statement = self.gen_statement("cls.driver.set_window_size(%s, %s)" % self.window_size)
            setup_method_def.append(statement)
        else:
            setup_method_def.append(self.gen_statement("cls.driver.maximize_window()"))
        setup_method_def.append(self.gen_new_line())
        return setup_method_def

    def gen_impl_wait(self, timeout, target='self'):
        return self.gen_statement("%s.driver.implicitly_wait(%s)" % (target, dehumanize_time(timeout)))

    def gen_test_method(self, name):
        self.log.debug("Generating test method %s", name)
        test_method = self.gen_method_definition(name, ["self"])
        return test_method

    def gen_teardownclass_method(self):
        self.log.debug("Generating tearDown test method")
        tear_down_method_def = self.gen_decorator_statement('classmethod')
        tear_down_method_def.append(self.gen_method_definition("tearDownClass", ["cls"]))
        tear_down_method_def.append(self.gen_statement("cls.driver.quit()"))
        tear_down_method_def.append(self.gen_new_line())
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
                raise TaurusConfigError("Only 'body' subject supported ")

            assert_message = "'%s' " % val
            if not reverse:
                assert_message += 'not '
            assert_message += 'found in BODY'

            if regexp:
                assert_method = "self.assertEqual" if reverse else "self.assertNotEqual"
                assertion_elements.append(self.gen_statement("re_pattern = re.compile(r'%s')" % val))

                method = '%s(0, len(re.findall(re_pattern, body)), "Assertion: %s")'
                method %= assert_method, assert_message
                assertion_elements.append(self.gen_statement(method))
            else:
                assert_method = "self.assertNotIn" if reverse else "self.assertIn"
                method = '%s("%s", body, "Assertion: %s")'
                method %= assert_method, val, assert_message
                assertion_elements.append(self.gen_statement(method))
        return assertion_elements

    def gen_action(self, action_config):
        aby, atype, param, selector = self._parse_action(action_config)

        bys = {
            'byxpath': "XPATH",
            'bycss': "CSS_SELECTOR",
            'byname': "NAME",
            'byid': "ID",
        }
        if atype in ('click', 'keys'):
            tpl = "self.driver.find_element(By.%s, %r).%s"
            if atype == 'click':
                action = "click()"
            else:
                action = "send_keys(%r)" % param

            return self.gen_statement(tpl % (bys[aby], selector, action))
        elif atype == 'wait':
            tpl = "WebDriverWait(self.driver, %s).until(econd.%s_of_element_located((By.%s, %r)), %r)"
            mode = "visibility" if param == 'visible' else 'presence'
            exc = TaurusInternalException("Timeout value should be present")
            timeout = dehumanize_time(self.scenario.get("timeout", exc))
            errmsg = "Element %r failed to appear within %ss" % (selector, timeout)
            return self.gen_statement(tpl % (timeout, mode, bys[aby], selector, errmsg))

        raise TaurusInternalException("Could not build code for action: %s" % action_config)

    def _parse_action(self, action_config):
        if isinstance(action_config, string_types):
            name = action_config
            param = None
        elif isinstance(action_config, dict):
            name, param = next(iteritems(action_config))
        else:
            raise TaurusConfigError("Unsupported value for action: %s" % action_config)

        expr = re.compile("^(click|wait|keys)(byName|byID|byCSS|byXPath)\((.+)\)$", re.IGNORECASE)
        res = expr.match(name)
        if not res:
            raise TaurusConfigError("Unsupported action: %s" % name)

        atype = res.group(1).lower()
        aby = res.group(2).lower()
        selector = res.group(3)

        # hello, reviewer!
        if selector.startswith('"') and selector.endswith('"'):
            selector = selector[1:-1]
        elif selector.startswith("'") and selector.endswith("'"):
            selector = selector[1:-1]

        return aby, atype, param, selector


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


class LDJSONReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.partial_buffer = ""
        self.offset = 0

    def read(self, last_pass=False):
        if not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        self.fds.seek(self.offset)
        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)
        self.offset = self.fds.tell()

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue
            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""
            yield json.loads(line)

    def __open_fds(self):
        if not os.path.isfile(self.filename):
            return False
        fsize = os.path.getsize(self.filename)
        if not fsize:
            return False
        self.fds = open(self.filename, 'rt', buffering=1)
        return True

    def __del__(self):
        if self.fds is not None:
            self.fds.close()


class SeleniumReportReader(object):
    REPORT_ITEM_KEYS = ["test_case", "test_suite", "status", "start_time", "duration",
                        "error_msg", "error_trace", "extras"]
    TEST_STATUSES = ("PASSED", "FAILED", "BROKEN", "SKIPPED")
    FAILING_TESTS_STATUSES = ("FAILED", "BROKEN")

    def __init__(self, filename, parent_logger, translation_table=None):
        super(SeleniumReportReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.json_reader = LDJSONReader(filename, self.log)
        self.translation_table = translation_table or {}

    def process_label(self, label):
        if label in self.translation_table:
            return self.translation_table[label]

        if isinstance(label, string_types):
            if label.startswith('test_') and label[5:10].isdigit():
                return label[11:]

        return label

    def read(self, last_pass=False):
        for row in self.json_reader.read(last_pass):
            for key in self.REPORT_ITEM_KEYS:
                if key not in row:
                    self.log.debug("Unexpected test record: %s", row)
                    self.log.warning("Test record doesn't conform to schema, skipping, %s", key)
                    continue

            row["test_case"] = self.process_label(row["test_case"])
            yield row


class LoadSamplesReader(ResultsReader):
    STATUS_TO_CODE = {
        "PASSED": "200",
        "SKIPPED": "300",
        "FAILED": "400",
        "BROKEN": "500",
    }

    def __init__(self, filename, parent_logger, translation_table):
        super(LoadSamplesReader, self).__init__()
        self.report_reader = SeleniumReportReader(filename, parent_logger, translation_table)
        self.read_records = 0

    def extract_sample(self, item):
        tstmp = int(item["start_time"])
        label = item["test_case"]
        concur = 1
        rtm = item["duration"]
        cnn = 0
        ltc = 0
        rcd = self.STATUS_TO_CODE.get(item["status"], "UNKNOWN")
        error = item["error_msg"] if item["status"] in SeleniumReportReader.FAILING_TESTS_STATUSES else None
        trname = ""
        byte_count = None
        return tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = self.extract_sample(row)
            yield sample


class FuncSamplesReader(FunctionalResultsReader):
    def __init__(self, filename, parent_logger, translation_table):
        self.report_reader = SeleniumReportReader(filename, parent_logger, translation_table)
        self.read_records = 0

    def read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = FunctionalSample(test_case=row["test_case"], test_suite=row["test_suite"],
                                      status=row["status"], start_time=row["start_time"], duration=row["duration"],
                                      error_msg=row["error_msg"], error_trace=row["error_trace"],
                                      extras=row.get("extras", {}))
            yield sample
