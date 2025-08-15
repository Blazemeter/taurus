"""
Copyright 2018 BlazeMeter Inc.

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
import os
import subprocess
import sys
import time
from os import listdir
from os.path import join

from bzt import ToolError
from bzt.modules import SubprocessedExecutor, RemoteExecutor
from bzt.modules.functional import FuncSamplesReader
from bzt.modules.jmeter import JTLReader
from bzt.utils import get_full_path, shell_exec, TclLibrary, JavaVM, BetterDict, get_assembled_value
from .tools import Hamcrest, Json, TaurusJavaHelper, JavaC, JUnitJupiterApi, JUnitJupiterEngine, JarTool
from .tools import JUnitPlatformCommons, JUnitPlatformLauncher, JUnitPlatformEngine, JUnitPlatformRunner
from .tools import JUnitPlatformSuiteApi, JUnitVintageEngine, ApiGuardian, JUnit, OpenTest4j, TestNG


class JavaTestRunner(SubprocessedExecutor):
    """
    Allows to test java and jar files
    :type script: str
    """

    def __init__(self):
        super(JavaTestRunner, self).__init__()
        self.working_dir = os.getcwd()
        self.target_java = "1.8"
        self.props_file = None
        self.class_path = []
        self._tools = []
        self._java_scripts = []
        self._full_install = True
        self.tool = None
        self.report_file_suffix = ".ldjson"

    def install_required_tools(self):
        self._add_jar_tool(Hamcrest, config=self.settings.get("hamcrest-core"))
        self._add_jar_tool(Json, config=self.settings.get("json-jar"))
        self._add_jar_tool(TaurusJavaHelper)

        if self._full_install or self._java_scripts:
            self._tools.append(self._get_tool(JavaC))

        self._tools.append(self._get_tool(TclLibrary))

        self.tool = self._get_tool(JavaVM)
        self._tools.append(self.tool)

        self._check_tools(self._tools)

    def _add_jar_tool(self, req_tool_class, **kwargs):
        req_tool = self._get_tool(req_tool_class, **kwargs)
        self._tools.append(req_tool)
        self.class_path.append(req_tool.tool_path)

    def prepare(self):
        super(JavaTestRunner, self).prepare()

        self.script = self.get_script_path(required=True)

        self.target_java = str(self.settings.get("compile-target-java", self.target_java))

        self._java_scripts = self._collect_script_files({'.java'})

        self.props_file = self.engine.create_artifact("runner", ".properties")

        self.working_dir = self.engine.create_artifact(self.settings.get("working-dir", "classes"), "")
        if not os.path.exists(self.working_dir):
            os.makedirs(self.working_dir)

        self.reporting_setup(suffix=self.report_file_suffix)

        self.class_path.extend(self.settings.get("additional-classpath", []))
        self.class_path.extend(self.get_scenario().get("additional-classpath", []))

        # expand user-defined class path elements
        self.class_path = [self.engine.find_file(x) for x in self.class_path]

        self._full_install = False  # should we move it to install_required_tools() params?
        self.install_required_tools()

        self._compile_scripts()

    def resource_files(self):
        resources = super(JavaTestRunner, self).resource_files()
        resources.extend(self.get_scenario().get("additional-classpath", []))
        global_additional_classpath = self.settings.get("additional-classpath", [])

        execution_files = self.execution.get('files', [], force_set=True)
        execution_files.extend(global_additional_classpath)  # later we need to fix path for sending into cloud

        return resources

    def _collect_script_files(self, extensions):
        file_list = []
        if os.path.isdir(self.script):
            for root, _, files in os.walk(self.script):
                for test_file in files:
                    if os.path.splitext(test_file)[1].lower() in extensions:
                        path = get_full_path(join(root, test_file))
                        file_list.append(path)
        else:
            if os.path.splitext(self.script)[1].lower() in extensions:
                file_list.append(get_full_path(self.script))
        return file_list

    def _compile_scripts(self):
        """
        Compile .java files
        """
        if not self._java_scripts:
            return

        self.log.debug("Compiling .java files started")

        jar_path = join(self.engine.artifacts_dir, self.working_dir, self.settings.get("jar-name", "compiled.jar"))
        if os.path.exists(jar_path):
            self.log.debug(".java files are already compiled, skipping")
            return

        compile_cl = ["javac",
                      "-source", self.target_java,
                      "-target", self.target_java,
                      "-d", self.working_dir,
                      ]

        compile_cl.extend(["-cp", os.pathsep.join(self.class_path)])
        compile_cl.extend(self._java_scripts)

        with open(self.engine.create_artifact("javac", ".out"), 'ab') as javac_out:
            with open(self.engine.create_artifact("javac", ".err"), 'ab') as javac_err:
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

        self._make_jar()

    def _make_jar(self):
        """
        move all .class files to compiled.jar
        """
        self.log.debug("Making .jar started")

        with open(join(self.engine.artifacts_dir, "jar.out"), 'ab') as jar_out:
            with open(join(self.engine.artifacts_dir, "jar.err"), 'ab') as jar_err:
                class_files = [java_file for java_file in listdir(self.working_dir) if java_file.endswith(".class")]
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


class JUnitTester(JavaTestRunner):
    """
    Allows to test java and jar files
    """

    def install_required_tools(self):
        path = get_full_path(self.settings.get("path"))
        if path:
            if os.path.isfile(get_full_path(path, "")):
                self.log.warning("JUnit path must point to directory.")
                path = get_full_path(path, step_up=1)

            path = os.path.join(path, "{tool_file}")

        config = BetterDict()
        if path:
            config["path"] = path

        self._add_jar_tool(JUnitJupiterApi, config=config)
        self._add_jar_tool(JUnitJupiterEngine, config=config)
        self._add_jar_tool(JUnitPlatformCommons, config=config)
        self._add_jar_tool(JUnitPlatformEngine, config=config)
        self._add_jar_tool(JUnitPlatformLauncher, config=config)
        self._add_jar_tool(JUnitPlatformRunner, config=config)
        self._add_jar_tool(JUnitPlatformSuiteApi, config=config)
        self._add_jar_tool(JUnitVintageEngine, config=config)
        self._add_jar_tool(ApiGuardian, config=config)
        self._add_jar_tool(OpenTest4j, config=config)
        self._add_jar_tool(JUnit, config=config)

        super(JUnitTester, self).install_required_tools()

    def prepare(self):
        self.report_file_suffix = ".ldjson" if self.engine.is_functional_mode() else ".csv"
        super(JUnitTester, self).prepare()
        self.__write_props_file()

    def create_func_reader(self, report_file):
        return FuncSamplesReader(report_file, self.engine, self.log)

    def create_load_reader(self, report_file):
        return JTLReader(report_file, self.log, None)

    def startup(self):
        jar_list = [join(self.working_dir, jar) for jar in listdir(self.working_dir) if jar.endswith(".jar")]
        jar_list.extend(self._collect_script_files({".jar"}))
        self.class_path.extend(jar_list)

        class_path = os.pathsep.join(self.class_path)
        runner_class = "com.blazemeter.taurus.junit.CustomRunner"
        junit_cmd_line = [self.tool.tool_path, "-cp", class_path, "-Djna.nosys=true", runner_class, self.props_file]

        self.process = self._execute(junit_cmd_line)

    def __write_props_file(self):
        def write_prop(name, val):
            if val:
                if isinstance(val, list):
                    val = ",".join(val)

                fds.write("{name}={val}\n".format(name=name, val=val))

        props = get_assembled_value(configs=[self.settings, self.get_scenario(), self.execution], key="properties")
        props = props or BetterDict()

        junit_version = str(self.settings.get("junit-version", "4"))
        if junit_version == "5":
            props.merge({"junit_version": 5})

        with open(self.props_file, 'wt') as fds:
            for key in sorted(props.keys()):
                write_prop(key, props[key])

            fds.write("report_file=%s\n" % self.report_file)

            load = self.get_load()

            write_prop("iterations", load.iterations)
            write_prop("hold_for", load.hold)
            write_prop("concurrency", load.concurrency)
            write_prop("ramp_up", load.ramp_up)
            write_prop("steps", load.steps)

            write_prop("run_items", self._get_items_list("run-items"))
            write_prop("include_category", self._get_items_list("include-categories"))
            write_prop("exclude_category", self._get_items_list("exclude-categories"))

    def _get_items_list(self, param):
        scenario = self.get_scenario()
        scen_items = scenario.get(param, [])
        if isinstance(scen_items, str):
            scen_items = scen_items.split(',')

        exec_items = self.execution.get(param, scen_items)
        if isinstance(exec_items, str):
            exec_items = exec_items.split(',')

        return exec_items


class RemoteJUnitTester(JUnitTester):

    def __init__(self):
        super(RemoteJUnitTester, self).__init__()
        self.remote_executor = RemoteExecutor()
        self.remote_jars = []
        self.remote_java_files = []
        self.remote_java_classes = []

    def prepare(self):
        self.remote_executor.prepare()
        # upload mandatory jars
        self.log.info("Downloading mandatory JARs to remote host")
        # JUnitJupiterApi
        jar_url = JarTool.REMOTE_ADDR + JUnitJupiterApi.REMOTE_PATH.format(
            version=JUnitJupiterApi.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + JUnitJupiterApi.TOOL_FILE.format(
            version=JUnitJupiterApi.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitJupiterEngine
        jar_url = JarTool.REMOTE_ADDR + JUnitJupiterEngine.REMOTE_PATH.format(
            version=JUnitJupiterEngine.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + JUnitJupiterEngine.TOOL_FILE.format(
            version=JUnitJupiterEngine.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitPlatformCommons
        jar_url = JarTool.REMOTE_ADDR + JUnitPlatformCommons.REMOTE_PATH.format(
            version=JUnitPlatformCommons.VERSION)
        jar_file_remote_path = (
                self.remote_executor.remote_artifacts_path + '/' + JUnitPlatformCommons.TOOL_FILE.format(
            version=JUnitPlatformCommons.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitPlatformEngine
        jar_url = JarTool.REMOTE_ADDR + JUnitPlatformEngine.REMOTE_PATH.format(
            version=JUnitPlatformEngine.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + JUnitPlatformEngine.TOOL_FILE.format(
            version=JUnitPlatformEngine.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitPlatformLauncher
        jar_url = JarTool.REMOTE_ADDR + JUnitPlatformLauncher.REMOTE_PATH.format(
            version=JUnitPlatformLauncher.VERSION)
        jar_file_remote_path = (
                self.remote_executor.remote_artifacts_path + '/' + JUnitPlatformLauncher.TOOL_FILE.format(
            version=JUnitPlatformLauncher.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitPlatformRunner
        jar_url = JarTool.REMOTE_ADDR + JUnitPlatformRunner.REMOTE_PATH.format(
            version=JUnitPlatformRunner.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + JUnitPlatformRunner.TOOL_FILE.format(
            version=JUnitPlatformRunner.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitPlatformSuiteApi
        jar_url = JarTool.REMOTE_ADDR + JUnitPlatformSuiteApi.REMOTE_PATH.format(
            version=JUnitPlatformSuiteApi.VERSION)
        jar_file_remote_path = (
                self.remote_executor.remote_artifacts_path + '/' + JUnitPlatformSuiteApi.TOOL_FILE.format(
            version=JUnitPlatformSuiteApi.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnitVintageEngine
        jar_url = JarTool.REMOTE_ADDR + JUnitVintageEngine.REMOTE_PATH.format(
            version=JUnitVintageEngine.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + JUnitVintageEngine.TOOL_FILE.format(
            version=JUnitVintageEngine.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # ApiGuardian
        jar_url = JarTool.REMOTE_ADDR + ApiGuardian.REMOTE_PATH.format(
            version=ApiGuardian.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + ApiGuardian.TOOL_FILE.format(
            version=ApiGuardian.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # OpenTest4j
        jar_url = JarTool.REMOTE_ADDR + OpenTest4j.REMOTE_PATH.format(
            version=OpenTest4j.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + OpenTest4j.TOOL_FILE.format(
            version=OpenTest4j.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # JUnit
        jar_url = JarTool.REMOTE_ADDR + JUnit.REMOTE_PATH.format(
            version=JUnit.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + JUnit.TOOL_FILE.format(
            version=JUnit.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # Hamcrest
        jar_url = JarTool.REMOTE_ADDR + Hamcrest.REMOTE_PATH.format(
            version=Hamcrest.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + Hamcrest.TOOL_FILE.format(
            version=Hamcrest.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        # TaurusJavaHelper
        jar_url = JarTool.REMOTE_ADDR + TaurusJavaHelper.REMOTE_PATH.format(
            version=TaurusJavaHelper.VERSION)
        jar_file_remote_path = (self.remote_executor.remote_artifacts_path + '/' + TaurusJavaHelper.TOOL_FILE.format(
            version=TaurusJavaHelper.VERSION)).replace("/", "\\")
        self.remote_executor.command(
            f'bitsadmin /transfer myDownloadJob /download /priority normal "{jar_url}" "{jar_file_remote_path}"')
        self.remote_jars.append(jar_file_remote_path)
        self.log.info("Download of mandatory JARs completed")
        # add additional class path jars
        self.log.info("Uploading additional classpath JARs")
        additional_jars = self.get_scenario().get("additional-classpath", [])
        for additional_jar in additional_jars:
            if os.path.exists(additional_jar):
                java_script_remote_path = self.remote_executor.remote_artifacts_path + '/' + os.path.basename(
                    additional_jar)
                self.remote_executor.upload_file(additional_jar, java_script_remote_path)
                self.remote_jars.append(java_script_remote_path)
            else:
                self.log.warning("Additional classpath JAR %s does not exist, skipping", additional_jar)
        self.log.info("Upload of the additional classpath JARs completed")
        # init class path
        if self.remote_executor.bridge_os == 'windows':
            self.class_path = ';'.join(self.remote_jars)
        else:
            self.class_path = ':'.join(self.remote_jars)
        # upload script java class
        self.log.info("Uploading java test files")
        self.script = self.get_script_path(required=True)
        self._java_scripts = self._collect_script_files({'.java'})
        if self._java_scripts:
            for java_script in self._java_scripts:
                if os.path.exists(java_script):
                    java_script_remote_path = self.remote_executor.remote_artifacts_path + '/' + os.path.basename(
                        java_script)
                    self.remote_executor.upload_file(java_script, java_script_remote_path)
                    self.remote_java_files.append('"' + java_script_remote_path + '"')
                    self.remote_java_classes.append('"' + os.path.basename(
                        java_script).replace('.java', '.class') + '"')
                else:
                    self.log.warning("Java class does not exist, skipping", java_script)
            self.log.info("Upload of the java test files completed")
            # compiling java classes
            self.log.info("Compiling java test files")
            compile_cl = ["javac", "-cp", '"' + self.class_path + '"', ]
            compile_cl.extend(self.remote_java_files)
            self.remote_executor.command(' '.join(compile_cl).replace("/", "\\"),
                                         workingDir=self.remote_executor.remote_artifacts_path)
            # build jar file from test classes
            self.log.info("Building test jar")
            jar_name = self.settings.get("jar-name", "compiled.jar")
            if self.remote_java_classes:
                compile_jar_cl = ["jar", "-cf", jar_name]
                compile_jar_cl.extend(self.remote_java_classes)
            else:
                compile_jar_cl = ["jar", "-cf", jar_name, "."]

            self.remote_executor.command(' '.join(compile_jar_cl).replace("/", "\\"),
                                         workingDir=self.remote_executor.remote_artifacts_path)
            # add built jar to classpath
            if self.remote_executor.bridge_os == 'windows':
                self.class_path = self.class_path + ';' + jar_name
            else:
                self.class_path = self.class_path + ':' + jar_name

        # create result file
        self.log.info("Uploading runner properties completed")
        # create a test report file
        self.report_file_suffix = ".ldjson" if self.engine.is_functional_mode() else ".csv"
        self.remote_report_path = self.remote_executor.remote_artifacts_path + '/RemoteJUnitTester' + \
                                  self.report_file_suffix
        self.reporting_setup(suffix=self.report_file_suffix)
        # create properties file
        self.props_file = self.engine.create_artifact("runner", ".properties")
        self.__write_props_file()
        self.remote_runner_properties_path = self.remote_executor.remote_artifacts_path + '/' + os.path.basename(
            self.props_file)
        self.remote_executor.upload_file(self.props_file, self.remote_runner_properties_path)

    def __write_props_file(self):
        def write_prop(name, val):
            if val:
                if isinstance(val, list):
                    val = ",".join(val)

                fds.write("{name}={val}\n".format(name=name, val=val))

        props = get_assembled_value(configs=[self.settings, self.get_scenario(), self.execution], key="properties")
        props = props or BetterDict()

        junit_version = str(self.settings.get("junit-version", "4"))
        if junit_version == "5":
            props.merge({"junit_version": 5})

        with open(self.props_file, 'wt') as fds:
            for key in sorted(props.keys()):
                write_prop(key, props[key])

            fds.write("report_file=%s\n" % self.remote_report_path)

            load = self.get_load()

            write_prop("iterations", load.iterations)
            write_prop("hold_for", load.hold)
            write_prop("concurrency", load.concurrency)
            write_prop("ramp_up", load.ramp_up)
            write_prop("steps", load.steps)

            write_prop("run_items", self._get_items_list("run-items"))
            write_prop("include_category", self._get_items_list("include-categories"))
            write_prop("exclude_category", self._get_items_list("exclude-categories"))

    def startup(self):
        runner_class = "com.blazemeter.taurus.junit.CustomRunner"
        junit_cmd_line = ['java', "-cp", '"' + self.class_path + '"', runner_class,
                          '"' + self.remote_runner_properties_path + '"']
        self.remote_executor.runner_pid \
            = self.remote_executor.command(' '.join(junit_cmd_line).replace("/", "\\"), wait_for_completion=False,
                                           workingDir=self.remote_executor.remote_artifacts_path).get('pid')
        executable = self.settings.get("interpreter", sys.executable)
        cmd = [
            executable,
            "/tmp/bridge_file_puller.py",
            self.remote_executor.file_url,
            str(1024 * 1024),
            self.remote_report_path.replace('/', '\\'),
            '/tmp/artifacts/RemoteJUnitTester' + self.report_file_suffix
        ]
        # Detach child process using setsid (Linux/Unix)
        subprocess.Popen(cmd, start_new_session=True, close_fds=True)

    def check(self):
        return self.remote_executor.check()

    def install_required_tools(self):
        pass

    def shutdown(self):
        self.remote_executor.shutdown()


class TestNGTester(JavaTestRunner):
    """
    Allows to test java and jar files with TestNG
    """
    __test__ = False  # Hello, nosetests discovery mechanism

    def install_required_tools(self):
        local_path = self.settings.get("path", None)
        config = {}
        if local_path:
            config = BetterDict.from_dict({"config": local_path})
        self._add_jar_tool(TestNG, config=config)
        super(TestNGTester, self).install_required_tools()

    def _store_testng_xml(self):
        if not self.execution.get('testng-xml') and self.settings.get("autodetect-xml", True):
            script_dir = get_full_path(self.get_script_path(), step_up=1)
            testng_xml = os.path.join(script_dir, 'testng.xml')
            if os.path.exists(testng_xml):
                self.log.info("Detected testng.xml file at %s", testng_xml)
                self.execution['testng-xml'] = testng_xml

    def resource_files(self):
        resources = super(TestNGTester, self).resource_files()
        self._store_testng_xml()
        testng_xml = self.execution.get('testng-xml')
        if testng_xml:
            resources.append(testng_xml)

        return resources

    def startup(self):
        # java -classpath
        # testng.jar:selenium-server.jar:taurus-java-helpers.jar:json.jar:compiled.jar
        # com.blazemeter.taurus.testng.TestNGRunner runner.properties

        jar_list = [join(self.working_dir, jar) for jar in listdir(self.working_dir) if jar.endswith(".jar")]
        jar_list.extend(self._collect_script_files({".jar"}))
        self.class_path.extend(jar_list)
        self._store_testng_xml()

        with open(self.props_file, 'wt') as props:
            props.write("report_file=%s\n" % self.report_file)

            load = self.get_load()
            if load.iterations:
                props.write("iterations=%s\n" % load.iterations)

            if load.hold:
                props.write("hold_for=%s\n" % load.hold)

            for index, item in enumerate(jar_list):
                props.write("target_%s=%s\n" % (index, item.replace(os.path.sep, '/')))

            testng_xml = self.execution.get('testng-xml')
            if testng_xml:
                props.write('testng_config=%s\n' % testng_xml.replace(os.path.sep, '/'))

        cmdline = [self.tool.tool_path, "-cp", os.pathsep.join(self.class_path),
                   "com.blazemeter.taurus.testng.TestNGRunner", self.props_file]
        self.process = self._execute(cmdline)
