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
import time
from os import listdir
from os.path import join

from bzt import ToolError
from bzt.engine import HavingInstallableTools
from bzt.modules import SubprocessedExecutor
from bzt.modules.functional import FuncSamplesReader
from bzt.modules.jmeter import JTLReader
from bzt.utils import get_full_path, shell_exec, TclLibrary, JavaVM, BetterDict, get_assembled_value
from .tools import SeleniumServer, Hamcrest, Json, TaurusJavaHelper, JavaC, JUnitJupiterApi, JUnitJupiterEngine
from .tools import JUnitPlatformCommons, JUnitPlatformLauncher, JUnitPlatformEngine, JUnitPlatformRunner
from .tools import JUnitPlatformSuiteApi, JUnitVintageEngine, ApiGuardian, JUnit, OpenTest4j, TestNG


class JavaTestRunner(SubprocessedExecutor, HavingInstallableTools):
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
        self._add_jar_tool(SeleniumServer, config=self.settings.get("selenium-server"))
        self._add_jar_tool(Hamcrest, config=self.settings.get("hamcrest-core"))
        self._add_jar_tool(Json, config=self.settings.get("json-jar"))
        self._add_jar_tool(TaurusJavaHelper)

        if self._full_install or self._java_scripts:
            self._tools.append(self._get_tool(JavaC))

        self._tools.append(self._get_tool(TclLibrary))

        self.tool = self._get_tool(JavaVM, mandatory=True)
        self._tools.append(self.tool)

        self._check_tools(self._tools)

    def _add_jar_tool(self, req_tool_class, **kwargs):
        # todo: it's for backward compatibility only, remove it later
        if "local_path" in kwargs:
            local_path = kwargs.pop("local_path")
            if local_path:
                kwargs["config"] = BetterDict.from_dict({"config": local_path})

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


class TestNGTester(JavaTestRunner):
    """
    Allows to test java and jar files with TestNG
    """
    __test__ = False  # Hello, nosetests discovery mechanism

    def install_required_tools(self):
        self._add_jar_tool(TestNG, local_path=self.settings.get("path"))
        super(TestNGTester, self).install_required_tools()

    def detected_testng_xml(self):
        script_path = self.get_script_path()
        if script_path and self.settings.get("autodetect-xml", True):
            script_dir = get_full_path(script_path, step_up=1)
            testng_xml = join(script_dir, 'testng.xml')
            if os.path.exists(testng_xml):
                return testng_xml
        return None

    def resource_files(self):
        resources = super(TestNGTester, self).resource_files()
        testng_xml = self.execution.get('testng-xml')
        if not testng_xml:
            testng_xml = self.detected_testng_xml()
            if testng_xml:
                self.log.info("Detected testng.xml file at %s", testng_xml)
                self.execution['testng-xml'] = testng_xml
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

        with open(self.props_file, 'wt') as props:
            props.write("report_file=%s\n" % self.report_file)

            load = self.get_load()
            if load.iterations:
                props.write("iterations=%s\n" % load.iterations)

            if load.hold:
                props.write("hold_for=%s\n" % load.hold)

            for index, item in enumerate(jar_list):
                props.write("target_%s=%s\n" % (index, item.replace(os.path.sep, '/')))

            testng_xml = self.execution.get('testng-xml') or self.detected_testng_xml()
            if testng_xml:
                props.write('testng_config=%s\n' % testng_xml.replace(os.path.sep, '/'))

        cmdline = [self.tool.tool_path, "-cp", os.pathsep.join(self.class_path),
                   "com.blazemeter.taurus.testng.TestNGRunner", self.props_file]
        self.process = self._execute(cmdline)

