"""
Copyright 2017 BlazeMeter Inc.

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
import time
from os import listdir
from os.path import join

from bzt import ToolError
from bzt.engine import HavingInstallableTools
from bzt.modules import SubprocessedExecutor
from bzt.six import string_types
from bzt.utils import get_full_path, shell_exec, TclLibrary, JavaVM, RequiredTool, MirrorsManager, TaurusJavaHelperJar

SELENIUM_DOWNLOAD_LINK = "http://selenium-release.storage.googleapis.com/3.6/" \
                         "selenium-server-standalone-3.6.0.jar"

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

JUNIT_PATH = "~/.bzt/selenium-taurus/tools/junit/junit.jar"
HAMCREST_PATH = "~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar"
JSON_JAR_PATH = "~/.bzt/selenium-taurus/tools/junit/json.jar"
SELENIUM_SERVER_PATH = "~/.bzt/selenium-taurus/selenium-server.jar"
TESTNG_PATH = "~/.bzt/selenium-taurus/tools/testng/testng.jar"


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

    def install_required_tools(self):
        selenium_link = SELENIUM_DOWNLOAD_LINK.format(version=SELENIUM_VERSION)
        selenium_path = self.settings.get("selenium-server", SELENIUM_SERVER_PATH)
        self._add_jar_tool(SeleniumServerJar(selenium_path, selenium_link, self.log))

        hamcrest_path = self.settings.get("hamcrest-core", HAMCREST_PATH)
        self._add_jar_tool(HamcrestJar(hamcrest_path, HAMCREST_DOWNLOAD_LINK))

        json_jar_path = self.settings.get("json-jar", JSON_JAR_PATH)
        self._add_jar_tool(JsonJar(json_jar_path, JSON_JAR_DOWNLOAD_LINK))

        self._add_jar_tool(TaurusJavaHelperJar(self.log))

        if self._full_install or self._java_scripts:
            self._tools.append(JavaC(self.log))

        self._tools.append(TclLibrary(self.log))
        self._tools.append(JavaVM(self.log))

        self._check_tools(self._tools)

    def _add_jar_tool(self, req_tool):
        self._tools.append(req_tool)
        self.class_path.append(req_tool.tool_path)

    def prepare(self):
        self.script = self.get_script_path(required=True)

        self.target_java = str(self.settings.get("compile-target-java", self.target_java))

        self._java_scripts = self._collect_script_files({'.java'})

        self.props_file = self.engine.create_artifact("runner", ".properties")

        self.working_dir = self.engine.create_artifact(self.settings.get("working-dir", "classes"), "")
        if not os.path.exists(self.working_dir):
            os.makedirs(self.working_dir)

        self.reporting_setup(suffix=".ldjson")

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


class JUnitTester(JavaTestRunner, HavingInstallableTools):
    """
    Allows to test java and jar files
    """
    def install_required_tools(self):
        junit_path = self.engine.find_file(self.settings.get("path", JUNIT_PATH))
        self._add_jar_tool(JUnitJar(junit_path, self.log, JUNIT_VERSION))

        super(JUnitTester, self).install_required_tools()

    def startup(self):
        # java -cp junit.jar:selenium-test-small.jar:
        # selenium-2.46.0/selenium-java-2.46.0.jar:./../selenium-server.jar
        # com.blazemeter.taurus.junit.CustomRunner runner.properties

        jar_list = [join(self.working_dir, jar) for jar in listdir(self.working_dir) if jar.endswith(".jar")]
        jar_list.extend(self._collect_script_files({".jar"}))
        self.class_path.extend(jar_list)
        scenario = self.get_scenario()

        with open(self.props_file, 'wt') as fds:
            fds.write("report_file=%s\n" % self.report_file)

            load = self.get_load()
            if load.iterations:
                fds.write("iterations=%s\n" % load.iterations)

            if load.hold:
                fds.write("hold_for=%s\n" % load.hold)

            for index, item in enumerate(jar_list):
                fds.write("target_%s=%s\n" % (index, item.replace(os.path.sep, '/')))

            run_items = self._get_items_list('run-items')
            if run_items:
                fds.write("run_items=%s\n" % ','.join(run_items))

            includes = self._get_items_list('include-categories')
            if includes:
                fds.write("include_category=%s\n" % ','.join(includes))

            excludes = self._get_items_list('exclude-categories')
            if excludes:
                fds.write("exclude_category=%s\n" % ','.join(excludes))

            props = self.settings.get("properties")
            props.merge(scenario.get("properties"))
            props.merge(self.execution.get("properties"))
            for key in sorted(props.keys()):
                fds.write("%s=%s\n" % (key, props[key]))

        class_path = os.pathsep.join(self.class_path)
        junit_cmd_line = ["java", "-cp", class_path, "-Djna.nosys=true",
                          "com.blazemeter.taurus.junit.CustomRunner", self.props_file]

        self._start_subprocess(junit_cmd_line)

    def _get_items_list(self, param):
        scenario = self.get_scenario()
        scen_items = scenario.get(param, [])
        if isinstance(scen_items, string_types):
            scen_items = scen_items.split(',')

        exec_items = self.execution.get(param, scen_items)
        if isinstance(exec_items, string_types):
            exec_items = exec_items.split(',')

        return exec_items


class TestNGTester(JavaTestRunner, HavingInstallableTools):
    """
    Allows to test java and jar files with TestNG
    """
    __test__ = False  # Hello, nosetests discovery mechanism

    def install_required_tools(self):
        testng_path = self.engine.find_file(self.settings.get("path", TESTNG_PATH))
        self._add_jar_tool(TestNGJar(testng_path, TESTNG_DOWNLOAD_LINK))

        super(TestNGTester, self).install_required_tools()

    def detected_testng_xml(self):
        script_path = self.get_script_path()
        if script_path and self.settings.get("autodetect-xml", True):
            script_dir = get_full_path(script_path, step_up=1)
            testng_xml = os.path.join(script_dir, 'testng.xml')
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

        cmdline = ["java", "-cp", os.pathsep.join(self.class_path),
                   "com.blazemeter.taurus.testng.TestNGRunner", self.props_file]
        self._start_subprocess(cmdline)


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
    def __init__(self, parent_logger, tool_path='javac', download_link=''):
        super(JavaC, self).__init__("JavaC", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = subprocess.check_output([self.tool_path, '-version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (subprocess.CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


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


class JUnitMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, junit_version):
        self.junit_version = junit_version
        super(JUnitMirrorsManager, self).__init__(JUNIT_MIRRORS_SOURCE, parent_logger)

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
        default_link = JUNIT_DOWNLOAD_LINK.format(version=self.junit_version)
        if default_link not in links:
            links.append(default_link)
        self.log.debug('Total mirrors: %d', len(links))
        return links
