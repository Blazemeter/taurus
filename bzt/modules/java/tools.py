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
import subprocess

from bzt import ToolError
from bzt.utils import shell_exec, RequiredTool


class JarTool(RequiredTool):
    VERSION = ""
    URL = "{remote_addr}{remote_path}"
    REMOTE_ADDR = "http://search.maven.org/remotecontent?filepath="
    REMOTE_PATH = ""
    LOCAL_PATH = "~/.bzt/selenium-taurus/{tool_file}"

    def __init__(self, tool_name, local_path, tool_file):
        tool_file = tool_file.format(version=self.VERSION)
        remote_path = self.REMOTE_PATH.format(version=self.VERSION)

        if not local_path:
            local_path = self.LOCAL_PATH

        local_path = local_path.format(tool_file=tool_file)
        download_link = self.URL.format(remote_addr=self.REMOTE_ADDR, remote_path=remote_path)
        super(JarTool, self).__init__(tool_name=tool_name, tool_path=local_path, download_link=download_link)


class JavaC(RequiredTool):
    def __init__(self, tool_path="javac"):
        super(JavaC, self).__init__("JavaC", tool_path)

    def check_if_installed(self):
        try:
            output = subprocess.check_output([self.tool_path, '-version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (subprocess.CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class SeleniumServer(JarTool):
    VERSION = "3.6"
    REMOTE_ADDR = "http://selenium-release.storage.googleapis.com/"
    REMOTE_PATH = "{version}/selenium-server-standalone-{version}.0.jar"

    def __init__(self, local_path=""):
        tool_file = "selenium-server-{version}.jar"
        super(SeleniumServer, self).__init__("Selenium server", local_path, tool_file)

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


class Json(JarTool):
    REMOTE_PATH = "org/json/json/20160810/json-20160810.jar"

    def __init__(self, tool_path=""):
        tool_file = "json.jar"
        super(Json, self).__init__("Json", tool_path, tool_file)


class TestNG(JarTool):
    VERSION = "6.8.5"
    REMOTE_PATH = "org/testng/testng/{version}/testng-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "testng-{version}.jar"
        super(TestNG, self).__init__("TestNG", tool_path, tool_file)


class Hamcrest(JarTool):
    VERSION = "1.3"
    REMOTE_PATH = "org/hamcrest/hamcrest-core/{version}/hamcrest-core-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "hamcrest-core-{version}.jar"
        super(Hamcrest, self).__init__("HamcrestJar", tool_path, tool_file)


class JUnitJupiterApi(JarTool):
    VERSION = "5.2.0"
    REMOTE_PATH = "org/junit/jupiter/junit-jupiter-api/{version}/junit-jupiter-api-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-jupiter-api-{version}.jar"
        super(JUnitJupiterApi, self).__init__("JUnitJupiterApi", tool_path, tool_file)


class JUnitJupiterEngine(JarTool):
    VERSION = "5.2.0"
    REMOTE_PATH = "org/junit/jupiter/junit-jupiter-engine/{version}/junit-jupiter-engine-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-jupiter-engine-{version}.jar"
        super(JUnitJupiterEngine, self).__init__("JUnitJupiterEngine", tool_path, tool_file)


class JUnitVintageEngine(JarTool):
    VERSION = "5.2.0"
    REMOTE_PATH = "org/junit/vintage/junit-vintage-engine/{version}/junit-vintage-engine-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-vintage-engine-{version}.jar"
        super(JUnitVintageEngine, self).__init__("JUnitVintageEngine", tool_path, tool_file)


class JUnitPlatformCommons(JarTool):
    VERSION = "1.2.0"
    REMOTE_PATH = "org/junit/platform/junit-platform-commons/{version}/junit-platform-commons-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-platform-commons-{version}.jar"
        super(JUnitPlatformCommons, self).__init__("JUnitPlatformCommons", tool_path, tool_file)


class JUnitPlatformEngine(JarTool):
    VERSION = "1.2.0"
    REMOTE_PATH = "org/junit/platform/junit-platform-engine/{version}/junit-platform-engine-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-platform-engine-{version}.jar"
        super(JUnitPlatformEngine, self).__init__("JUnitPlatformEngine", tool_path, tool_file)


class JUnitPlatformLauncher(JarTool):
    VERSION = "1.2.0"
    REMOTE_PATH = "org/junit/platform/junit-platform-launcher/{version}/junit-platform-launcher-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-platform-launcher-{version}.jar"
        super(JUnitPlatformLauncher, self).__init__("JUnitPlatformLauncher", tool_path, tool_file)


class JUnitPlatformRunner(JarTool):
    VERSION = "1.2.0"
    REMOTE_PATH = "org/junit/platform/junit-platform-runner/{version}/junit-platform-runner-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-platform-runner-{version}.jar"
        super(JUnitPlatformRunner, self).__init__("JUnitPlatformRunner", tool_path, tool_file)


class JUnitPlatformSuiteApi(JarTool):
    VERSION = "1.2.0"
    REMOTE_PATH = "org/junit/platform/junit-platform-suite-api/{version}/junit-platform-suite-api-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-platform-suite-api-{version}.jar"
        super(JUnitPlatformSuiteApi, self).__init__("JUnitPlatformSuiteApi", tool_path, tool_file)


class ApiGuardian(JarTool):
    VERSION = "1.0.0"
    REMOTE_PATH = "org/apiguardian/apiguardian-api/{version}/apiguardian-api-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "apiguardian-api-{version}.jar"
        super(ApiGuardian, self).__init__("ApiGuardian", tool_path, tool_file)


class OpenTest4j(JarTool):
    VERSION = "1.1.0"
    REMOTE_PATH = "org/opentest4j/opentest4j/{version}/opentest4j-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "opentest4j-{version}.jar"
        super(OpenTest4j, self).__init__("OpenTest4j", tool_path, tool_file)


class JUnit(JarTool):
    VERSION = "4.12"
    REMOTE_PATH = "junit/junit/{version}/junit-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "junit-{version}.jar"
        super(JUnit, self).__init__("JUnit", tool_path, tool_file)


class TaurusJavaHelper(JarTool):
    VERSION = "1.4"
    REMOTE_PATH = "com/blazemeter/taurus-java-helpers/{version}/taurus-java-helpers-{version}.jar"

    def __init__(self, tool_path=""):
        tool_file = "taurus-java-helpers-{version}.jar"
        super(TaurusJavaHelper, self).__init__("TaurusJavaHelper", tool_path, tool_file)
