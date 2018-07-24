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
    MAVEN_URL = "http://search.maven.org/remotecontent?filepath={maven_path}"
    LOCAL_PATH = "~/.bzt/selenium-taurus/{tool_file}"
    VERSION = ""

    def __init__(self, tool_name, local_path, tool_file, maven_path, version=VERSION):
        tool_file = tool_file.format(version=version)
        maven_path = maven_path.format(version=version)

        if not local_path:
            local_path = self.LOCAL_PATH.format(tool_file=tool_file)

        local_path = local_path.format(tool_file=tool_file)
        download_link = self.MAVEN_URL.format(maven_path=maven_path)
        super(JarTool, self).__init__(tool_name=tool_name, tool_path=local_path, download_link=download_link)


class JavaC(RequiredTool):
    def __init__(self, parent_logger, tool_path="javac"):
        super(JavaC, self).__init__("JavaC", tool_path)
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


class SeleniumServer(RequiredTool):
    def __init__(self, local_path):
        download_link = "http://selenium-release.storage.googleapis.com/3.6/selenium-server-standalone-3.6.0.jar"
        if not local_path:
            local_path = "~/.bzt/selenium-taurus/selenium-server.jar"

        super(SeleniumServer, self).__init__("Selenium server", local_path, download_link)

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
    def __init__(self, tool_path):
        maven_path = "org/json/json/20160810/json-20160810.jar"
        super(Json, self).__init__("Json", tool_path, tool_file="json.jar", maven_path=maven_path)


class TestNG(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/testng/testng/{version}/testng-{version}.jar"
        version = "6.8.5"
        tool_file = "testng-{version}.jar"
        super(TestNG, self).__init__("TestNG", tool_path, tool_file, maven_path, version)


class Hamcrest(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/hamcrest/hamcrest-core/1.3/hamcrest-core-{version}.jar"
        tool_file = "hamcrest-core-{version}.jar"
        version = "1.3"
        super(Hamcrest, self).__init__("HamcrestJar", tool_path, tool_file, maven_path, version)


class JUnitJupiterApi(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/jupiter/junit-jupiter-api/{version}/junit-jupiter-api-{version}.jar"
        tool_file = "junit-jupiter-api-{version}.jar"
        version = "5.2.0"
        super(JUnitJupiterApi, self).__init__("JUnitJupiterApi", tool_path, tool_file, maven_path, version)


class JUnitJupiterEngine(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/jupiter/junit-jupiter-engine/{version}/junit-jupiter-engine-{version}.jar"
        tool_file = "junit-jupiter-engine-{version}.jar"
        version = "5.2.0"
        super(JUnitJupiterEngine, self).__init__("JUnitJupiterEngine", tool_path, tool_file, maven_path, version)


class JUnitPlatformCommons(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/platform/junit-platform-commons/{version}/junit-platform-commons-{version}.jar"
        tool_file = "junit-platform-commons-{version}.jar"
        version = "1.2.0"
        super(JUnitPlatformCommons, self).__init__("JUnitPlatformCommons", tool_path, tool_file, maven_path, version)


class JUnitPlatformEngine(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/platform/junit-platform-engine/{version}/junit-platform-engine-{version}.jar"
        tool_file = "junit-platform-engine-{version}.jar"
        version = "1.2.0"
        super(JUnitPlatformEngine, self).__init__("JUnitPlatformEngine", tool_path, tool_file, maven_path, version)


class JUnitPlatformLauncher(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/platform/junit-platform-launcher/{version}/junit-platform-launcher-{version}.jar"
        tool_file = "junit-platform-launcher-{version}.jar"
        version = "1.2.0"
        super(JUnitPlatformLauncher, self).__init__("JUnitPlatformLauncher", tool_path, tool_file, maven_path, version)


class JUnitPlatformRunner(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/platform/junit-platform-runner/{version}/junit-platform-runner-{version}.jar"
        tool_file = "junit-platform-runner-{version}.jar"
        version = "1.2.0"
        super(JUnitPlatformRunner, self).__init__("JUnitPlatformRunner", tool_path, tool_file, maven_path, version)


class JUnitPlatformSuiteApi(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/platform/junit-platform-suite-api/{version}/junit-platform-suite-api-{version}.jar"
        tool_file = "junit-platform-suite-api-{version}.jar"
        version = "1.2.0"
        super(JUnitPlatformSuiteApi, self).__init__("JUnitPlatformSuiteApi", tool_path, tool_file, maven_path, version)


class JUnitVintageEngine(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/junit/vintage/junit-vintage-engine/{version}/junit-vintage-engine-{version}.jar"
        tool_file = "junit-vintage-engine-{version}.jar"
        version = "5.2.0"
        super(JUnitVintageEngine, self).__init__("JUnitVintageEngine", tool_path, tool_file, maven_path, version)


class ApiGuardian(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/apiguardian/apiguardian-api/{version}/apiguardian-api-{version}.jar"
        tool_file = "apiguardian-api-{version}.jar"
        version = "1.0.0"
        super(ApiGuardian, self).__init__("ApiGuardian", tool_path, tool_file, maven_path, version)


class OpenTest4j(JarTool):
    def __init__(self, tool_path):
        maven_path = "org/opentest4j/opentest4j/{version}/opentest4j-{version}.jar"
        tool_file = "opentest4j-{version}.jar"
        version = "1.1.0"
        super(OpenTest4j, self).__init__("OpenTest4j", tool_path, tool_file, maven_path, version)


class JUnit(JarTool):
    def __init__(self, tool_path):
        maven_path = "junit/junit/{version}/junit-{version}.jar"
        tool_file = "junit-{version}.jar"
        version = "4.12"
        super(JUnit, self).__init__("JUnit", tool_path, tool_file, maven_path, version)


class TaurusJavaHelper(JarTool):
    def __init__(self, tool_path):
        maven_path = "com/blazemeter/taurus-java-helpers/{version}/taurus-java-helpers-{version}.jar"
        tool_file = "taurus-java-helpers-{version}.jar"
        version = "1.2"
        super(TaurusJavaHelper, self).__init__("TaurusJavaHelper", tool_path, tool_file, maven_path, version)
