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
import re

from bzt.utils import RequiredTool, parse_java_version, CALL_PROBLEMS, BetterDict


class JarTool(RequiredTool):
    VERSION = ""
    URL = "{remote_addr}{remote_path}"
    REMOTE_ADDR = "http://search.maven.org/remotecontent?filepath="
    REMOTE_PATH = ""
    LOCAL_PATH = "~/.bzt/selenium-taurus/{tool_file}"
    TOOL_FILE = ""

    def __init__(self, config=None, **kwargs):
        if config is None:
            config = BetterDict()

        if not isinstance(config, dict):
            config = BetterDict.from_dict({"path": config})

        version = config.get("version", self.VERSION)

        tool_file = self.TOOL_FILE.format(version=version)

        local_path = config.get("path", self.LOCAL_PATH)
        local_path = local_path.format(tool_file=tool_file)

        download_link = config.get("download-link", self.URL)

        remote_path = self.REMOTE_PATH.format(version=version)
        download_link = download_link.format(remote_addr=self.REMOTE_ADDR, remote_path=remote_path)

        super(JarTool, self).__init__(tool_path=local_path, download_link=download_link, version=version, **kwargs)


class JavaC(RequiredTool):
    def __init__(self, **kwargs):
        super(JavaC, self).__init__(tool_path="javac", installable=False, **kwargs)

    def _get_version(self, output):
        versions = re.findall("javac\ ([\d\._]*)", output)
        version = parse_java_version(versions)

        if not version:
            self.log.warning("Tool version parsing error: %s", output)

        return version

    def check_if_installed(self):
        self.log.debug("Trying %s: %s", self.tool_name, self.tool_path)
        try:
            out, err = self.call([self.tool_path, "-version"])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)
        self.version = self._get_version(out)
        return True


class SeleniumServer(RequiredTool):
    VERSION = "3.141"
    REMOTE_ADDR = "http://selenium-release.storage.googleapis.com/"
    REMOTE_PATH = "{short_version}/selenium-server-standalone-{full_version}.jar"
    TOOL_FILE = "selenium-server-{version}.jar"

    def __init__(self, config, **kwargs):
        if not isinstance(config, dict):
            config = BetterDict.from_dict({"path": config})

        version = config.get("version", self.VERSION)
        version = str(version).split('.')
        version.extend(['0'] * (3 - len(version)))
        short_version = '.'.join(version[:2])   # 2 elements
        full_version = '.'.join(version)        # 3+ elements

        remote_path = config.get("remote-path", self.REMOTE_PATH)
        remote_path = remote_path.format(short_version=short_version, full_version=full_version)

        tool_file = config.get("tool-file", self.TOOL_FILE)
        tool_file = tool_file.format(version=full_version)

        local_path = config.get("path", JarTool.LOCAL_PATH)
        local_path = local_path.format(tool_file=tool_file)

        download_link = config.get("download-link", JarTool.URL)
        download_link = download_link.format(remote_addr=self.REMOTE_ADDR, remote_path=remote_path)

        super(SeleniumServer, self).__init__(
            tool_path=local_path,
            download_link=download_link,
            version=full_version,
            **kwargs)

    def check_if_installed(self):
        self.log.debug("Trying %s: %s", self.tool_name, self.tool_path)
        try:
            out, err = self.call(["java", "-jar", self.tool_path, "-help"])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)
        return True


class Json(JarTool):
    REMOTE_PATH = "org/json/json/20160810/json-20160810.jar"
    TOOL_FILE = "json.jar"


class TestNG(JarTool):
    VERSION = "6.9.9"
    REMOTE_PATH = "org/testng/testng/{version}/testng-{version}.jar"
    TOOL_FILE = "testng-{version}.jar"


class Hamcrest(JarTool):
    VERSION = "1.3"
    REMOTE_PATH = "org/hamcrest/hamcrest-core/{version}/hamcrest-core-{version}.jar"
    TOOL_FILE = "hamcrest-core-{version}.jar"


class JUnitJupiterApi(JarTool):
    VERSION = "5.8.2"
    REMOTE_PATH = "org/junit/jupiter/junit-jupiter-api/{version}/junit-jupiter-api-{version}.jar"
    TOOL_FILE = "junit-jupiter-api-{version}.jar"


class JUnitJupiterEngine(JarTool):
    VERSION = "5.8.2"
    REMOTE_PATH = "org/junit/jupiter/junit-jupiter-engine/{version}/junit-jupiter-engine-{version}.jar"
    TOOL_FILE = "junit-jupiter-engine-{version}.jar"


class JUnitVintageEngine(JarTool):
    VERSION = "5.8.2"
    REMOTE_PATH = "org/junit/vintage/junit-vintage-engine/{version}/junit-vintage-engine-{version}.jar"
    TOOL_FILE = "junit-vintage-engine-{version}.jar"


class JUnitPlatformCommons(JarTool):
    VERSION = "1.8.2"
    REMOTE_PATH = "org/junit/platform/junit-platform-commons/{version}/junit-platform-commons-{version}.jar"
    TOOL_FILE = "junit-platform-commons-{version}.jar"


class JUnitPlatformEngine(JarTool):
    VERSION = "1.8.2"
    REMOTE_PATH = "org/junit/platform/junit-platform-engine/{version}/junit-platform-engine-{version}.jar"
    TOOL_FILE = "junit-platform-engine-{version}.jar"


class JUnitPlatformLauncher(JarTool):
    VERSION = "1.8.2"
    REMOTE_PATH = "org/junit/platform/junit-platform-launcher/{version}/junit-platform-launcher-{version}.jar"
    TOOL_FILE = "junit-platform-launcher-{version}.jar"


class JUnitPlatformRunner(JarTool):
    VERSION = "1.8.2"
    REMOTE_PATH = "org/junit/platform/junit-platform-runner/{version}/junit-platform-runner-{version}.jar"
    TOOL_FILE = "junit-platform-runner-{version}.jar"


class JUnitPlatformSuiteApi(JarTool):
    VERSION = "1.8.2"
    REMOTE_PATH = "org/junit/platform/junit-platform-suite-api/{version}/junit-platform-suite-api-{version}.jar"
    TOOL_FILE = "junit-platform-suite-api-{version}.jar"


class ApiGuardian(JarTool):
    VERSION = "1.1.2"
    REMOTE_PATH = "org/apiguardian/apiguardian-api/{version}/apiguardian-api-{version}.jar"
    TOOL_FILE = "apiguardian-api-{version}.jar"


class OpenTest4j(JarTool):
    VERSION = "1.2.0"
    REMOTE_PATH = "org/opentest4j/opentest4j/{version}/opentest4j-{version}.jar"
    TOOL_FILE = "opentest4j-{version}.jar"


class JUnit(JarTool):
    VERSION = "4.13.2"
    REMOTE_PATH = "junit/junit/{version}/junit-{version}.jar"
    TOOL_FILE = "junit-{version}.jar"


class TaurusJavaHelper(JarTool):
    VERSION = "1.9"
    REMOTE_PATH = "com/blazemeter/taurus-java-helpers/{version}/taurus-java-helpers-{version}.jar"
    TOOL_FILE = "taurus-java-helpers-{version}.jar"
