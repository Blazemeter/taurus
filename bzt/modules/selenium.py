"""
Module holds selenium stuff
"""

import os

from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.console import WidgetProvider
from subprocess import CalledProcessError
import subprocess
import traceback
from six.moves.urllib.request import URLopener
from bzt.utils import download_progress_hook, unzip, shell_exec
import time
import signal
import io
import tempfile
import logging


try:
    from lxml import etree
except ImportError:
    try:
        import cElementTree as etree
    except ImportError:
        import elementtree.ElementTree as etree

class SeleniumExecutor(ScenarioExecutor, WidgetProvider):
    """
    Selenium executor
    """
    SELENIUM_DOWNLOAD_LINK = "http://selenium-release.storage.googleapis.com/{version}/selenium-server-standalone-{version}.0.jar"
    SELEINUM_VERSION = "2.46"

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.selenium_log = None
        self.test_runner = None

    def prepare(self):

        self.selenium_log = self.engine.create_artifact("selenium", ".log")
        self.__run_checklist()
        scenario = self.get_scenario()
        if Scenario.SCRIPT in scenario:
            self.script = self.engine.find_file(scenario[Scenario.SCRIPT])
            self.engine.existing_artifact(self.script)

    def startup(self):
        """
        Start selenium server, build, execute script
        :return:
        """
        # TODO: implement python-unittest scenario support

        selenium_server_cmdline = ["java", "-jar", os.path.realpath(self.settings.get("path"))]

        self.start_time = time.time()
        out = self.engine.create_artifact("selenium-server-stdout", ".log")
        err = self.engine.create_artifact("selenium-server-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")
        self.process = shell_exec(selenium_server_cmdline, cwd=self.engine.artifacts_dir,
                                  stdout=self.stdout_file,
                                  stderr=self.stderr_file)

        self.maven_process = shell_exec(selenium_server_cmdline, cwd=self.engine.artifacts_dir,
                                        stdout=self.stdout_file,
                                        stderr=self.stderr_file)

        maven = Maven()

    def check(self):
        """
        check if test completed
        :return:
        """
        return

    def shutdown(self):
        """
        shutdown server
        :return:
        """
        while self.process and self.process.poll() is None:
            self.log.info("Terminating selenium-server PID: %s", self.process.pid)
            time.sleep(1)
            try:
                os.killpg(self.process.pid, signal.SIGTERM)
            except OSError as exc:
                self.log.debug("Failed to terminate: %s", exc)

            if self.stdout_file:
                self.stdout_file.close()
            if self.stderr_file:
                self.stderr_file.close()

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium server worked for %s seconds",
                           self.end_time - self.start_time)

    def get_widget(self):
        """

        :return:
        """
        pass

    def __run_checklist(self):
        """
        Check all tools: java, maven, selenium-server, installed selenium package (python)
        :return:
        """
        selenium_path = self.settings.get("path", "~/selenium-taurus/selenium-server.jar")
        selenium_path = os.path.abspath(os.path.expanduser(selenium_path))
        selenium_tools_path = self.settings.get("tools-spath", "~/selenium-taurus/tools/")
        selenium_tools_path = os.path.abspath(os.path.expanduser(selenium_tools_path))
        self.settings['path'] = selenium_path
        self.settings['tools-path'] = selenium_tools_path

        try:
            self.__check_java()
        except (OSError, CalledProcessError):
            self.log.debug("Failed to run java: %s", traceback.format_exc())
            return

        try:
            self.__check_selenium_server(selenium_path)
        except (OSError, CalledProcessError):
            self.log.debug("Failed to run selenium-server: %s", traceback.format_exc())
            # try install selenium-server
            self.__install_selenium_server(selenium_path)
            self.__check_selenium_server(selenium_path)
            return
        try:
            self.__check_maven(selenium_tools_path)
        except (OSError, CalledProcessError):
            self.log.debug("Failed to run maven tool: %s", traceback.format_exc())
            # try install selenium-server
            self.__install_maven(selenium_tools_path)
            self.__check_selenium_server(selenium_tools_path)
            pass

    def __check_selenium_server(self, selenium_path):
        """
        Check if selenium server working

        :return: Bool
        """
        self.log.debug("Trying selenium-server: %s > %s", selenium_path, self.selenium_log)
        selenium_launch_command = ["java", "-jar", selenium_path, "-help"]
        selenium_subproc = subprocess.Popen(selenium_launch_command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        selenium_output = selenium_subproc.communicate()[0]

        if selenium_subproc.returncode != 0:
            raise CalledProcessError(selenium_subproc.returncode, " ".join(selenium_launch_command))
        self.log.debug("Selenium check: %s", selenium_output)

    def __install_selenium_server(self, dest_path):
        """
        Download and install selenium-server
        :param dest: path
        :return:
        """

        dest = os.path.dirname(os.path.expanduser(dest_path))
        if not dest:
            dest = "selenium-taurus"
        dest = os.path.abspath(dest)
        if not os.path.exists(dest):
            os.makedirs(dest)

        selenium_path = os.path.join(dest, "selenium-server.jar")
        try:
            self.__check_selenium_server(selenium_path)
            return selenium_path
        except (OSError, CalledProcessError):
            self.log.info("Will try to install selenium server into %s", dest)

        downloader = URLopener()
        selenium_server_download_link = self.settings.get("download-link", SeleniumExecutor.SELENIUM_DOWNLOAD_LINK)
        selenium_version = self.settings.get("version", SeleniumExecutor.SELEINUM_VERSION)
        selenium_server_download_link = selenium_server_download_link.format(version=selenium_version)
        self.log.info("Downloading %s", selenium_server_download_link)
        try:
            downloader.retrieve(selenium_server_download_link, selenium_path, download_progress_hook)
        except BaseException as exc:
            self.log.error("Error while downloading %s", selenium_server_download_link)
            raise exc

    def __check_java(self):
        """
        Check java
        :return:
        """
        try:
            jout = subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
            self.log.debug("Java check: %s", jout)
        except BaseException:
            self.log.warning("Failed to run java: %s", traceback.format_exc())
            raise RuntimeError("The 'java' is not operable or not available. Consider installing it")


class AbstractTestRunner():
    def check_requirements(self):
        raise NotImplementedError
    def check_if_installed(self):
        raise NotImplementedError
    def install(self):
        raise NotImplementedError
    def run_tests(self):
        raise NotImplementedError


class Maven(AbstractTestRunner):

    MAVEN_DOWNLOAD_LINK = "http://apache-mirror.rbc.ru/pub/apache/maven/maven-3/{version}/binaries/apache-maven-{version}-bin.zip"
    MAVEN_VERSION = "3.3.3"

    def __init__(self, maven_config, executor_path = "~/selenium-tools/"):
        self.settings = maven_config
        self.maven_path = maven_config.get("path", "tools/maven/bin/mvn")
        # self.parent_logger =  logging.getLogger('')
        self.log = logging.getLogger('')
        self.pom = maven_config.get("pom", "tools/maven/bin/mvn")

    def check_if_installed(self):
        """
        Check if maven installed
        """
        try:
            subprocess.check_output(["mvn", "-v"], stderr=subprocess.STDOUT)  # check if installed globally
            return True
        except (OSError, CalledProcessError):
            try:
                subprocess.check_output([self.maven_path, "-v"], stderr=subprocess.STDOUT)
                return True
            except (OSError, CalledProcessError):
                return False

    def install(self):
        """
        Install maven
        """
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(self.maven_path)))
        if not dest:
            dest = "selenium-taurus/tools/maven/"
        dest = os.path.abspath(dest)

        if not os.path.exists(dest):
            os.makedirs(dest)

        with tempfile.NamedTemporaryFile() as maven_zip_path:
            downloader = URLopener()
            maven_download_link = self.settings.get("download-link", Maven.MAVEN_DOWNLOAD_LINK)
            maven_version = self.settings.get("maven-version", Maven.MAVEN_VERSION)
            maven_download_link = maven_download_link.format(version=maven_version)
            self.log.info("Downloading %s", maven_download_link)
            try:
                downloader.retrieve(maven_download_link, maven_zip_path.name, download_progress_hook)
            except BaseException as exc:
                self.log.error("Error while downloading %s", maven_download_link)
                raise exc

            self.log.info("Unzipping %s", os.path.join(dest, "maven"))
            unzip(maven_zip_path.name, dest, "apache-maven-%s" % Maven.MAVEN_VERSION)
            os.chmod(self.maven_path, 0o755)
            self.log.info("Installed maven successfully")
            return self.maven_path

    def make_pom(self):
        self.pom = PomFile(features)

    def

    def generate_pom(self):
        generated_pom = """<?xml version="1.0" encoding="UTF-8"?>
<project>
<modelVersion>4.0.0</modelVersion>
<groupId>sample.pom</groupId>
<artifactId>web-driver-jenkins-integration</artifactId>
<version>1.0.0-SNAPSHOT</version>
<dependencies>
<dependency>
<groupId>junit</groupId>
<artifactId>junit</artifactId>
<version>4.11</version>
</dependency>
<dependency>
<groupId>org.seleniumhq.selenium</groupId>
<artifactId>selenium-java</artifactId>
<version>LATEST</version>
</dependency>
</dependencies>
</project>"""
        return

    def build(self):
        pass

    def run_test(self):
        pass

class PomGenerator():
    def __init__(self):
        self.xml_tree = None




class PomFile():
    def __init__(self):
        self.buff = io.StringIO()
