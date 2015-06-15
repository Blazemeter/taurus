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
from bzt.utils import download_progress_hook, unzip, shell_exec, humanize_time
import time
import signal
import urwid
import tempfile
import logging
import shutil
import threading
from bzt.modules.aggregator import ResultsReader

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
        self.widget = None
        self.reader = None

    def prepare(self):
        """
        1) Locate script or folder
        2) check type py/java
        3) check tool readiness (maven or nose + pip selenium)
        :return:
        """

        self.reader = DumbReader()
        self.selenium_log = self.engine.create_artifact("selenium", ".log")
        scenario = self.get_scenario()
        # TODO: check if scenario is directory or script

        self.__run_checklist()

        script_type, script_is_folder = self.detect_script_type(scenario.get("script"))

        if script_type == "java":
            self.test_runner = Maven(self.settings)
        elif script_type == "python":
            self.test_runner = NoseTester()
        elif script_type == "jar":
            self.test_runner = JunitTester()

        if not self.test_runner.check_if_installed():
            self.test_runner.install()

        tests_scripts_folder = os.path.join(self.engine.artifacts_dir, "selenium_scripts")
        if Scenario.SCRIPT in scenario:
            if script_is_folder:
                shutil.copytree(scenario.get("script"), tests_scripts_folder)
            else:
                os.makedirs(tests_scripts_folder)
                shutil.copy2(scenario.get("script"), tests_scripts_folder)

    def detect_script_type(self, script_path):
        """
        checks if script is java or python
        if it's folder or single script
        :return:
        """
        if os.path.isdir(script_path):
            self.log.info("processing all scripts in a folder %s", script_path)
            for script_file in os.listdir(script_path):
                file_ext = os.path.splitext(script_file)[1].lower()
                if file_ext == ".java":
                    self.log.info("detected script type: java")
                    return ("java", True)
                elif file_ext == ".py":
                    self.log.info("detected script type: python")
                    return ("python", True)
            self.log.error("Unknown script type.")
            raise BaseException("unknown script type")
        else:
            self.log.info("checking type of script %s", script_path)
            file_ext = os.path.splitext(script_path)[1]
            if file_ext == ".java":
                self.log.info("detected script type: java")
                return ("java", False)
            elif file_ext == ".py":
                self.log.info("detected script type: python")
                return ("python", False)
            elif file_ext == ".jar":
                self.log.info("detected script type: jar")
                return ("jar", False)
            else:
                self.log.error("Unknown script type.")
                raise BaseException("unknown script type")

    def startup(self):
        """
        Start selenium server, execute script
        :return:
        """
        # TODO: implement selenium-server grid

        selenium_server_cmdline = ["java", "-jar", os.path.realpath(self.settings.get("path"))]  # , "-debug", "-log", "test.log"

        self.start_time = time.time()
        out = self.engine.create_artifact("selenium-server-stdout", ".log")
        err = self.engine.create_artifact("selenium-server-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")

        self.process = shell_exec(selenium_server_cmdline, cwd=self.engine.artifacts_dir,
                                  stdout=self.stdout_file,
                                  stderr=self.stderr_file)

        self.test_runner.run_tests(self.engine.artifacts_dir)

    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            self.widget.update()

        self.retcode = self.test_runner.process.poll()
        if self.retcode is not None:
            if self.retcode != 0:
                self.log.info("test runner exit code: %s", self.retcode)
                raise RuntimeError("test runner exited with non-zero code")
            return True
        return False

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
        if not self.widget:
            self.widget = SeleniumWidget(self)
        return self.widget

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

    def run_tests(self, artifacts_dir):
        raise NotImplementedError

    def is_finished(self):
        raise NotImplementedError

class Maven(AbstractTestRunner):
    MAVEN_DOWNLOAD_LINK = "http://apache-mirror.rbc.ru/pub/apache/maven/maven-3/{version}/binaries/apache-maven-{version}-bin.zip"
    MAVEN_VERSION = "3.3.3"

    def __init__(self, executor_config):
        self.settings = executor_config.get("maven", {})
        self.maven_path = self.settings.get("path", "~/selenium-taurus/tools/maven/bin/mvn")
        self.maven_path = os.path.expanduser(self.maven_path)
        self.log = logging.getLogger('')
        self.pom_file = PomFile(self.settings.get("pom_file", ""))

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
                self.maven_path = os.path.abspath(self.maven_path)
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

    def run_tests(self, artifacts_dir):
        """
        run tests
        1) generate pom
        2) save pom into artifacts
        3) execute maven tests
        """
        self.pom_file.generate_pom()
        self.pom_file.save(os.path.join(artifacts_dir, "pom.xml"))

        maven_out = open(os.path.join(artifacts_dir, "mvn_out"), 'wb')
        maven_err = open(os.path.join(artifacts_dir, "mvn_err"), 'wb')
        self.process = shell_exec(self.maven_path + " test -fn", cwd=artifacts_dir, stdout=maven_out, stderr=maven_err)

    def is_finished(self):
        """
        return
        :return: bool
        """

class PomFile():
    def __init__(self, existing_pom_file_path):
        self.pom_file_path = existing_pom_file_path
        self.xml_tree = None

    def generate_pom(self):
        """
        Generate POM from scratch or use and modify existing pom
        :return:
        """
        #  TODO implement xml modification
        base_pom = b"""<?xml version="1.0" encoding="UTF-8"?>
<project>
<modelVersion>4.0.0</modelVersion>
<groupId>test.test</groupId>
<artifactId>web-driver-jenkins-integration</artifactId>
<version>1.0.0-SNAPSHOT</version>
<build>
<directory>target</directory>
<outputDirectory>target/classes</outputDirectory>
<testOutputDirectory>target/test-classes</testOutputDirectory>
<sourceDirectory>selenium_scripts</sourceDirectory>
<scriptSourceDirectory>selenium_scripts</scriptSourceDirectory>
<testSourceDirectory>selenium_scripts</testSourceDirectory>
</build>
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
<reporting>
    <plugins>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-surefire-report-plugin</artifactId>
        <version>2.18.1</version>
        <configuration>
          <outputDirectory>${basedir}/surefire</outputDirectory>
        </configuration>
      </plugin>
    </plugins>
  </reporting>
</project>"""

        self.xml_tree = base_pom

        # if not self.pom_file_path:
        #    root = etree.Element(b"project")
        #    self.xml_tree = etree.Element(root)
        #    self.xml_tree.append(etree.fromstring(base_pom))
        #else:
        #    with open(self.pom_file_path, 'rb') as fds:
        #        self.xml_tree = etree.parse(fds)

        # self.modify_test_dir()

    def save(self, path):
        """
        save pom to file
        :param path:
        :return:
        """
        with open(path, "wb") as fds:
            fds.write(self.xml_tree)
            # fds.write(self.xml_tree.tostring, pretty_print=True, encoding="UTF-8", xml_declaration=True)

    def modify_test_dir(self):
        project_tag = self.xml_tree.find("project")
        build_tag = etree.Element("build")
        test_path = etree.Element("testSourceDirectory")
        test_path.text = "selenium_scripts"
        build_tag.append(test_path)
        project_tag.append(build_tag)


class JunitTester(AbstractTestRunner):
    def __init__(self, executor_settings):
        self.log = logging.getLogger("")


    def check_if_installed(self):
        try:
            subprocess.check_output(["mvn", "-v"], stderr=subprocess.STDOUT)  # check if installed globally
            return True
        except (OSError, CalledProcessError):
            try:
                subprocess.check_output([self.maven_path, "-v"], stderr=subprocess.STDOUT)
                self.maven_path = os.path.abspath(self.maven_path)
                return True
            except (OSError, CalledProcessError):
                return False

class NoseTester(AbstractTestRunner):
    """
    Python selenium tests runner
    """
    def __init__(self):
        self.log = logging.getLogger('')
        self.thread = None
        self.return_code = None

    def check_if_installed(self):
        """
        nose and selenium packages are required.
        """
        try:
            import nose
            import selenium
            self.log.info("nose and selenium already installed")
            return True
        except ImportError:
            self.log.info("missing nose or selenium packages")
            return False

    def install(self):
        """
        install nose, selenium from pip
        """
        try:
            import pip
            pip.main(['install', "nose"])
            pip.main(['install', "selenium"])
            self.log.info("nose and selenium packages were successfully installed")
        except BaseException as exc:
            self.log.debug("Error while installing additional packages: nose, selenium", traceback.format_exc())
            raise RuntimeError("Error while installing nose and selenium %s" % exc)

    def run_tests(self, artifacts_dir):
        """
        run python tests in separate thread,
        """
        self.thread = threading.Thread(target=self._run_in_thread, args=(artifacts_dir,))
        self.thread.start()

    def _run_in_thread(self, artifacts_dir):

        self.log.info("running in thread %s", artifacts_dir)
        try:
            import nose
            import selenium
            nose.run()
        except BaseException as exc:
            self.log.debug("failed", traceback.format_exc())
            raise RuntimeError("Nose failed: %s" % exc)

    def is_finished(self):
        return not self.thread.isAlive()




class SeleniumWidget(urwid.Pile):
    """
    Not implemented yet
    """
    def __init__(self, executor):
        self.executor = executor
        self.dur = executor.get_load().duration
        widgets = []
        self.elapsed = urwid.Text("Elapsed: N/A")
        self.eta = urwid.Text("ETA: N/A", align=urwid.RIGHT)
        widgets.append(urwid.Columns([self.elapsed, self.eta]))
        #if self.executor.script:
        #    self.script_name = urwid.Text("Script: %s" % os.path.basename(self.executor.script))
        #    widgets.append(self.script_name)
        super(SeleniumWidget, self).__init__(widgets)
    def update(self):
        if self.executor.start_time:
            elapsed = time.time() - self.executor.start_time
            self.elapsed.set_text("Elapsed: %s" % humanize_time(elapsed))
        self._invalidate()

class DumbReader(ResultsReader):
    def _read(self, last_pass=False):
        tstmp = 0
        label = "dummy label"
        concur = 0
        rtm = 0
        cnn = 0
        ltc = 0
        rcd = 0
        error = 0
        trname = ""

        yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname