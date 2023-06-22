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
import copy
import os
import time
import requests
from abc import abstractmethod

from bzt import TaurusConfigError
from bzt.modules import ReportableExecutor
from bzt.modules.console import ExecutorWidget
from bzt.modules.services import PythonTool
from bzt.utils import get_files_recursive, get_full_path, RequiredTool, is_windows, is_mac, unzip, untar


class AbstractSeleniumExecutor(ReportableExecutor):
    @abstractmethod
    def subscribe_to_transactions(self, listener):
        pass


class SeleniumExecutor(ReportableExecutor):
    """
    Selenium executor
    :type runner: bzt.modules.SubprocessedExecutor
    """

    SUPPORTED_RUNNERS = ["apiritif", "junit", "testng", "rspec", "mocha", "nunit", "xunit", "pytest", "wdio", "robot"]
    SELENIUM_TOOLS_DIR = "~/.bzt/selenium-taurus/tools"

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.end_time = None
        self.runner = None
        self.script = None
        self.runner_working_dir = None
        self.register_reader = True
        self.webdrivers = []
        self.selenium = None

    def add_env(self, env):  # compatibility with taurus-cloud
        self.env.set(env)

    def get_runner_working_dir(self):
        if self.runner_working_dir is None:
            self.runner_working_dir = self.engine.create_artifact("classes", "")
        return self.runner_working_dir

    def subscribe_to_transactions(self, listener):
        self.runner.subscribe_to_transactions(listener)
        self.runner.set_source(self)

    def create_runner(self):
        runner_type = self.get_runner_type()
        self.runner = self.engine.instantiate_module(runner_type)
        self.runner.env = self.env
        self.runner.parameters = self.parameters
        self.runner.provisioning = self.provisioning
        self.runner.execution = copy.deepcopy(self.execution)
        self.runner.execution['files'] = self.execution.get('files', [], force_set=True)
        self.runner.execution['executor'] = runner_type
        self.runner.register_reader = self.register_reader
        self.runner.settings = copy.deepcopy(self.settings).merge(self.runner.settings)

        if runner_type == "apiritif":
            self.runner.execution["test-mode"] = "selenium"

    def get_virtual_display(self):
        pass  # for compatibility with taurus server

    def install_required_tools(self):
        self.selenium = self._get_tool(Selenium, engine=self.engine, settings=self.settings)
        self.webdrivers = [self._get_tool(ChromeDriver, settings=self.settings.get('chromedriver')),
                           self._get_tool(GeckoDriver, settings=self.settings.get('geckodriver'))]

        for tool in self.webdrivers + [self.selenium]:
            if not tool.check_if_installed():
                self.log.info("Installing %s %s...", tool.tool_name, tool.version)
                tool.install()

    def prepare(self):
        super(SeleniumExecutor, self).prepare()
        self.install_required_tools()
        for driver in self.webdrivers:
            self.env.add_path({"PATH": driver.get_dir()})

        self.create_runner()
        self.runner.prepare()
        self.script = self.runner.script

    def get_runner_type(self):
        runner = self.execution.get("runner")
        if runner:
            if runner in SeleniumExecutor.SUPPORTED_RUNNERS:
                self.log.debug("Using script type: %s", runner)
                return runner
            else:
                msg = "Runner '%s' is not supported. Supported runners: %s"
                raise TaurusConfigError(msg % (runner, SeleniumExecutor.SUPPORTED_RUNNERS))

        script_name = self.get_script_path()
        if script_name:
            return self.detect_script_type(script_name)
        else:
            if "requests" in self.get_scenario():
                return "apiritif"
            else:
                raise TaurusConfigError("You must specify either script or list of requests to run Selenium")

    def resource_files(self):
        self.create_runner()
        return self.runner.resource_files()

    def detect_script_type(self, script_name):
        if not os.path.exists(script_name):
            raise TaurusConfigError("Script '%s' doesn't exist" % script_name)

        file_types = set()

        # gather file extensions and choose script_type according to priority
        if os.path.isfile(script_name):  # regular file received
            file_types.add(os.path.splitext(script_name)[1].lower())
        else:  # dir received: check contained files
            for file_name in get_files_recursive(script_name):
                file_types.add(os.path.splitext(file_name)[1].lower())

        if '.java' in file_types or '.jar' in file_types:
            script_dir = get_full_path(self.get_script_path(), step_up=1)
            if self.execution.get('testng-xml') or os.path.exists(os.path.join(script_dir, 'testng.xml')):
                script_type = 'testng'
            else:
                script_type = 'junit'
        elif '.py' in file_types:
            script_type = 'apiritif'
        elif '.rb' in file_types:
            script_type = 'rspec'
        elif '.js' in file_types:
            script_type = 'mocha'
        elif '.dll' in file_types or '.exe' in file_types:
            script_type = 'nunit'
        else:
            if os.path.isfile(script_name):
                message = "Unsupported script type: %r" % script_name
            else:
                message = "Directory %r doesn't contain supported scripts" % script_name
            raise TaurusConfigError(message)

        self.log.debug("Detected script type: %s", script_type)

        return script_type

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        self.runner.startup()

    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            self.widget.update()

        return self.runner.check()

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
        self.runner.post_process()

        if os.path.exists("geckodriver.log"):
            self.engine.existing_artifact("geckodriver.log", True)

        self.selenium.post_process()
        super(SeleniumExecutor, self).post_process()

    def has_results(self):
        return self.runner.has_results()

    def get_widget(self):
        if not self.widget:
            label = ("%s" % self.runner).split('/')
            self.widget = ExecutorWidget(self, f"Selenium/{label[0].title()}: {label[1]}")
        return self.widget

    def get_error_diagnostics(self):
        diagnostics = []
        if self.runner:
            diagnostics.extend(self.runner.get_error_diagnostics())
        gecko_logs = ["geckodriver.log", os.path.join(self.engine.artifacts_dir, "geckodriver.log")]
        for possible_log in gecko_logs:
            if os.path.exists(possible_log):
                with open(possible_log) as fds:
                    diagnostics.append("Geckodriver log:\n" + fds.read())
        return diagnostics


class Selenium(PythonTool):
    PACKAGES = ["selenium"]
    VERSION = '4.1.3'

    def _get_version(self, output):
        return Selenium.VERSION


class WebDriver(RequiredTool):
    def __init__(self, settings, log=None, **kwargs):
        self.log = log
        base_dir = get_full_path(SeleniumExecutor.SELENIUM_TOOLS_DIR)

        # some parent logic is duplicated here to define appropriate tool_path in time
        driver_name = self.__class__.__name__.lower()
        file_name = driver_name + ('.exe' if is_windows() else "")

        version = settings.get("version")
        version = version or self._get_latest_version(driver_name)

        # try to use latest existed...
        drivers_dir = os.path.join(base_dir, 'drivers', driver_name)
        drivers_dir_content = os.path.exists(drivers_dir) and os.listdir(drivers_dir)
        if drivers_dir_content:
            drivers_dir_content.sort()
            version = version or drivers_dir_content[-1]

        version = version or self.VERSION
        version = str(version)  # let's fix reading version as number from yaml
        self.log.info(f'Used version of {driver_name} is {version}')

        tool_path = settings.get("path") or os.path.join(drivers_dir, version, file_name)
        download_link = settings.get('download-link')
        super().__init__(tool_path=tool_path, version=version, download_link=download_link, mandatory=False, **kwargs)
        self._expand_download_link()

    def get_dir(self):
        return get_full_path(self.tool_path, step_up=1)

    def _expand_download_link(self):
        pass

    def _get_latest_version(self, driver_name):
        try:
            latest_version = self._get_latest_version_from_inet()
            self.log.info(f'Latest stable version of {driver_name} is {latest_version}')
            return latest_version
        except BaseException as e:
            self.log.warning(f'Getting latest version is failed for {driver_name}: {e}')
            # return None if external request is impossible

    def _get_latest_version_from_inet(self):
        pass


class ChromeDriver(WebDriver):
    VERSION = "96.0.4664.45"
    DOWNLOAD_LINK = "https://chromedriver.storage.googleapis.com/{version}/chromedriver_{arch}.zip"

    def _get_latest_version_from_inet(self):
        return requests.get('https://chromedriver.storage.googleapis.com/LATEST_RELEASE').text

    def _expand_download_link(self):
        if is_windows():
            arch = 'win32'
        elif is_mac():
            arch = 'mac64'
        else:
            arch = 'linux64'

        self.download_link = self.download_link.format(version=self.version, arch=arch)

    def install(self):
        _dir = self.get_dir()
        if not os.path.exists(_dir):
            os.makedirs(_dir)

        dist = self._download(use_link=True)
        if dist:
            unzip(dist, _dir)
            os.remove(dist)

            if not is_windows():
                os.chmod(self.tool_path, 0o755)


class GeckoDriver(WebDriver):
    VERSION = "0.30.0"
    DOWNLOAD_LINK = \
        "https://github.com/mozilla/geckodriver/releases/download/v{version}/geckodriver-v{version}-{arch}.{ext}"

    def _get_latest_version_from_inet(self):
        return requests.get("https://api.github.com/repos/mozilla/geckodriver/releases/latest").json()["name"]

    def _expand_download_link(self):
        if is_windows():
            arch = 'win64'
            ext = 'zip'
        elif is_mac():
            arch = 'macos'
            ext = 'tar.gz'
        else:
            arch = 'linux64'
            ext = 'tar.gz'

        self.download_link = self.download_link.format(version=self.version, arch=arch, ext=ext)

    def install(self):
        _dir = self.get_dir()
        if not os.path.exists(_dir):
            os.makedirs(_dir)

        dist = self._download(use_link=True)
        if dist:
            if self.download_link.endswith('.zip'):
                self.log.info("Unzipping %s to %s", dist, _dir)
                unzip(dist, self.get_dir())
            else:
                self.log.info("Untaring %s to %s", dist, _dir)
                untar(dist, _dir)
            os.remove(dist)

            if not is_windows():
                os.chmod(self.tool_path, 0o755)
