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
import multiprocessing
import os
import re
import shlex
from urllib.parse import quote

import requests

from bzt import TaurusConfigError
from bzt.engine import SETTINGS, Provisioning
from bzt.modules import ScenarioExecutor, RemoteProcessedExecutor
from bzt.modules.services import PythonTool
from bzt.utils import FileReader, RESOURCES_DIR, RequiredTool

IGNORED_LINE = re.compile(r"[^,]+,Total:\d+ Passed:\d+ Failed:\d+")


class RemotePyTestExecutor(RemoteProcessedExecutor):

    def __init__(self):
        super(RemotePyTestExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "pytest_runner.py")
        bridge_url = os.environ.get("WINDOWS_BRIDGE_URL", "")
        # Safely build URLs only if bridge_url is not empty
        if bridge_url:
            self.bridge_command_url = bridge_url.rstrip("/") + "/command"
            self.bridge_upload_url = bridge_url.rstrip("/") + "/upload?path="
            data = {
                "command": "where python"
            }
            response = requests.post(self.bridge_command_url, json=data)
            self.python_path = response.json()['output'].split('\n')[0].replace("\\", "/")
            self.report_file = "/tmp/external/RemotePyTestExecutor.ldjson"
            bridge_path = os.environ.get("WINDOWS_BRIDGE_TEST_PATH", "")
            if bridge_path:
                bridge_path = bridge_path.replace('\\', '/')
                self.external_folder_path = bridge_path
            else:
                self.external_folder_path = ""

            self.report_remote_path = self.external_folder_path + '/' + os.path.basename(self.report_file)
            self.runner_pid = 0
            self._tailer = FileReader('', file_opener=lambda _: None, parent_logger=self.log)
            self._additional_args = []

    def prepare(self):
        super(RemotePyTestExecutor, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for pytest executor")

        scenario = self.get_scenario()
        if "additional-args" in scenario:
            argv = scenario.get("additional-args")
            self._additional_args = shlex.split(argv)
        if self.report_file:
            self.reporting_remote_setup(self.report_file)

    def __is_verbose(self):
        engine_verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        executor_verbose = self.settings.get("verbose", engine_verbose)
        return executor_verbose

    def install_required_tools(self):
        """
        we need installed nose plugin
        """
        pass

    def get_load(self):
        raw_concurrency = str(self.get_raw_load().concurrency).lower()
        if raw_concurrency == 'auto':
            prov_type = self.engine.config.get(Provisioning.PROV)
            self.execution.get(ScenarioExecutor.CONCURR)[prov_type] = -1
        return super().get_load()

    def startup(self):
        """
        run python tests
        """
        # upload runner file
        with open(self.runner_path, "rb") as f:
            filename = os.path.basename(self.runner_path)
            runner_remote_path = self.external_folder_path + '/' + filename
            files = {"file": (filename, f, "text/plain")}
            encoded_runner_path = quote(runner_remote_path)
            requests.post(self.bridge_upload_url + encoded_runner_path, files=files)
        # upload test file
        with open(self.script, "rb") as f:
            filename = os.path.basename(self.script)
            files = {"file": (filename, f, "text/plain")}
            remote_test_path = self.external_folder_path + '/' + filename
            encoded_test_path = quote(remote_test_path)
            requests.post(self.bridge_upload_url + encoded_test_path, files=files)

        cmdline = [self.python_path, runner_remote_path, '--report-file', self.report_remote_path]
        load = self.get_load()
        if load.iterations:
            cmdline += ['-i', str(load.iterations)]

        if load.hold:
            cmdline += ['-d', str(load.hold)]

        raw_concurrency = self.get_raw_load().concurrency

        if raw_concurrency:
            if raw_concurrency == -1:
                raw_concurrency = 'auto'
            elif raw_concurrency > multiprocessing.cpu_count():
                raw_concurrency = multiprocessing.cpu_count()
                self.log.warning(f"pytest concurrency is limited to CPU count [{raw_concurrency}]")
            cmdline.extend(['-n', str(raw_concurrency)])

        cmdline += self._additional_args
        cmdline += [remote_test_path]
        data = {
            "command": ' '.join(cmdline).replace("/", "\\")
        }
        response = requests.post(self.bridge_command_url, json=data)
        self.runner_pid = response.json().get('pid')

    def check(self):
        self.__log_lines()
        if self.runner_pid != 0:
            data = {
                "command": 'tasklist /FI "PID eq ' + str(self.runner_pid) + '"'
            }
            response = requests.post(self.bridge_command_url, json=data)
            if response.status_code != 200:
                self.log.error("Failed to check remote pytest process status")
                return False
            if str(self.runner_pid) not in response.json().get('output'):
                return True
            else:
                return False
        return True

    def post_process(self):
        super(RemotePyTestExecutor, self).post_process()
        self.__log_lines()

    def __log_lines(self):
        lines = []
        for line in self._tailer.get_lines():
            if not IGNORED_LINE.match(line):
                lines.append(line)

        if lines:
            self.log.info("\n".join(lines))


class PyTest(PythonTool):
    PACKAGES = ["pytest", "pytest-xdist", "apiritif"]


class TaurusRemotePytestRunner(RequiredTool):
    def __init__(self, tool_path, **kwargs):
        super(TaurusRemotePytestRunner, self).__init__(tool_path=tool_path, installable=False, **kwargs)
