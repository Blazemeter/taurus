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
import subprocess
import sys

from bzt import TaurusConfigError
from bzt.engine import SETTINGS, Provisioning
from bzt.modules import ScenarioExecutor, RemoteExecutor
from bzt.modules.services import PythonTool
from bzt.utils import RESOURCES_DIR, RequiredTool

IGNORED_LINE = re.compile(r"[^,]+,Total:\d+ Passed:\d+ Failed:\d+")


class RemotePyTestExecutor(RemoteExecutor):

    def __init__(self):
        super(RemotePyTestExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "pytest_runner.py")
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
        self.remote_report_path = self.remote_artifacts_path + '/RemotePyTestExecutor.ldjson'
        self.reporting_setup(suffix=".ldjson")

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
        runner_remote_path = self.remote_artifacts_path + '/' + "pytest_runner.py"
        self.upload_file(self.runner_path, runner_remote_path)
        remote_test_path = self.remote_artifacts_path + '/' + os.path.basename(self.script)
        self.upload_file(self.script, remote_test_path)
        cmdline = ['python', runner_remote_path, '--report-file', self.remote_report_path]
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
        self.runner_pid = self.command(' '.join(cmdline).replace("/", "\\"), wait_for_completion=False,
                                       workingDir=self.remote_artifacts_path).get('pid')
        executable = self.settings.get("interpreter", sys.executable)
        cmd = [
            executable,
            "/tmp/bridge_file_puller.py",
            self.file_url,
            str(1024 * 1024),
            self.remote_report_path.replace('/', '\\'),
            '/tmp/artifacts/RemotePyTestExecutor.ldjson'
        ]
        # Detach child process using setsid (Linux/Unix)
        subprocess.Popen(cmd, start_new_session=True, close_fds=True)

    def post_process(self):
        super(RemotePyTestExecutor, self).post_process()


class PyTest(PythonTool):
    PACKAGES = ["pytest", "pytest-xdist", "apiritif"]


class TaurusRemotePytestRunner(RequiredTool):
    def __init__(self, tool_path, **kwargs):
        super(TaurusRemotePytestRunner, self).__init__(tool_path=tool_path, installable=False, **kwargs)
