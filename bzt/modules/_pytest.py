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
import sys

from bzt import TaurusConfigError
from bzt.engine import SETTINGS, Provisioning
from bzt.modules import SubprocessedExecutor, ScenarioExecutor
from bzt.modules.services import PythonTool
from bzt.utils import FileReader, RESOURCES_DIR, RequiredTool

IGNORED_LINE = re.compile(r"[^,]+,Total:\d+ Passed:\d+ Failed:\d+")


class PyTestExecutor(SubprocessedExecutor):
    def __init__(self):
        super(PyTestExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "pytest_runner.py")
        self._tailer = FileReader('', file_opener=lambda _: None, parent_logger=self.log)
        self._additional_args = []
        self.pytest = None

    def prepare(self):
        super(PyTestExecutor, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for pytest executor")

        scenario = self.get_scenario()
        if "additional-args" in scenario:
            argv = scenario.get("additional-args")
            self._additional_args = shlex.split(argv)

        self.reporting_setup(suffix=".ldjson")

    def __is_verbose(self):
        engine_verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        executor_verbose = self.settings.get("verbose", engine_verbose)
        return executor_verbose

    def install_required_tools(self):
        """
        we need installed nose plugin
        """
        self.pytest = self._get_tool(PyTest, engine=self.engine, settings=self.settings)
        self._check_tools([self.pytest, self._get_tool(TaurusPytestRunner, tool_path=self.runner_path)])

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
        executable = self.settings.get("interpreter", sys.executable)

        cmdline = [executable, self.runner_path, '--report-file', self.report_file]

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
        cmdline += [self.script]

        self.process = self._execute(cmdline)

        if self.__is_verbose():
            self._tailer = FileReader(filename=self.stdout.name, parent_logger=self.log)

    def check(self):
        self.__log_lines()
        return super(PyTestExecutor, self).check()

    def post_process(self):
        self.pytest.post_process()
        super(PyTestExecutor, self).post_process()
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


class TaurusPytestRunner(RequiredTool):
    def __init__(self, tool_path, **kwargs):
        super(TaurusPytestRunner, self).__init__(tool_path=tool_path, installable=False, **kwargs)
