"""
Modules package holds EngineModule implementations

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
import os

from bzt import ToolError
from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.utils import shutdown_process
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.functional import FunctionalAggregator, FuncSamplesReader, LoadSamplesReader


class ReportableExecutor(ScenarioExecutor):
    def __init__(self):
        super(ReportableExecutor, self).__init__()
        self.report_file = None
        self.reported = True

    def reporting_setup(self, prefix=None, suffix=None, translation_table=None):
        if not self.reported:
            return

        if translation_table is None:
            translation_table = {}

        if "report-file" in self.execution:
            self.report_file = self.execution.get("report-file")
        else:
            if not prefix:
                prefix = self.__class__.__name__
            if suffix is None:
                suffix = '.dat'
            self.report_file = self.engine.create_artifact(prefix, suffix)

        self.report_file = self.report_file.replace(os.path.sep, '/')

        if self.engine.is_functional_mode():
            self.reader = FuncSamplesReader(self.report_file, self.engine, self.log, translation_table)
            if isinstance(self.engine.aggregator, FunctionalAggregator):
                self.engine.aggregator.add_underling(self.reader)
        else:
            self.reader = LoadSamplesReader(self.report_file, self.log, translation_table)
            if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                self.engine.aggregator.add_underling(self.reader)


class SubprocessedExecutor(ReportableExecutor, FileLister):
    """
    Class for subprocessed executors

    All executors must implement the following interface.
    """

    def __init__(self):
        super(SubprocessedExecutor, self).__init__()
        self.script = None
        self.env = {}
        self.process = None
        self.opened_descriptors = []
        self.stdout_file = None
        self.stderr_file = None

    def _start_subprocess(self, cmdline):
        prefix = self.execution.get("executor", None) or "executor"
        self.stdout_file = self.engine.create_artifact(prefix, ".out")
        std_out = open(self.stdout_file, "wt")
        self.opened_descriptors.append(std_out)
        self.stderr_file = self.engine.create_artifact(prefix, ".err")
        std_err = open(self.stderr_file, "wt")
        self.opened_descriptors.append(std_err)
        self.process = self.execute(cmdline, stdout=std_out, stderr=std_err, env=self.env)

    def resource_files(self):
        scenario = self.get_scenario()
        script = scenario.get(Scenario.SCRIPT, None)
        if script:
            return [script]
        else:
            return []

    def check(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                with open(self.stderr_file) as fds:
                    std_err = fds.read()
                msg = "Test runner %s (%s) has failed with retcode %s \n %s"
                raise ToolError(msg % (self.label, self.__class__.__name__, ret_code, std_err.strip()))
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)
        for desc in self.opened_descriptors:
            desc.close()
        self.opened_descriptors = []

    def _check_tools(self, tools):
        for tool in tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s...", tool.tool_name)
                tool.install()

    def has_results(self):
        return bool(self.reader) and bool(self.reader.read_records)
