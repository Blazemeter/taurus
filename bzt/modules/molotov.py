"""
Molotov-based executor for Taurus. Python 3.5+ only.

Copyright 2017 BlazeMeter Inc.

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
import time
from distutils.version import LooseVersion

from math import ceil
from subprocess import CalledProcessError

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor, HavingInstallableTools, SelfDiagnosable, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.jmeter import IncrementalCSVReader
from bzt.six import communicate, unicode_decode
from bzt.utils import shell_exec, shutdown_process, RequiredTool, dehumanize_time


class MolotovExecutor(ScenarioExecutor, FileLister, WidgetProvider, HavingInstallableTools, SelfDiagnosable):
    def __init__(self):
        super(MolotovExecutor, self).__init__()
        self.process = None
        self.report_file_name = None
        self.stdout_file = None
        self.stderr_file = None
        self.tool_path = None
        self.scenario = None

    def _get_script(self):
        scenario = self.get_scenario()
        script = scenario.get("script", TaurusConfigError("You must provide script for Molotov"))
        return self.engine.find_file(script)

    def prepare(self):
        self.tool_path = self.install_required_tools()

        self.stdout_file = open(self.engine.create_artifact("molotov", ".out"), 'w')
        self.stderr_file = open(self.engine.create_artifact("molotov", ".err"), 'w')

        self.report_file_name = self.engine.create_artifact("molotov-report", ".csv")
        self.reader = MolotovReportReader(self.report_file_name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def get_widget(self):
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "Molotov: " + label.split('/')[1])
        return self.widget

    def startup(self):
        load = self.get_load()

        cmdline = [self.tool_path]

        if load.concurrency is not None:
            cmdline += ['--workers', str(load.concurrency)]
        # TODO: processes vs workers?
        # TODO: autosizing as `concurrency: auto`?

        duration = 0
        if load.ramp_up:
            ramp_up = int(ceil(dehumanize_time(load.hold)))
            duration += ramp_up
            cmdline += ['--ramp-up', str(ramp_up)]
        if load.hold:
            hold = int(ceil(dehumanize_time(load.hold)))
            duration += hold
        cmdline += ['--duration', str(duration)]

        cmdline += ['--use-extension=bzt.resources.molotov_ext']

        cmdline += [self._get_script()]

        env = {"MOLOTOV_TAURUS_REPORT": self.report_file_name}

        self.start_time = time.time()
        self.process = self.execute(cmdline, stdout=self.stdout_file, stderr=self.stderr_file, env=env)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            raise ToolError("molotov exited with non-zero code: %s" % ret_code, self.get_error_diagnostics())
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.stdout_file and not self.stdout_file.closed:
            self.stdout_file.close()
        if self.stderr_file and not self.stderr_file.closed:
            self.stderr_file.close()

    def install_required_tools(self):
        tool_path = self.settings.get('path', 'molotov')
        tool = Molotov(tool_path, self.log)
        if not tool.check_if_installed():
            tool.install()
        return tool_path

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout_file is not None:
            with open(self.stdout_file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("molotov STDOUT:\n" + contents)
        if self.stderr_file is not None:
            with open(self.stderr_file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("molotov STDERR:\n" + contents)
        return diagnostics

    def resource_files(self):
        return [self._get_script()]


class Molotov(RequiredTool):
    def __init__(self, tool_path, parent_logger):
        super(Molotov, self).__init__("Molotov", tool_path)
        self.tool_path = tool_path
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking Molotov: %s' % self.tool_path)
        try:
            stdout, stderr = communicate(shell_exec([self.tool_path, '--version']))
            self.log.debug("Molotov stdout/stderr: %s, %s", stdout, stderr)
            version_s = stdout.strip()
            version = LooseVersion(version_s)
            if version < LooseVersion("1.4"):
                self.log.warning("Molotov version %s detected" % version)
        except (CalledProcessError, OSError, AttributeError):
            return False
        return True

    def install(self):
        raise ToolError("You must install molotov tool (version 1.4 or greater) to use it")


class MolotovReportReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(MolotovReportReader, self).__init__()
        self.is_distributed = False
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.csvreader = IncrementalCSVReader(self.log, filename)
        self.read_records = 0

    def _read(self, last_pass=False):
        for row in self.csvreader.read(last_pass):
            label = unicode_decode(row["label"])

            rtm = float(row["elapsed"])
            rcd = row["responseCode"]
            error = None
            if int(rcd) >= 400:
                error = row["responseMessage"]

            tstmp = int(float(row["timestamp"]))
            self.read_records += 1
            concur = 0
            cnn = 0
            ltc = 0
            trname = ''
            byte_count = 0
            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count
