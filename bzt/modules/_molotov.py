"""
Molotov-based executor for Taurus.

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
import shutil
from math import ceil

from bzt import ToolError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import ExecutorWidget
from bzt.modules.services import RequiredTool
from bzt.utils import unicode_decode
from bzt.utils import shutdown_process, dehumanize_time, get_full_path, LDJSONReader


class MolotovExecutor(ScenarioExecutor):
    def __init__(self):
        super(MolotovExecutor, self).__init__()
        self.process = None
        self.report_file_name = None
        self.stdout = None
        self.stderr = None
        self.molotov = None
        self.scenario = None
        self.script = None
        self.launch_cmdline = None
        self.user_tool_path = None

    def prepare(self):
        super(MolotovExecutor, self).prepare()
        self.install_required_tools()

        self.script = self.get_script_path()
        self.stdout = open(self.engine.create_artifact("molotov", ".out"), 'w')
        self.stderr = open(self.engine.create_artifact("molotov", ".err"), 'w')

        self.report_file_name = self.engine.create_artifact("molotov-report", ".ldjson")
        self.reader = MolotovReportReader(self.report_file_name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def install_required_tools(self):
        self.molotov = self._get_tool(Molotov, path=self.settings.get('path', None))
        if not self.molotov.check_if_installed():
            self.molotov.install()

    def get_widget(self):
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "Molotov: " + label.split('/')[1])
        return self.widget

    def startup(self):
        load = self.get_load()

        cmdline = [self.molotov.tool_path]

        if load.concurrency:
            cmdline += ['--workers', str(load.concurrency)]

        if 'processes' in self.execution:
            cmdline += ['--processes', str(self.execution['processes'])]

        if load.ramp_up:
            cmdline += ['--ramp-up', str(int(ceil(load.ramp_up)))]

        cmdline += ['--duration', str(int(ceil(load.hold)))]

        think_time = self.get_scenario().get("think-time", None)
        if think_time:
            cmdline += ['--delay', str(dehumanize_time(think_time))]

        user_cmd = self.settings.get("cmdline")
        if user_cmd:
            cmdline += user_cmd.split(" ")

        cmdline += ['--use-extension=bzt.resources.molotov_ext']

        cmdline += [self.get_script_path(required=True)]

        self.env.set({"MOLOTOV_TAURUS_REPORT": self.report_file_name})
        self.env.add_path({"PYTHONPATH": get_full_path(__file__, step_up=3)})

        self.process = self._execute(cmdline)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            raise ToolError("Molotov exited with non-zero code: %s" % ret_code, self.get_error_diagnostics())
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("molotov STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("molotov STDERR:\n" + contents)
        return diagnostics

    def resource_files(self):
        return [self.get_script_path(required=True)]


class Molotov(RequiredTool):
    def __init__(self, path=None, **kwargs):
        super(Molotov, self).__init__(installable=False, **kwargs)
        self.tool_path = path or shutil.which(self.tool_name.lower())


class MolotovReportReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(MolotovReportReader, self).__init__()
        self.is_distributed = False
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.ldjson_reader = LDJSONReader(filename, self.log)
        self.read_records = 0
        self._concurrency = 0

    def _read(self, final_pass=False):
        for row in self.ldjson_reader.read(final_pass):
            self.read_records += 1
            if row.get("type") == "workers":
                self._concurrency = row.get("value", self._concurrency)

            elif row.get("type") == "scenario_success":
                label = unicode_decode(row["name"])
                tstmp = int(float(row["ts"]))
                rtm = float(row["duration"])
                rcd = "200"
                error = None
                cnn = ltc = byte_count = 0
                trname = ''
                yield tstmp, label, self._concurrency, rtm, cnn, ltc, rcd, error, trname, byte_count

            elif row.get("type") == "scenario_failure":
                label = unicode_decode(row["name"])
                tstmp = int(float(row["ts"]))
                rtm = float(row["duration"])
                rcd = row["exception"]
                error = row["errorMessage"]
                cnn = ltc = byte_count = 0
                trname = ''
                yield tstmp, label, self._concurrency, rtm, cnn, ltc, rcd, error, trname, byte_count
