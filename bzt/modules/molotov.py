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
from distutils.version import LooseVersion

from math import ceil

from bzt import ToolError
from bzt.engine import ScenarioExecutor, HavingInstallableTools, SelfDiagnosable, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.utils import unicode_decode
from bzt.utils import shutdown_process, RequiredTool, dehumanize_time, get_full_path, LDJSONReader, CALL_PROBLEMS


class MolotovExecutor(ScenarioExecutor, FileLister, WidgetProvider, HavingInstallableTools, SelfDiagnosable):
    def __init__(self):
        super(MolotovExecutor, self).__init__()
        self.process = None
        self.report_file_name = None
        self.stdout = None
        self.stderr = None
        self.molotov = None
        self.scenario = None

    def prepare(self):
        super(MolotovExecutor, self).prepare()
        self.install_required_tools()

        self.stdout = open(self.engine.create_artifact("molotov", ".out"), 'w')
        self.stderr = open(self.engine.create_artifact("molotov", ".err"), 'w')

        self.report_file_name = self.engine.create_artifact("molotov-report", ".ldjson")
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

        cmdline = [self.molotov.tool_path]

        if load.concurrency is not None:
            cmdline += ['--workers', str(load.concurrency)]

        if 'processes' in self.execution:
            cmdline += ['--processes', str(self.execution['processes'])]

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

        think_time = self.get_scenario().get("think-time", None)
        if think_time:
            cmdline += ['--delay', str(dehumanize_time(think_time))]

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
            raise ToolError("molotov exited with non-zero code: %s" % ret_code, self.get_error_diagnostics())
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def install_required_tools(self):
        self.molotov = self._get_tool(Molotov, config=self.settings)
        if not self.molotov.check_if_installed():
            self.molotov.install()

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
    def __init__(self, config=None, **kwargs):
        settings = config or {}
        tool_path = settings.get('path', 'molotov')
        super(Molotov, self).__init__(tool_path=tool_path, installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug("Trying %s: %s", self.tool_name, self.tool_path)
        try:
            out, err = self.call([self.tool_path, "--version"])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        version = out.strip()
        if LooseVersion(version) < LooseVersion("1.4"):
            raise ToolError("You must install molotov>=1.4 to use this executor (version %s detected)" % version)
        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)

        return True


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
            elif row.get("type") == "request":
                label = unicode_decode(row["label"])
                tstmp = int(float(row["ts"]))
                rtm = float(row["elapsed"])
                rcd = row["responseCode"]
                error = None
                if int(rcd) >= 400:
                    error = row["responseMessage"]
                cnn = 0
                ltc = 0
                trname = ''
                byte_count = 0
                yield tstmp, label, self._concurrency, rtm, cnn, ltc, rcd, error, trname, byte_count
