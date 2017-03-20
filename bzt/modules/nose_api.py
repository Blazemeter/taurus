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
import os
import sys

from bzt import ToolError
from bzt.engine import ScenarioExecutor
from bzt.utils import get_full_path, shutdown_process
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.functional import FunctionalAggregator
from bzt.modules.selenium import FuncSamplesReader, LoadSamplesReader, SeleniumWidget


class NoseAPIExecutor(ScenarioExecutor):
    def __init__(self):
        super(NoseAPIExecutor, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2), "resources", "nose_plugin.py")
        self.process = None
        self.stdout_path = None
        self.stderr_path = None
        self.stdout_file = None
        self.stderr_file = None
        self.script = None
        self.report_path = None

    def prepare(self):
        self.script = self.get_script_path()
        self.stdout_path = self.engine.create_artifact("nose", ".out")
        self.stderr_path = self.engine.create_artifact("nose", ".err")
        self.report_path = self.engine.create_artifact("report", ".ldjson")

        if self.engine.is_functional_mode():
            self.reader = FuncSamplesReader(self.report_path, self.log, [])
            if isinstance(self.engine.aggregator, FunctionalAggregator):
                self.engine.aggregator.add_underling(self.reader)
        else:
            self.reader = LoadSamplesReader(self.report_path, self.log, [])
            if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        load = self.get_load()
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, '--report-file', self.report_path]

        if load.iterations:
            nose_command_line += ['-i', str(load.iterations)]

        if load.hold:
            nose_command_line += ['-d', str(load.hold)]

        nose_command_line += [self.script]

        self.stdout_file = open(self.stdout_path, "wt")
        self.stderr_file = open(self.stderr_path, "wt")
        self.process = self.execute(nose_command_line, stdout=self.stdout_file, stderr=self.stderr_file)

    def check(self):
        if self.widget:
            self.widget.update()

        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                with open(self.stderr_path) as fds:
                    std_err = fds.read()
                msg = "Nose %s (%s) has failed with retcode %s \n %s"
                raise ToolError(msg % (self.label, self.__class__.__name__, ret_code, std_err.strip()))
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)
        if self.stdout_file:
            self.stdout_file.close()
        if self.stderr_file:
            self.stderr_file.close()

    def post_process(self):
        pass

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.stdout_path)
        return self.widget
