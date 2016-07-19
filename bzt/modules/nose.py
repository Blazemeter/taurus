"""
Copyright 2016 BlazeMeter Inc.

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
import time

import urwid

from bzt.engine import ScenarioExecutor, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.jmeter import JTLReader
from bzt.utils import shutdown_process, get_full_path


class NoseExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Nose executor
    """

    def __init__(self):
        super(NoseExecutor, self).__init__()
        self.process = None
        self.start_time = None
        self.end_time = None
        self.widget = None
        self.reader = None
        self.kpi_file = None
        self.err_jtl = None
        self.nose_stdout = None
        self.nose_stderr = None
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "nose_func_plugin.py")
        self.test_dir = None

    def prepare(self):
        self.test_dir = self.execution.get("test-dir", ValueError("You must provide 'test-dir' field"))
        self.log.info("Test dir: %s", self.test_dir)

        self.kpi_file = self.engine.create_artifact("selenium_tests_report", ".csv")
        self.err_file = self.engine.create_artifact("selenium_tests_err", ".xml")
        self.nose_stdout = open(self.engine.create_artifact("nose", ".out"), "wt")
        self.nose_stderr = open(self.engine.create_artifact("nose", ".err"), "wt")

        self.reader = JTLReader(self.kpi_file, self.log, self.err_file)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        self.start_time = time.time()
        executable = self.execution.get("python", sys.executable)
        nose_command_line = [executable, self.plugin_path, '-k', self.kpi_file, '-e', self.err_file]
        nose_command_line += [self.test_dir]

        self.process = self.execute(nose_command_line, stdout=self.nose_stdout, stderr=self.nose_stderr)

    def get_widget(self):
        if self.widget is None:
            self.widget = NoseWidget(self.test_dir, self.nose_stdout.name)
        return self.widget

    def check(self):
        if self.widget:
            self.widget.update()
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        self.log.debug("nose exit code: %s", ret_code)
        if ret_code != 0:
            raise RuntimeError("nose exited with non-zero code")
        return True

    def report_test_duration(self):
        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium tests ran for %s seconds", self.end_time - self.start_time)

    def shutdown(self):
        shutdown_process(self.process, self.log)
        self.report_test_duration()

    def post_process(self):
        if self.nose_stdout:
            self.nose_stdout.close()
        if self.nose_stderr:
            self.nose_stderr.close()
        if self.reader and not self.reader.read_records:
            raise RuntimeWarning("Empty results, most likely Selenium failed")

    def resource_files(self):
        raise NotImplemented()


class NoseWidget(urwid.Pile, PrioritizedWidget):
    def __init__(self, label, runner_output):
        widgets = []
        self.script_name = urwid.Text("Tests: %s" % label)
        self.summary_stats = urwid.Text("")
        self.current_test = urwid.Text("")
        self.runner_output = runner_output
        widgets.append(self.script_name)
        widgets.append(self.summary_stats)
        widgets.append(self.current_test)
        super(NoseWidget, self).__init__(widgets)
        PrioritizedWidget.__init__(self, priority=10)

    def update(self):
        cur_test = reader_summary = "No data received yet"
        if os.path.exists(self.runner_output):
            with open(self.runner_output, "rt") as fds:
                lines = fds.readlines()
                if lines:
                    line = lines[-1]
                    if line and "," in line:
                        try:
                            cur_test, reader_summary = line.split(",")
                        except ValueError:
                            pass

        self.current_test.set_text(cur_test)
        self.summary_stats.set_text(reader_summary)
        self._invalidate()
