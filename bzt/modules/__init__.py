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
import abc
import os

from bzt import ToolError
from bzt.engine import FileLister, SelfDiagnosable, ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.functional import FunctionalAggregator, FuncSamplesReader, LoadSamplesReader
from bzt.utils import shutdown_process


class ReportableExecutor(ScenarioExecutor):
    def __init__(self):
        super(ReportableExecutor, self).__init__()
        self.report_file = None
        self.reported = True
        self.register_reader = True

    def create_func_reader(self, report_file):
        return FuncSamplesReader(report_file, self.engine, self.log)

    def create_load_reader(self, report_file):
        return LoadSamplesReader(report_file, self.log)

    def reporting_setup(self, prefix=None, suffix=None):
        if not self.reported:
            self.log.debug("Skipping reporting setup for executor %s", self)
            return

        report_file = self.execution.get("report-file")
        if not report_file:
            if not prefix:
                prefix = self.__class__.__name__
            if suffix is None:
                suffix = '.dat'
            report_file = self.engine.create_artifact(prefix, suffix)

        self.report_file = report_file.replace(os.path.sep, '/')

        if self.engine.is_functional_mode():
            self.reader = self.create_func_reader(self.report_file)
        else:
            self.reader = self.create_load_reader(self.report_file)

        if not self.register_reader:
            self.log.debug("Skipping reader registration for executor %s", self)
            return

        if isinstance(self.engine.aggregator, (ConsolidatingAggregator, FunctionalAggregator)):
            self.engine.aggregator.add_underling(self.reader)


class TransactionListener(object):
    @abc.abstractmethod
    def transaction_started(self, sender, label, start_time):
        pass

    @abc.abstractmethod
    def iteration_ended(self, sender, label, end_time):
        pass


class TransactionProvider(object):
    def __init__(self):
        self._listeners = []
        self._source = self

    def subscribe_to_transactions(self, listener):
        assert isinstance(listener, TransactionListener)
        self._listeners.append(listener)

    def set_source(self, source_obj):
        self._source = source_obj

    def transaction_started(self, label, start_time):
        for listener in self._listeners:
            listener.transaction_started(self._source, label, start_time)

    def transacion_ended(self, label, duration):
        for listener in self._listeners:
            listener.transaction_ended(self._source, label, duration)


class SubprocessedExecutor(ReportableExecutor, FileLister, SelfDiagnosable, WidgetProvider, TransactionProvider):
    """
    Class for subprocessed executors

    All executors must implement the following interface.
    """

    def __init__(self):
        super(SubprocessedExecutor, self).__init__()
        TransactionProvider.__init__(self)
        self.script = None
        self.process = None
        self.widget = None

    def prepare(self):
        super(SubprocessedExecutor, self).prepare()
        prefix = self.execution.get("executor", "executor")
        self.stdout = open(self.engine.create_artifact(prefix, ".out"), "wt")
        self.stderr = open(self.engine.create_artifact(prefix, ".err"), "wt")

    def _start_subprocess(self, cmdline, **kwargs):
        self.process = self._execute(cmdline, **kwargs)

    def resource_files(self):
        script = self.get_script_path()
        if script:
            return [script]
        else:
            return []

    def check(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                msg = "Test runner %s (%s) has failed with retcode %s"
                raise ToolError(msg % (self.label, self.__class__.__name__, ret_code), self.get_error_diagnostics())
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def _check_tools(self, tools):
        for tool in tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s...", tool.tool_name)
                tool.install()

    def has_results(self):
        return bool(self.reader) and bool(self.reader.read_records)

    def get_error_diagnostics(self):
        diagnostics = []
        class_name = self.__class__.__name__
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents:
                    diagnostics.append(class_name + " STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
                contents = fds.read().strip()
                if contents:
                    diagnostics.append(class_name + " STDERR:\n" + contents)
        return diagnostics

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :rtype: ExecutorWidget
        """
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, label)
        return self.widget
