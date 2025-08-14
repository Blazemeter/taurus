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
from datetime import datetime
from urllib.parse import quote

import requests

from bzt import ToolError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.console import ExecutorWidget
from bzt.modules.functional import FunctionalAggregator, FuncSamplesReader, LoadSamplesReader, LinuxFuncSamplesReader, \
    LinuxLoadSamplesReader
from bzt.utils import shutdown_process


class ReportableExecutor(ScenarioExecutor):
    def __init__(self):
        super(ReportableExecutor, self).__init__()
        self.report_file = None
        self.reported = True
        self.register_reader = True

    def create_func_reader(self, report_file):
        return FuncSamplesReader(report_file, self.engine, self.log)

    def create_linux_func_reader(self, report_file):
        return LinuxFuncSamplesReader(report_file, self.engine, self.log)

    def create_load_reader(self, report_file):
        return LoadSamplesReader(report_file, self.log)

    def create_linux_load_reader(self, report_file):
        return LinuxLoadSamplesReader(report_file, self.log)

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

    def reporting_remote_setup(self, report_file):
        if not self.reported:
            self.log.debug("Skipping reporting setup for executor %s", self)
            return

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


class SubprocessedExecutor(ReportableExecutor, TransactionProvider):
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
            label = ("%s" % self).split('/')
            self.widget = ExecutorWidget(self, f"{label[0].title()}: {label[1]}")
        return self.widget


class RemoteExecutor(ReportableExecutor, TransactionProvider):
    """
    Class for remote executors

    All remote executors must implement the following interface.
    """

    def __init__(self):
        super(RemoteExecutor, self).__init__()
        TransactionProvider.__init__(self)
        self.script = None
        self.process = None
        self.widget = None

    def prepare(self):
        self.bridge_url = self.settings.get("bridge-url", os.environ.get("BRIDGE_URL", None))
        if not self.bridge_url:
            self.log.warning("Bridge URL is not set. Please set 'bridge-url' "
                             "in the executor settings or BRIDGE_URL environment variable.")
            pass
        self.bridge_command_url = self.bridge_url.rstrip("/") + "/command"
        self.bridge_upload_url = self.bridge_url.rstrip("/") + "/upload?path="
        self.file_url = self.bridge_url.rstrip("/") + "/file"
        self.bridge_os = self.settings.get("remote-os", os.environ.get("REMOTE_OS", 'windows')).lower()
        self.remote_root_path = self.settings.get("remote-root-path", os.environ.get("REMOTE_ROOT_PATH", None))
        if not self.remote_root_path:
            self.remote_root_path = self.command('echo %TEMP%')['output']
        if self.bridge_os == 'windows':
            self.remote_root_path = self.remote_root_path.replace('\\', '/')
        self.log.info('Remote root path: %s', self.remote_root_path)

        artifacts_dir = datetime.now().strftime("%Y-%m-%d-%Hh-%Mm-%Ss-") \
                        + str(datetime.now().microsecond // 1000).zfill(3) + "ms"
        self.remote_artifacts_path = os.path.join(self.remote_root_path, artifacts_dir)
        if self.bridge_os == 'windows':
            self.remote_artifacts_path = self.remote_artifacts_path.replace('\\', '/')
        self.log.info('Creating remote artifacts path: %s', self.remote_artifacts_path)
        self.command('mkdir -p "%s"' % self.remote_artifacts_path, wait_for_completion=True, use_shell=True)
        self.log.info('Remote artifacts path created')
        super(RemoteExecutor, self).prepare()

    def command(self, command, wait_for_completion=True, use_shell=False, workingDir=None):
        """
        Send command to bridge and return response
        :param command: dict
        :param wait_for_completion: bool
        :param use_shell: bool
        :return: dict
        """
        data = {
            "command": command,
            "waitForCompletion": wait_for_completion,
            "useShell": use_shell,
            "workingDir": workingDir
        }
        response = requests.post(self.bridge_command_url, json=data)
        if response.status_code != 200:
            raise ToolError(f"Bridge command failed with status code {response.status_code}: {response.text}")
        return response.json()

    def upload_file(self, file_path, remote_file_path):
        with open(file_path, "rb") as f:
            files = {"file": ('file', f, "text/plain")}
            encoded_test_path = quote(remote_file_path)
            requests.post(self.bridge_upload_url + encoded_test_path, files=files)


def resource_files(self):
    script = self.get_script_path()
    if script:
        return [script]
    else:
        return []


def check(self):
    pass


def shutdown(self):
    shutdown_process(self.process, self.log)


def _check_tools(self, tools):
    pass


def has_results(self):
    return bool(self.reader) and bool(self.reader.read_records)


def get_error_diagnostics(self):
    pass


def get_widget(self):
    """
    Add progress widget to console screen sidebar

    :rtype: ExecutorWidget
    """
    if not self.widget:
        label = ("%s" % self).split('/')
        self.widget = ExecutorWidget(self, f"{label[0].title()}: {label[1]}")
    return self.widget
