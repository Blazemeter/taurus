"""
Module holds all stuff regarding Siege tool usage

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

import logging
import time
from math import ceil

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor, FileLister, HavingInstallableTools, SelfDiagnosable
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.requests_model import HTTPRequest
from bzt.utils import iteritems, CALL_PROBLEMS, shutdown_process, RequiredTool, dehumanize_time, FileReader


class SiegeExecutor(ScenarioExecutor, WidgetProvider, HavingInstallableTools, FileLister, SelfDiagnosable):
    def __init__(self):
        super(SiegeExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__rc_name = None
        self.__url_name = None
        self.tool = None
        self.scenario = None

    def prepare(self):
        super(SiegeExecutor, self).prepare()
        self.scenario = self.get_scenario()
        self.install_required_tools()

        self.__rc_name = self.execution.get("rc-file")
        if not self.__rc_name:
            config_params = ('verbose = true',
                             'csv = true',
                             'timestamp = false',
                             'fullurl = true',
                             'display-id = true',
                             'show-logfile = false',
                             'logging = false')
            self.__rc_name = self.engine.create_artifact("siegerc", "")
            with open(self.__rc_name, 'w') as rc_artifact_file:
                rc_artifact_file.writelines('\n'.join(config_params))

        self.__url_name = self.get_script_path()

        if self.__url_name:
            self.engine.existing_artifact(self.__url_name)
        elif 'requests' in self.scenario:
            self.__url_name = self._fill_url_file()
        else:
            raise TaurusConfigError("Siege: you must specify either script(url-file) or some requests")

        self.stdout = open(self.engine.create_artifact("siege", ".out"), 'w')
        self.stderr = open(self.engine.create_artifact("siege", ".err"), 'w')

        self.reader = DataLogReader(self.stdout.name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def resource_files(self):
        script = self.get_script_path()
        if script:
            return [script]
        else:
            return []

    def _fill_url_file(self):
        url_file_name = self.engine.create_artifact("siege", ".url")
        user_vars = self.scenario.get('variables')
        user_vars = ["%s=%s" % (key, val) for (key, val) in iteritems(user_vars)]

        url_list = []
        for req in self.scenario.get_requests():
            if not isinstance(req, HTTPRequest):
                msg = "Siege executor doesn't support '%s' blocks, skipping"
                self.log.warning(msg, req.NAME)
                continue
            url_list.append(req.url)

        with open(url_file_name, 'w') as url_file:
            url_file.writelines('\n'.join(user_vars + url_list))

        return url_file_name

    def startup(self):
        args = [self.tool.tool_path]
        load = self.get_load()

        if load.iterations:
            args += ['--reps', str(load.iterations)]
        elif load.hold:
            hold_for = ceil(dehumanize_time(load.hold))
            args += ['--time', '%sS' % hold_for]
        else:
            raise TaurusConfigError("Siege: You must specify either 'hold-for' or 'iterations'")

        think_time = self.scenario.get_think_time()
        if think_time:
            args += ['--delay', str(dehumanize_time(think_time))]
        else:
            args += ['--benchmark']

        load_concurrency = load.concurrency
        args += ['--concurrent', str(load_concurrency)]
        self.reader.concurrency = load_concurrency

        args += ['--file', self.__url_name]

        for key, val in iteritems(self.scenario.get_headers()):
            args += ['--header', "%s: %s" % (key, val)]

        self.env.set({"SIEGERC": self.__rc_name})
        self.process = self._execute(args)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            raise ToolError("Siege tool exited with non-zero code: %s" % ret_code, self.get_error_diagnostics())
        return True

    def get_widget(self):
        if not self.widget:
            if self.get_load().hold:
                label = "Siege Benchmark"
            else:
                label = None
            self.widget = ExecutorWidget(self, label)
        return self.widget

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        shutdown_process(self.process, self.log)

    def install_required_tools(self):
        self.tool = self._get_tool(Siege, config=self.settings)
        if not self.tool.check_if_installed():
            self.tool.install()

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents:
                    diagnostics.append("Siege STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
                contents = fds.read().strip()
                if contents:
                    diagnostics.append("Siege STDERR:\n" + contents)
        return diagnostics


class DataLogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(DataLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.concurrency = None

    def _read(self, last_pass=False):
        lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

        for line in lines:
            if line.count(chr(0x1b)) != 2:  # skip garbage
                continue
            l_start = line.index('m') + 1
            l_end = line.index(chr(0x1b), l_start)
            line = line[l_start:l_end]
            log_vals = [val.strip() for val in line.split(',')]

            # _mark = log_vals[0]           # 0. current test mark, defined by --mark key
            # _http = log_vals[1]           # 1. http protocol
            _rstatus = log_vals[2]  # 2. response status code
            _etime = float(log_vals[3])  # 3. elapsed time (total time - connection time)
            _rsize = int(log_vals[4])  # 4. size of response
            _url = log_vals[5]  # 6. long or short URL value
            # _url_id = int(log_vals[7])    # 7. url number
            _tstamp = time.strptime(log_vals[7], "%Y-%m-%d %H:%M:%S")
            _tstamp = int(time.mktime(_tstamp))  # 8. moment of request sending

            _con_time = 0
            _latency = 0
            _error = None
            _concur = self.concurrency

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, '', _rsize


class Siege(RequiredTool):
    def __init__(self, config=None, **kwargs):
        settings = config or {}
        tool_path = settings.get("path", "siege")
        super(Siege, self).__init__(tool_path=tool_path, installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug("Trying %s: %s", self.tool_name, self.tool_path)
        try:
            out, err = self.call([self.tool_path, "-h"])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)
        return True
