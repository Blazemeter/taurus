"""
Module holds all stuff regarding usage of Apache Benchmark

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

import logging
import time
from math import ceil
from subprocess import CalledProcessError

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor, HavingInstallableTools, SelfDiagnosable
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.requests_model import HTTPRequest
from bzt.six import iteritems
from bzt.utils import shell_exec, shutdown_process, RequiredTool, dehumanize_time, FileReader


class ApacheBenchmarkExecutor(ScenarioExecutor, WidgetProvider, HavingInstallableTools, SelfDiagnosable):
    """
    Apache Benchmark executor module
    """

    def __init__(self):
        super(ApacheBenchmarkExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self._tsv_file = None
        self.stdout_file = None
        self.stderr_file = None
        self.tool_path = None
        self.scenario = None

    def prepare(self):
        self.scenario = self.get_scenario()
        self.tool_path = self.install_required_tools()

        self._tsv_file = self.engine.create_artifact("ab", ".tsv")

        out = self.engine.create_artifact("ab", ".out")
        err = self.engine.create_artifact("ab", ".err")
        self.stdout_file = open(out, 'w')
        self.stderr_file = open(err, 'w')

        self.reader = TSVDataReader(self._tsv_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :return:
        """
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "ab: " + label.split('/')[1])
        return self.widget

    def __first_http_request(self):
        for request in self.scenario.get_requests():
            if isinstance(request, HTTPRequest):
                return request
        return None

    def startup(self):
        args = [self.tool_path]
        load = self.get_load()
        load_iterations = load.iterations if load.iterations is not None else 1
        load_concurrency = load.concurrency if load.concurrency is not None else 1

        if load.hold:
            hold = int(ceil(dehumanize_time(load.hold)))
            args += ['-t', str(hold)]
        else:
            args += ['-n', str(load_iterations * load_concurrency)]  # ab waits for total number of iterations

        args += ['-c', str(load_concurrency)]
        args += ['-d']  # do not print 'Processed *00 requests' every 100 requests or so
        args += ['-r']  # do not crash on socket level errors
        args += ['-l']  # accept variable-len responses
        args += ['-g', str(self._tsv_file)]  # dump stats to TSV file

        # add global scenario headers
        for key, val in iteritems(self.scenario.get_headers()):
            args += ['-H', "%s: %s" % (key, val)]

        requests = self.scenario.get_requests()
        if not requests:
            raise TaurusConfigError("You must specify at least one request for ab")
        if len(requests) > 1:
            self.log.warning("ab doesn't support multiple requests. Only first one will be used.")
        request = self.__first_http_request()
        if request is None:
            raise TaurusConfigError("ab supports only HTTP requests, while scenario doesn't have any")

        # add request-specific headers
        for header in request.headers:
            for key, val in iteritems(header):
                args += ['-H', "%s: %s" % (key, val)]

        if request.method != 'GET':
            raise TaurusConfigError("ab supports only GET requests, but '%s' is found" % request.method)

        if request.priority_option('keepalive', default=True):
            args += ['-k']

        args += [request.url]

        self.reader.setup(load_concurrency, request.label)

        self.start_time = time.time()
        self.process = self.execute(args, stdout=self.stdout_file, stderr=self.stderr_file)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            self.log.warning("ab tool exited with non-zero code: %s", ret_code)
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.stdout_file and not self.stdout_file.closed:
            self.stdout_file.close()
        if self.stderr_file and not self.stderr_file.closed:
            self.stderr_file.close()

    def install_required_tools(self):
        tool_path = self.settings.get('path', 'ab')
        ab_tool = ApacheBenchmark(tool_path, self.log)
        if not ab_tool.check_if_installed():
            ab_tool.install()
        return tool_path

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout_file is not None:
            with open(self.stdout_file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("ab STDOUT:\n" + contents)
        if self.stderr_file is not None:
            with open(self.stderr_file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("ab STDERR:\n" + contents)
        return diagnostics


class TSVDataReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(TSVDataReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.skipped_header = False
        self.concurrency = None
        self.url_label = None

    def setup(self, concurrency, url_label):
        self.concurrency = concurrency
        self.url_label = url_label

        return True

    def _read(self, last_pass=False):
        lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

        for line in lines:
            if not self.skipped_header:
                self.skipped_header = True
                continue
            log_vals = [val.strip() for val in line.split('\t')]

            _error = None
            _rstatus = None

            _url = self.url_label
            _concur = self.concurrency
            _tstamp = int(log_vals[1])  # timestamp - moment of request sending
            _con_time = float(log_vals[2]) / 1000  # connection time
            _etime = float(log_vals[4]) / 1000  # elapsed time
            _latency = float(log_vals[5]) / 1000  # latency (aka waittime)
            _bytes = None

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, '', _bytes


class ApacheBenchmark(RequiredTool):
    def __init__(self, tool_path, parent_logger):
        super(ApacheBenchmark, self).__init__("ApacheBenchmark", tool_path)
        self.tool_path = tool_path
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking ApacheBenchmark: %s' % self.tool_path)
        try:
            shell_exec([self.tool_path, '-h'])
        except (CalledProcessError, OSError):
            return False
        return True

    def install(self):
        raise ToolError("You must install ab tool at first")
