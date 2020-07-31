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
import re
from math import ceil
from distutils.version import LooseVersion

from bzt import TaurusConfigError
from bzt.engine import ScenarioExecutor, HavingInstallableTools, SelfDiagnosable
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.requests_model import HTTPRequest
from bzt.utils import iteritems, CALL_PROBLEMS, shutdown_process, RequiredTool, dehumanize_time, FileReader


class ApacheBenchmarkExecutor(ScenarioExecutor, WidgetProvider, HavingInstallableTools, SelfDiagnosable):
    """
    Apache Benchmark executor module
    """

    def __init__(self):
        super(ApacheBenchmarkExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self._tsv_file = None
        self.tool = None
        self.scenario = None

    def prepare(self):
        super(ApacheBenchmarkExecutor, self).prepare()
        self.scenario = self.get_scenario()
        self.install_required_tools()

        self._tsv_file = self.engine.create_artifact("ab", ".tsv")

        self.stdout = open(self.engine.create_artifact("ab", ".out"), 'w')
        self.stderr = open(self.engine.create_artifact("ab", ".err"), 'w')

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
        args = [self.tool.tool_path]
        load = self.get_load()
        load_iterations = load.iterations or 1
        load_concurrency = load.concurrency or 1

        if load.hold:
            hold = int(ceil(dehumanize_time(load.hold)))
            args += ['-t', str(hold)]
        else:
            args += ['-n', str(load_iterations * load_concurrency)]  # ab waits for total number of iterations

        timeout = self.get_scenario().get("timeout", None)
        if timeout:
            args += ['-s', str(ceil(dehumanize_time(timeout)))]

        args += ['-c', str(load_concurrency)]
        args += ['-d']  # do not print 'Processed *00 requests' every 100 requests or so
        args += ['-r']  # do not crash on socket level errors

        if self.tool.version and LooseVersion(self.tool.version) >= LooseVersion("2.4.7"):
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
        for key, val in iteritems(request.headers):
            args += ['-H', "%s: %s" % (key, val)]

        if request.method != 'GET':
            raise TaurusConfigError("ab supports only GET requests, but '%s' is found" % request.method)

        if request.priority_option('keepalive', default=True):
            args += ['-k']

        args += [request.url]

        self.reader.setup(load_concurrency, request.label)

        self.process = self._execute(args)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            self.log.warning("ab tool exited with non-zero code: %s", ret_code)
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def install_required_tools(self):
        self.tool = self._get_tool(ApacheBenchmark, config=self.settings)
        if not self.tool.check_if_installed():
            self.tool.install()

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("ab STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
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
            _con_time = float(log_vals[2]) / 1000.0  # connection time
            _etime = float(log_vals[4]) / 1000.0  # elapsed time
            _latency = float(log_vals[5]) / 1000.0  # latency (aka waittime)
            _bytes = None

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, '', _bytes


class ApacheBenchmark(RequiredTool):
    def __init__(self, config=None, **kwargs):
        settings = config or {}
        tool_path = settings.get('path', 'ab')

        super(ApacheBenchmark, self).__init__(tool_path=tool_path, installable=False, **kwargs)

    def _get_version(self, output):
        version = re.findall("Version\s(\S+)\s", output)
        if not version:
            self.log.warning("%s tool version parsing error: %s", self.tool_name, output)
        else:
            return version[0]

    def check_if_installed(self):
        self.log.debug('Trying %s: %s', self.tool_name, self.tool_path)
        try:
            out, err = self.call([self.tool_path, '-V'])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        self.version = self._get_version(out)
        self.log.debug("%s check stdout: %s", self.tool_name, out)
        if err:
            self.log.warning("%s check stderr: %s", self.tool_name, err)
        return True
