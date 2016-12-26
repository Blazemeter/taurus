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
from os import path

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.services import HavingInstallableTools
from bzt.six import iteritems
from bzt.utils import shell_exec, shutdown_process, RequiredTool, dehumanize_time


class ApacheBenchmarkExecutor(ScenarioExecutor, WidgetProvider, HavingInstallableTools):
    """
    Apache Benchmark executor module
    """

    def __init__(self):
        super(ApacheBenchmarkExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__tsv_file_name = None
        self.__out = None
        self.__err = None
        self.tool_path = None
        self.scenario = None

    def prepare(self):
        self.scenario = self.get_scenario()
        self.tool_path = self.install_required_tools()

        self.__tsv_file_name = self.engine.create_artifact("ab", ".tsv")

        out_file_name = self.engine.create_artifact("ab", ".out")
        self.reader = TSVDataReader(self.__tsv_file_name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        self.__out = open(out_file_name, 'w')
        self.__err = open(self.engine.create_artifact("ab", ".err"), 'w')

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :return:
        """
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "ab: " + label.split('/')[1])
        return self.widget

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
        args += ['-g', str(self.__tsv_file_name)]  # dump stats to TSV file

        # add global scenario headers
        for key, val in iteritems(self.scenario.get_headers()):
            args += ['-H', "%s: %s" % (key, val)]

        requests = list(self.scenario.get_requests())
        if not requests:
            raise TaurusConfigError("You must specify at least one request for ab")
        if len(requests) > 1:
            self.log.warning("ab doesn't support multiple requests. Only first one will be used.")
        request = requests[0]

        # add request-specific headers
        for header in request.headers:
            for key, val in iteritems(header):
                args += ['-H', "%s: %s" % (key, val)]

        if request.method != 'GET':
            raise TaurusConfigError("ab supports only GET requests, but '%s' is found" % request.method)

        keepalive = True
        if request.config.get('keepalive') is not None:
            keepalive = request.config.get('keepalive')
        elif self.scenario.get('keepalive') is not None:
            keepalive = self.scenario.get('keepalive')
        if keepalive:
            args += ['-k']

        args += [request.url]

        self.reader.setup(load_concurrency, request.label)

        self.start_time = time.time()
        self.process = self.execute(args, stdout=self.__out, stderr=self.__err)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            raise ToolError("ab tool exited with non-zero code: %s" % ret_code)
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.__out and not self.__out.closed:
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()

    def install_required_tools(self):
        tool_path = self.settings.get('path', 'ab')
        ab_tool = ApacheBenchmark(tool_path, self.log)
        if not ab_tool.check_if_installed():
            ab_tool.install()
        return tool_path


class TSVDataReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(TSVDataReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.skipped_header = False
        self.concurrency = None
        self.url_label = None

    def setup(self, concurrency, url_label):
        self.concurrency = concurrency
        self.url_label = url_label

    def __open_fds(self):
        if not path.isfile(self.filename):
            self.log.debug("File not appeared yet")
            return False

        if not path.getsize(self.filename):
            self.log.debug("File is empty: %s", self.filename)
            return False

        if not self.fds:
            self.fds = open(self.filename)

        return True

    def __del__(self):
        if self.fds:
            self.fds.close()

    def _read(self, last_pass=False):
        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None
        if last_pass:
            lines = self.fds.readlines()
            self.fds.close()
        else:
            lines = self.fds.readlines(1024 * 1024)

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
        except OSError:
            return False
        return True

    def install(self):
        raise ToolError("You must install ab tool at first")
