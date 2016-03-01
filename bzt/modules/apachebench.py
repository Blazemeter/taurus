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
from os import path

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.six import iteritems
from bzt.utils import shell_exec, shutdown_process, RequiredTool


class ApacheBenchExecutor(ScenarioExecutor, WidgetProvider):
    """
    Apache Benchmark executor module
    """
    def __init__(self):
        super(ApacheBenchExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__tsv_file_name = None
        self.__out = None
        self.__err = None
        self.__rc_name = None
        self.__url_name = None
        self.tool_path = None
        self.scenario = None
        self.reader = None
        self.widget = None

    def prepare(self):
        self.scenario = self.get_scenario()
        self.tool_path = self._check_installed()

        self.__tsv_file_name = self.engine.create_artifact("apachebench", ".tsv")

        out_file_name = self.engine.create_artifact("apachebench", ".out")
        self.reader = TSVDataReader(self.__tsv_file_name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        self.__out = open(out_file_name, 'w')
        self.__err = open(self.engine.create_artifact("apachebench", ".err"), 'w')

    def startup(self):
        args = [self.tool_path]
        load = self.get_load()

        if load.iterations:
            args += ['-n', str(load.iterations)]
        else:
            raise ValueError("You must specify 'iterations' for apachebench")

        load_concurrency = load.concurrency
        args += ['-c', str(load_concurrency)]
        self.reader.concurrency = load_concurrency

        args += ['-d']  # do not emit 'Processed *00 requests' every 100 requests or so

        args += ['-g', str(self.__tsv_file_name)]  # dump stats to TSV file

        # verbosity level: 1-4
        args += ['-v', '3']

        for key, val in iteritems(self.scenario.get_headers()):
            args += ['-H', "%s: %s" % (key, val)]

        requests = list(self.scenario.get_requests())
        if not requests:
            raise ValueError("You must specify at least one request for apachebench")
        if len(requests) > 1:
            self.log.warning("ApacheBench doesn't support multiple requests."
                             " Only first one will be used.")
        args += [requests[0].url]

        self.process = shell_exec(args, stdout=self.__out, stderr=self.__err)

    def check(self):
        if self.widget:
            self.widget.update()

        ret_code = self.process.poll()
        if ret_code is None:
            return False
        self.log.info("ApacheBench tool exit code: %s", ret_code)
        if ret_code != 0:
            raise RuntimeError("ApacheBench tool exited with non-zero code")
        return True

    def get_widget(self):
        if not self.widget:
            if self.get_load().hold:
                label = "Apache Benchmark"
            else:
                label = None
            self.widget = SidebarWidget(self, label)
        return self.widget

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        shutdown_process(self.process, self.log)
        if self.__out and not self.__out.closed:
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()

    def _check_installed(self):
        tool_path = self.settings.get('path', 'ab')
        ab = ApacheBench(tool_path, self.log)
        if not ab.check_if_installed():
            raise RuntimeError("You must install ApacheBench tool at first")
        return tool_path


class TSVDataReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(TSVDataReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.concurrency = None

    def _calculate_datapoints(self, final_pass=False):
        for point in super(TSVDataReader, self)._calculate_datapoints(final_pass):
            yield point

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

    def _read(self, last_pass=False):
        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None
        if last_pass:
            lines = self.fds.readlines()  # unlimited
            self.fds.close()
        else:
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read    git

        for line in lines:
            log_vals = [val.strip() for val in line.split('\t')]

            if all(val.isalpha() for val in log_vals):
                continue

            _con_time = 0
            _latency = 0
            _error = None
            _concur = self.concurrency

            _tstamp = int(log_vals[1])   # timestamp - moment of request sending
            _etime = float(log_vals[5])  # elapsed time

            _url = None
            _rstatus = None

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, ''


class ApacheBench(RequiredTool):
    def __init__(self, tool_path, parent_logger):
        super(ApacheBench, self).__init__("ApacheBench", tool_path)
        self.tool_path = tool_path
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking ApacheBench: %s' % self.tool_path)
        try:
            shell_exec([self.tool_path, '-h'])
        except OSError:
            return False
        return True
