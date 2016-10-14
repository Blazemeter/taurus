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
from os import path

from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.six import iteritems
from bzt.utils import shell_exec, shutdown_process, RequiredTool, dehumanize_time


class SiegeExecutor(ScenarioExecutor, WidgetProvider):
    def __init__(self):
        super(SiegeExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__out = None
        self.__err = None
        self.__rc_name = None
        self.__url_name = None
        self.tool_path = None
        self.scenario = None

    def prepare(self):
        self.scenario = self.get_scenario()
        self.tool_path = self._check_installed()

        config_params = ('verbose = true',
                         'csv = true',
                         'timestamp = false',
                         'fullurl = true',
                         'display-id = true',
                         'show-logfile = false',
                         'logging = false')

        self.__rc_name = self.engine.create_artifact("siegerc", "")
        with open(self.__rc_name, 'w') as rc_file:
            rc_file.writelines('\n'.join(config_params))
            rc_file.close()

        if Scenario.SCRIPT in self.scenario and self.scenario[Scenario.SCRIPT]:
            self.__url_name = self.engine.find_file(self.scenario[Scenario.SCRIPT])
            self.engine.existing_artifact(self.__url_name)
        elif 'requests' in self.scenario:
            self.__url_name = self._fill_url_file()
        else:
            raise ValueError("You must specify either script(url-file) or some requests for siege")

        out_file_name = self.engine.create_artifact("siege", ".out")
        self.reader = DataLogReader(out_file_name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        self.__out = open(out_file_name, 'w')
        self.__err = open(self.engine.create_artifact("siege", ".err"), 'w')

    def resource_files(self):
        resource_files = []
        if Scenario.SCRIPT in self.scenario and self.scenario[Scenario.SCRIPT]:
            resource_files.append(self.engine.find_file(self.scenario[Scenario.SCRIPT]))
        return resource_files

    def _fill_url_file(self):
        url_file_name = self.engine.create_artifact("siege", ".url")
        user_vars = self.scenario.get('variables')
        user_vars = ["%s=%s" % (key, val) for (key, val) in iteritems(user_vars)]

        with open(url_file_name, 'w') as url_file:
            url_list = list(self.scenario.get_requests())
            url_list = [req.url for req in url_list]
            url_file.writelines('\n'.join(user_vars + url_list))
            url_file.close()
        return url_file_name

    def startup(self):
        args = [self.tool_path]
        load = self.get_load()

        if load.iterations:
            args += ['--reps', str(load.iterations)]
        elif load.hold:
            hold_for = ceil(dehumanize_time(load.hold))
            args += ['--time', '%sS' % hold_for]
        else:
            raise ValueError("You must specify either 'hold-for' or 'iterations' for siege")

        if self.scenario.get('think-time'):
            think_time = dehumanize_time(self.scenario.get('think-time'))
            args += ['--delay', str(think_time)]
        else:
            args += ['--benchmark']

        load_concurrency = load.concurrency
        args += ['--concurrent', str(load_concurrency)]
        self.reader.concurrency = load_concurrency

        args += ['--file', self.__url_name]

        for key, val in iteritems(self.scenario.get_headers()):
            args += ['--header', "%s: %s" % (key, val)]

        env = {"SIEGERC": self.__rc_name}
        self.start_time = time.time()

        self.process = self.execute(args, stdout=self.__out, stderr=self.__err, env=env)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False

        self.log.info("Siege tool exit code: %s", ret_code)
        if ret_code != 0:
            raise RuntimeError("Siege tool exited with non-zero code")

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
        if self.__out and not self.__out.closed:  # FIXME: this should happen in post_process
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()

    def _check_installed(self):
        tool_path = self.settings.get('path', 'siege')
        siege = Siege(tool_path, self.log)
        if not siege.check_if_installed():
            raise RuntimeError("You must install Siege tool at first")
        return tool_path


class DataLogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(DataLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.concurrency = None

    def _calculate_datapoints(self, final_pass=False):  # FIXME: why override it?
        for point in super(DataLogReader, self)._calculate_datapoints(final_pass):
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
            _rsize = int(log_vals[4])     # 4. size of response
            _url = log_vals[5]  # 6. long or short URL value
            # _url_id = int(log_vals[7])    # 7. url number
            _tstamp = time.strptime(log_vals[7], "%Y-%m-%d %H:%M:%S")
            _tstamp = int(time.mktime(_tstamp))  # 8. moment of request sending

            _con_time = 0
            _latency = 0
            _error = None
            _concur = self.concurrency

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, '', _rsize

    def __del__(self):
        if self.fds:
            self.fds.close()


class Siege(RequiredTool):
    def __init__(self, tool_path, parent_logger):
        super(Siege, self).__init__("Siege", tool_path)
        self.tool_path = tool_path
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Check Siege: %s' % self.tool_path)
        try:
            shell_exec([self.tool_path, '-h'])
        except OSError:
            return False
        return True
