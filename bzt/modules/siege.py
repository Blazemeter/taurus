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
import os

from datetime import datetime

from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.utils import shell_exec, shutdown_process, BetterDict


class SiegeExecutor(ScenarioExecutor):
    def __init__(self):
        super(SiegeExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__out = None
        self.__err = None
        self.__rc_name = None
        self.__url_name = None
        self.reader = None

    def prepare(self):
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

        self.__url_name = self.engine.create_artifact("siege", "url")

        with open(self.__url_name, 'w') as url_file:
            url_list = self.get_scenario().get("requests", ["http://blazedemo.com"])
            url_list = [dic['url'] for dic in url_list]     # FIXME: read all info
            url_file.writelines('\n'.join(url_list))
            url_file.close()

        out_file_name = self.engine.create_artifact("siege", ".out")
        self.reader = DataLogReader(out_file_name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        self.__out = open(out_file_name, 'w')
        self.__err = open(self.engine.create_artifact("siege", ".err"), 'w')

    def startup(self):
        """
        Should start the tool as fast as possible.
        """
        args = [self.settings.get('path', 'siege')]
        load = self.get_load()
        args += ['--reps=%s' % load.iterations, '--concurrent=%s' % load.concurrency]
        self.reader.concurency = load.concurrency
        args += ['--file="%s"' % self.__url_name]
        env = BetterDict()
        env.merge({k: os.environ.get(k) for k in os.environ.keys()})
        env.merge({"SIEGERC": self.__rc_name})

        self.process = shell_exec(args, stdout=self.__out, stderr=self.__err, env=env)

    def check(self):
        retcode = self.process.poll()
        if retcode is None:
            return False
        if retcode != 0:
            raise RuntimeError("Siege tool exited with non-zero code")
        self.log.info("Siege tool exit code: %s", str(retcode))
        return True

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        shutdown_process(self.process, self.log)
        if self.__out and not self.__out.closed:
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()


class DataLogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(DataLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.concurrency = None

    def _calculate_datapoints(self, final_pass=False):
        for point in super(DataLogReader, self)._calculate_datapoints(final_pass):
            yield point

    def __open_fds(self):
        """
        opens siege.log
        """
        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet")
            return False

        if not os.path.getsize(self.filename):
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
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read
        for line in lines:
            l_start = line.index('m') + 1
            l_end = line.index(chr(0x1b), l_start)
            line = line[l_start:l_end]
            log_vals = [val.strip() for val in line.split(',')]

            # _mark = log_vals[0]           # 0. current test mark, defined by --mark key
            # _user_id = int(log_vals[1])   # 1. fake user id
            # _http = log_vals[2]           # 2. http protocol
            _rstatus = int(log_vals[2])     # 3. response status code
            _etime = float(log_vals[3])     # 4. elapsed time (total time - connection time)
            # _rsize = int(log_vals[5])     # 5. size of response
            _url = log_vals[5]              # 6. long or short URL value
            # _url_id = int(log_vals[7])    # 7. url number
            _tstamp = datetime.strptime(log_vals[7], "%Y-%m-%d %H:%M:%S")
            _tstamp = _tstamp.toordinal()   # 8. moment of request sending

            _con_time = 0
            _latency = 0
            _error = None
            _concur = self.concurrency

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, ''
