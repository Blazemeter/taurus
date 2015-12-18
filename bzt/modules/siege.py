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

import os
import logging
import time
import datetime
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.engine import ScenarioExecutor
from bzt.utils import shell_exec, shutdown_process, BetterDict


class SiegeExecutor(ScenarioExecutor):
    def __init__(self):
        super(SiegeExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.log = None
        self.__out = None
        self.__err = None
        self.__rc = None
        self.reader = None

    def prepare(self):

        self.__out = self.engine.create_artifact("siege", ".out")
        self.__err = self.engine.create_artifact("siege", ".err")
        self.__rc = os.path.join(self.engine.artifacts_dir, 'siegerc')
        config_params = ('verbose = true',
                         'csv = true',
                         'timestamp = true',
                         'fullurl = false',
                         'display-id = true',
                         'show-logfile = true',
                         'logging = false')
        with open(self.__rc) as rc_file:
            rc_file.writelines([line + '\n' for line in config_params])
            rc_file.close()
        self.reader = DataLogReader(self.__out.name, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        """
        Should start the tool as fast as possible.
        """
        args = [self.settings.get('path', 'siege'), 'blazedemo.com']

        load = self.get_load()
        args += ['--reps=%s' % load.iterations, '--concurrent=%s' % load.concurrency]
        env = BetterDict()
        env.merge({k: os.environ.get(k) for k in os.environ.keys()})
        env.merge({"SIEGERC": self.__rc})

        self.process = shell_exec(args, stdout=self.__out, stderr=self.__err, env=env)

    def check(self):
        if self.process.poll() is None:
            return False
        else:
            return True

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        shutdown_process(self.process, self.log)


class DataLogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(DataLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None

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
            _log_vals = [val.strip() for val in line.strip().split(',')]
            t_stamp = time.mktime(datetime.datetime.strptime(_log_vals[1][:19], "%Y-%m-%d %H:%M:%S").timetuple())
            label = ""
            r_code = int(_log_vals[3])
            latency = con_time = 0
            r_time = float(_log_vals[4])
            error = None
            concur = None

            self.log.debug("_log_vals = %s" % str(_log_vals))

            yield t_stamp, label, concur, r_time, con_time, latency, r_code, error, ''
