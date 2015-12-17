"""
Module holds all stuff regarding Grinder tool usage

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

from bzt.engine import ScenarioExecutor
from bzt.utils import shell_exec, shutdown_process


class SiegeExecutor(ScenarioExecutor):
    def __init__(self):
        super(SiegeExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.log = None
        self.__out = None
        self.__err = None
        self.__log = None

    def startup(self):
        """
        Should start the tool as fast as possible.
        """
        load = self.get_load()
        data_dir = os.path.realpath(self.engine.artifacts_dir)
        args = ['siege', 'blazedemo.com']
        args += ['--reps=%s' % load.iterations, '--concurrent=%s' % load.concurrency]

        log_path = data_dir + '/siege.log'
        args += ['--log', log_path]

        self.__log = open(self.engine.create_artifact("siege", ".log"), 'w')

        self.__out = open(self.engine.create_artifact("siege", ".out"), 'w')
        self.__err = open(self.engine.create_artifact("siege", ".err"), 'w')

        self.process = shell_exec(args, stdout=self.__out, stderr=self.__err)

    def check(self):
        if self.process.poll() is None:
            return False
        else:
            return True

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        try:
            shutdown_process(self.process, self.log)
        finally:
            if self.__out:
                self.__out.close()
            if self.__err:
                self.__err.close()
            if self.__log:
                self.__log.close()
