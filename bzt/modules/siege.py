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

import logging

from bzt.engine import ScenarioExecutor
from bzt.utils import shell_exec, shutdown_process


class SiegeExecutor(ScenarioExecutor):
    def __init__(self):
        super(SiegeExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.log.warning("init!")
        self.process = None
        self.log = None

    def startup(self):
        """
        Should start the tool as fast as possible.
        """
        self.log.warning("startup!")
        self.process = shell_exec('siege blazedemo.com --log=/tmp/sige.log -c2 -b -r3 > siege.out 2> siege.err')

    def check(self):
        self.log.warning("check!")
        if self.process.poll():
            return False
        else:
            return True

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        self.log.warning("shutdown!")
        shutdown_process(self.process, self.log)
