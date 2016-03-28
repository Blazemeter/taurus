"""
Implementations for `Provisioning` classes

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

import time

from bzt.engine import Provisioning
from bzt.utils import dehumanize_time


class Local(Provisioning):
    """
    Local provisioning means we start all the tools locally
    """

    def prepare(self):
        """
        Call prepare on executors
        """
        super(Local, self).prepare()
        for executor in self.executors:
            self.log.debug("Preparing executor: %s", executor)
            executor.prepare()
            self.engine.prepared.append(executor)
            executor.delay = dehumanize_time(executor.execution.get('delay', '0'))

    def startup(self):
        self.start_time = time.time()

    def _start_modules(self):
        for executor in self.executors:
            if executor in self.engine.prepared and executor not in self.engine.started:
                if time.time() >= self.start_time + executor.delay:  # time to start executor
                    executor.startup()
                    self.engine.started.append(executor)

    def check(self):
        """
        Check executors for finish. Return True if all of them has finished.
        """
        finished = True

        self._start_modules()
        for executor in self.executors:
            if executor in self.engine.started:
                finished &= executor.check()
            else:
                finished = False

        return finished

    def shutdown(self):
        """
        Call shutdown on executors
        """
        for executor in self.executors:
            if executor in self.engine.started:
                self.log.debug("Shutdown %s", executor)
                executor.shutdown()

    def post_process(self):
        """
        Post-process executors
        """
        for executor in self.executors:
            if executor in self.engine.prepared:
                self.log.debug("Post-process %s", executor)
                executor.post_process()
