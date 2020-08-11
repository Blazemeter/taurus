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

import datetime
import sys
import time
import traceback

from bzt import ToolError
from bzt.engine import Provisioning, SelfDiagnosable
from bzt.utils import dehumanize_time, reraise


class Local(Provisioning):
    """
    Local provisioning means we start all the tools locally
    """

    def __init__(self):
        super(Local, self).__init__()
        self.extend_configs = True
        self.start_time = None
        self.available_slots = None
        self.finished_modules = []
        self.started_modules = []

    def _get_start_shift(self, shift):
        if not shift:
            return 0

        time_formats = ['%Y-%m-%d %H:%M:%S',
                        '%Y-%m-%d %H:%M',
                        '%H:%M:%S',
                        '%H:%M']

        for time_format in time_formats:
            try:
                date = datetime.datetime.strptime(shift, time_format)
            except ValueError:
                continue
            except TypeError:
                self.log.warning('Start time must be string type ("%s"), ignored "%s"', time_format[0], shift)
                break
            today = datetime.date.today()
            if today > date.date():
                date = datetime.datetime(today.year, today.month, today.day, date.hour, date.minute, date.second)
            return time.mktime(date.timetuple()) - self.start_time
        else:
            self.log.warning('Unrecognized time format: %s ("%s" required), ignored', shift, time_formats[0])

        return 0

    def prepare(self):
        super(Local, self).prepare()
        for executor in self.executors:
            self.log.debug("Preparing executor: %s", executor)
            executor.prepare()

    def startup(self):
        self.start_time = time.time()

        if self.settings.get("sequential", False):
            self.available_slots = 1
        else:
            self.available_slots = self.settings.get("capacity", None)
            if not self.available_slots:
                self.available_slots = sys.maxsize  # no limit

        for executor in self.executors:
            start_at = executor.execution.get('start-at', 0)
            start_shift = self._get_start_shift(start_at)
            delay = dehumanize_time(executor.execution.get('delay', 0))
            executor.delay = delay + start_shift
            msg = "Delay setup for %s: %s(start-at) + %s(delay) = %s"
            self.log.debug(msg, executor, start_shift, delay, executor.delay)

    def _start_modules(self):
        if self.available_slots:
            non_started_executors = [e for e in self.executors if e not in self.started_modules]
            for executor in non_started_executors:
                self.engine.logging_level_up()
                if time.time() >= self.start_time + executor.delay:
                    executor.startup()
                    self.started_modules.append(executor)
                    self.available_slots -= 1
                    msg = "Starting execution: %s, rest of available slots: %s"
                    self.log.debug(msg, executor, self.available_slots)
                    if not self.available_slots:
                        break

                self.engine.logging_level_down()

    def check(self):
        """
        Check executors for finish. Return True if all of them has finished.
        """
        finished = True

        self._start_modules()
        for executor in self.executors:
            if executor in self.finished_modules:
                continue

            if executor not in self.started_modules:
                finished = False
                continue

            if executor.check():
                self.finished_modules.append(executor)
                self.available_slots += 1
                self.log.debug("%s finished", executor)
            else:
                finished = False

        return finished

    def shutdown(self):
        """
        Call shutdown on executors
        """
        exc_info = exc_value = None
        for executor in self.started_modules:
            self.log.debug("Shutdown %s", executor)
            try:
                executor.shutdown()
            except BaseException as exc:
                msg = "Exception in shutdown of %s: %s %s"
                self.log.debug(msg, executor.__class__.__name__, exc, traceback.format_exc())
                if not exc_info:
                    exc_info = sys.exc_info()
                if not exc_value:
                    exc_value = exc
        if exc_info:
            reraise(exc_info, exc_value)

    def post_process(self):
        """
        Post-process executors
        """
        exc_info = exc_value = None
        for executor in self.executors:
            self.log.debug("Post-process %s", executor)
            try:
                executor.post_process()
                if executor in self.started_modules and not executor.has_results():
                    msg = "Empty results, most likely %s (%s) failed. " \
                          "Actual reason for this can be found in logs under %s"
                    message = msg % (executor.label, executor.__class__.__name__, self.engine.artifacts_dir)
                    diagnostics = None
                    if isinstance(executor, SelfDiagnosable):
                        diagnostics = executor.get_error_diagnostics()
                    raise ToolError(message, diagnostics)
            except BaseException as exc:
                msg = "Exception in post_process of %s: %s %s"
                self.log.debug(msg, executor.__class__.__name__, exc, traceback.format_exc())
                if not exc_info:
                    exc_info = sys.exc_info()
                if not exc_value:
                    exc_value = exc
        if exc_info:
            reraise(exc_info, exc_value)
