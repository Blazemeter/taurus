#! /usr/bin/env python
"""
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

from bzt.utils import task_exec, shutdown_process
from bzt.engine import EngineModule
import os
from bzt import AutomatedShutdown
from shutil import move
from tempfile import NamedTemporaryFile
from bzt.utils import ensure_is_dict
from bzt.six import string_types


class ShellExecutor(EngineModule):
    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.tasks = {"prepare": [], "startup": [], "check": []}

    def prepare(self):
        """
        Configure Tasks
        :return:
        """
        for stage in self.tasks.keys():
            if self.parameters.get(stage, []) and isinstance(self.parameters.get(stage), string_types):  # ensure is list
                self.parameters[stage] = [self.parameters.get(stage)]

            for index, stage_task in enumerate(self.parameters[stage]):
                stage_task = ensure_is_dict(self.parameters[stage], index, "command")

                if stage == 'startup' and not stage_task.get('background', False):
                    self.log.error("Only background tasks are allowed on startup stage %s", stage_task.get("command"))
                    raise ValueError

                self.tasks[stage].append(Task(self.parameters[stage][index], self.log, self.engine.artifacts_dir))
                self.log.debug("Added task: %s, stage: %s", stage_task, stage)

        self._start_tasks("prepare")
        self._check_background_tasks()

    def startup(self):
        self._start_tasks("startup")
        self._check_background_tasks()

    def check(self):

        self._start_tasks("check")
        self._check_background_tasks()
        return super(ShellExecutor, self).check()

    def shutdown(self):
        self._check_background_tasks()
        self._shutdown_background_tasks("check")
        self._shutdown_background_tasks("startup")

    def post_process(self):
        self._check_background_tasks()
        self._shutdown_background_tasks("prepare")

    def _start_tasks(self, cur_stage):
        if cur_stage != "check":
            for task in self.tasks[cur_stage]:
                task.start()
        else:
            for task in self.tasks[cur_stage]:
                if not task.is_background:
                    task.start()
                else:
                    if not task.process:
                        task.start()
                        continue
                    if task.process and task.is_finished():
                        task.shutdown()
                        task.start()
                    else:
                        self.log.warning("This task is already running: %s", task)

    def _check_background_tasks(self):
        """
        check all background tasks if task was finished
        :return:
        """
        for stage in self.tasks.keys():
            for task in self.tasks[stage]:
                if task.is_background and task.process and task.is_finished():
                    task.shutdown()

    def _shutdown_background_tasks(self, target_stage):
        self.log.debug("Shutting down tasks, stage: %s", target_stage)
        for task in self.tasks[target_stage]:
            if task.is_background and task.process:
                task.shutdown()


class Task(object):
    def __init__(self, config, parent_log, working_dir):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.working_dir = working_dir
        self.command = config.get("command")
        self.is_background = config.get("background", False)
        self.ignore_failure = config.get("ignore-failure", True)
        self.out_file = config.get("out", None)
        self.err_file = config.get("err", None)
        self.stderr = None
        self.stdout = None
        self.process = None
        self.failed = False

    def start(self):
        """
        Start task
        :return:
        """
        self.log.debug("Starting task: %s", self)
        with NamedTemporaryFile(delete=False) as self.stdout, NamedTemporaryFile(delete=False) as self.stderr:
            self.process = task_exec(self.command, cwd=self.working_dir, stdout=self.stdout, stderr=self.stderr)
        self.log.debug("Task started: %s, PID: %d", self, self.process.pid)

        if not self.is_background:  # wait for completion if not background
            self.process.wait()
            self.shutdown()  # provide output

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                self.failed = True
            self.log.debug("Task: %s was finished with exit code: %s", self, ret_code)
            return True
        self.log.debug('Task: %s was not finished yet', self)
        return False

    def shutdown(self):
        """
        If task was not completed, kill process, provide output
        else provide output
        :return:
        """
        if not self.is_finished():
            self.log.info("Background task %s was not completed, shutting it down", self)
            shutdown_process(self.process, self.log)
        self.process = None
        with open(self.stderr.name) as fds_stderr, open(self.stdout.name) as fds_stdout:
            out = fds_stdout.read()
            err = fds_stderr.read()

        if out:
            self.log.info("Task %s stdout:\n %s", self, out)
        if err:
            self.log.error("Task %s stderr:\n %s", self, err)

        if self.out_file:
            if not os.path.dirname(self.out_file):
                self.out_file = os.path.join(self.working_dir, self.out_file)
            move(self.stdout.name, self.out_file)
        else:
            os.remove(self.stdout.name)

        if self.err_file:
            if not os.path.dirname(self.err_file):
                self.err_file = os.path.join(self.working_dir, self.err_file)
            move(self.stderr.name, self.err_file)
        else:
            os.remove(self.stderr.name)

        if self.failed and not self.ignore_failure:
            self.log.error("Task %s failed with ignore-failure = False, terminating", self)
            raise AutomatedShutdown

    def __repr__(self):
        return self.command
