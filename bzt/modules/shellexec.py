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
import os
import platform
import subprocess
from subprocess import CalledProcessError

from bzt.utils import shutdown_process
from bzt.engine import EngineModule
from bzt.utils import ensure_is_dict


class ShellExecutor(EngineModule):
    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.prepare_tasks = []
        self.startup_tasks = []
        self.check_tasks = []
        self.shutdown_tasks = []
        self.postprocess_tasks = []

    def _load_tasks(self, stage, container):
        if not isinstance(self.parameters.get(stage, []), list):
            self.parameters[stage] = [self.parameters[stage]]

        for index, stage_task in enumerate(self.parameters[stage]):
            stage_task = ensure_is_dict(self.parameters[stage], index, "command")
            container.append(Task(self.parameters[stage][index], self.log, os.getcwd()))
            self.log.debug("Added task: %s, stage: %s", stage_task, stage)

    def prepare(self):
        """
        Configure Tasks
        :return:
        """
        self._load_tasks('prepare', self.prepare_tasks)
        self._load_tasks('startup', self.startup_tasks)
        self._load_tasks('check', self.check_tasks)
        self._load_tasks('shutdown', self.shutdown_tasks)
        self._load_tasks('post-process', self.postprocess_tasks)

        for task in self.prepare_tasks:
            task.start()

    def startup(self):
        for task in self.startup_tasks:
            task.start()

    def check(self):
        for task in self.check_tasks:
            task.start()

        for task in self.prepare_tasks + self.startup_tasks + self.check_tasks:
            task.check()

        return super(ShellExecutor, self).check()

    def shutdown(self):
        for task in self.shutdown_tasks:
            task.start()

        for task in self.check_tasks + self.startup_tasks:
            task.shutdown()

    def post_process(self):
        for task in self.shutdown_tasks + self.check_tasks + self.startup_tasks + self.prepare_tasks:
            task.shutdown()

        for task in self.postprocess_tasks:
            task.start()
            task.shutdown()


class Task(object):
    """
    :type process: subprocess.Popen
    """

    def __init__(self, config, parent_log, working_dir):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.working_dir = working_dir
        self.command = config.get("command", ValueError("Parameter is required: command"))
        self.is_background = config.get("background", False)
        self.ignore_failure = config.get("ignore-failure", False)
        self.err = config.get("err", subprocess.PIPE)
        self.out = config.get("out", subprocess.PIPE)
        self.process = None
        self.ret_code = None

    def start(self):
        """
        Start task
        :return:
        """
        if self.process:
            self.check()
            self.log.info("Process still running: %s", self)
            return

        kwargs = {
            'args': self.command,
            'stdout': open(self.out, 'wt') if self.out != subprocess.PIPE else self.out,
            'stderr': open(self.err, 'wt') if self.err != subprocess.PIPE else self.err,
            'cwd': self.working_dir,
            'shell': True
        }
        # FIXME: shouldn't we bother closing opened descriptors?
        if platform.system() != 'Windows':
            kwargs['preexec_fn'] = os.setpgrp
            kwargs['close_fds'] = True

        self.log.debug("Starting task: %s", self)
        self.process = subprocess.Popen(**kwargs)
        if self.is_background:
            self.log.debug("Task started, PID: %d", self.process.pid)
        else:
            self.process.wait()
            self.check()
            self.process = None

    def check(self):
        if not self.process or self.ret_code is not None:
            return

        self.ret_code = self.process.poll()
        if self.ret_code is None:
            self.log.debug('Task: %s is not finished yet', self)
            return False

        stdout, stderr = self.process.communicate()
        if stdout and (self.out == subprocess.PIPE):
            self.log.debug("Output for %s:\n%s", self, stdout)

        if stderr and (self.err == subprocess.PIPE):
            self.log.warning("Errors for %s:\n%s", self, stderr)

        self.log.debug("Task was finished with exit code %s: %s", self.ret_code, self)
        if not self.ignore_failure and self.ret_code != 0:
            if self.out != subprocess.PIPE:
                self.log.warning("Output for %s:\n%s", self, stdout)
            raise CalledProcessError(self.ret_code, self)
        return True

    def shutdown(self):
        """
        If task was not completed, kill process, provide output
        else provide output
        :return:
        """
        self.check()

        if self.ret_code is None:
            self.log.info("Background task was not completed, shutting it down: %s", self)
            shutdown_process(self.process, self.log)
        self.process = None

    def __repr__(self):
        return self.command
