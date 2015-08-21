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

from bzt.utils import shell_exec, shutdown_process, ensure_is_dict, BetterDict
from bzt.engine import EngineModule
import time
import tempfile
import shutil
import os
from bzt import AutomatedShutdown


class ShellExecutor(EngineModule):
    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.task_list = []

    def prepare(self):
        """
        Configure Tasks
        :return:
        """
        tasks = self.settings
        for task_config in tasks:
            try:
                self.task_list.append(Task(task_config, self.log, self.engine.artifacts_dir))
                self.log.debug("Added task: %s", str(task_config))  # RecordingHandler fails if not str()
            except ValueError:
                self.log.warning("Ignoring wrong task config: %s", str(task_config))
        self.process_stage("prepare")

    def startup(self):
        """
        :return:
        """
        self.process_stage("startup")

    def check(self):
        """
        If any tasks in check stage -
        :return:
        """
        self.process_stage("check")
        # self._remove_completed_tasks()
        return True

    def shutdown(self):
        """
        should forcefully kill all tasks at end
        :return:
        """
        self.process_stage("shutdown")
        if self.task_list:
            self.log.warning("Some non-blocking tasks were not completed before shutdown!")

    def _start_tasks(self, cur_stage):
        self.log.debug("Stage: %s, starting tasks...", cur_stage)
        for task in self.task_list:
            if task.config.get("start-stage") == cur_stage:
                try:
                    task.startup()
                except BaseException as exc:  # FIXME: Should we just ignore failed task? Or it's better to fail on prepare stage and ignore on others?
                    self.log.error("Exception while starting task: %s", exc)
                    self.log.error("Removing task: %s from task list!", task)
                    self.task_list.remove(task)
        self.log.debug("Tasks started (if any)")

    def _shutdown_tasks(self, cur_stage):
        self.log.debug("Shutting down tasks on stage: %s", cur_stage)
        for task in self.task_list:
            if task.config.get("stop-stage") == cur_stage:
                self.log.debug("Attempting to shutdown task: %s, stage: %s, PID: %d", task, cur_stage, task.process.pid)
                task.shutdown()  # TODO: implement graceful shutdown with check and timeout
                self.log.debug("Removing task %s from task list %s", task, self.task_list)
                self.task_list.remove(task)
                self.log.debug("Modified task list: %s", self.task_list)
        self.log.debug("Tasks shutdown completed (if any)")

    def _wait_blocking_tasks_to_complete(self, cur_stage):
        self.log.debug("Waiting for blocking tasks (if any)")
        for task in self.task_list:
            if task.config.get("start-stage") == cur_stage and task.config.get("block"):
                while not task.is_finished():
                    self.log.debug("Waiting for blocking task: %s, stage: %s...", task, cur_stage)
                    time.sleep(1)

    def process_stage(self, cur_stage):
        self.log.debug("Processing tasks, stage: %s", cur_stage)
        self._start_tasks(cur_stage)
        self._wait_blocking_tasks_to_complete(cur_stage)
        self._shutdown_tasks(cur_stage)


class Task(object):
    def __init__(self, config, parent_log, working_dir):
        self.log = None
        self.config = config
        self.process = None
        self.working_dir = working_dir
        self.stdout = None
        self.stderr = None
        self.is_failed = False
        self.log = parent_log.getChild(self.__class__.__name__)
        self.prepare()

    def prepare(self):
        """
        Parse config, apply config, merge with defaults
        prepare  blocking/nonblocking
        startup  nonblocking only
        check    blocking/nonblocking
        postprocess blocking/nonblocking
        shutdown blocking/nonblocking
        :return:
        """

        stages = ["prepare", "startup", "check", "postprocess", "shutdown"]
        possible_keys = ["start-stage", "stop-stage", "block", "stop-on-fail", "label", "command", "out", "err"]

        # check keys in config
        for key in self.config.keys():
            if key not in possible_keys:
                self.log.warning("Ignoring unknown option %s in task config! %s", key, self.config)

        default_config = {"start-stage": "prepare", "stop-stage": "shutdown", "block": False, "stop-on-fail": False}
        default_config.update(self.config)
        self.config = default_config

        if self.config["start-stage"] not in stages or self.config["stop-stage"] not in stages:
            self.log.error("Invalid stage name in task config!")
            raise ValueError

        if self.config["block"] not in [True, False] or self.config["stop-on-fail"] not in [True, False]:
            raise ValueError

        if not self.config.get("command"):
            self.log.error("No command in task config!")
            raise ValueError

        if self.config["start-stage"] == "startup" and self.config["block"]:
            self.log.error("Blocking tasks are not allowed on startup stage!")
            raise ValueError

    def startup(self):
        """
        Run task
        :return:
        """
        task_cmd = self.config.get("command")

        self.log.debug("Starting task: %s", task_cmd)

        prefix_name = "task_" + str(self.__hash__())

        with tempfile.NamedTemporaryFile(prefix=prefix_name,
                                         suffix=".out",
                                         delete=False,
                                         dir=self.working_dir) as self.stdout, \
                tempfile.NamedTemporaryFile(prefix=prefix_name,
                                            suffix=".err",
                                            delete=False,
                                            dir=self.working_dir) as self.stderr:
            self.process = shell_exec(task_cmd, cwd=self.working_dir, stdout=self.stdout, stderr=self.stderr,
                                      shell=False)
        self.log.debug("Task started %s, process PID: %d", task_cmd, self.process.pid)

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                self.log.debug("Task: %s exit code: %s", self, ret_code)
                self.is_failed = True

                if self.config.get("stop-on-fail"):
                    self.log.error("Task: %s failed with stop-on-fail option, shutting down", self)
                    raise AutomatedShutdown
            self.log.info('Task: %s was finished', self)
            return True
        # self.stderr.flush()
        # self.stdout.flush()
        self.log.debug('Task: %s was not finished yet', self)
        return False

    def shutdown(self):
        """
        Shutdown force/grace
        :return:
        """
        if not self.is_finished():
            self.log.info("Task %s was not completed, shutting it down", self)
            shutdown_process(self.process, self.log)
        else:
            self.log.debug("Task %s already completed, no shutdown needed", self)
        with open(self.stderr.name) as fds_stderr, open(self.stdout.name) as fds_stdout:
            out = fds_stdout.read()
            err = fds_stderr.read()

        if out:
            self.log.debug("Task %s stdout:\n %s", self, out)
        if err:
            self.log.error("Task %s stderr:\n %s", self, err)

        out_option = self.config.get("out")
        err_option = self.config.get("err")

        try:
            if out_option:
                shutil.move(self.stdout.name, os.path.join(self.working_dir, out_option))
                self.log.info("Task output was saved in:%s", out_option)
            else:
                os.remove(self.stdout.name)
        except:
            self.log.error("Task output was saved in:%s", self.stdout.name)

        try:
            if err_option:
                shutil.move(self.stderr.name, os.path.join(self.working_dir, err_option))
                self.log.info("Task stderr was saved in:%s", err_option)
            else:
                os.remove(self.stderr.name)
        except:
            self.log.error("Task stderr was saved in:%s", self.stderr.name)

    def __str__(self):
        if self.config.get("label"):
            return self.config.get("label")
        else:
            return self.config.get("command")

    def __repr__(self):
        return str(self.config)
