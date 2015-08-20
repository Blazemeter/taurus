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
from bzt.engine import ScenarioExecutor
import time
import tempfile
import os
from bzt import AutomatedShutdown


class ShellExecutor(ScenarioExecutor):
    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.task_list = []

    def prepare(self):
        """
        Configure Tasks
        :return:
        """
        tasks = self.get_scenario().get("tasks")
        for task_config in tasks:
            try:
                self.task_list.append(Task(task_config, self.log))
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
            self.log.warning("Some no-blocking tasks was not completed before shutdown")

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
                task.shutdown()  # TODO: implement graceful shutdown with check
                self.log.debug("Removing task %s from task list %s", task, self.task_list)
                self.task_list.remove(task)
                self.log.debug("Modified task list: %s", self.task_list)
        self.log.debug("Tasks shutdown completed (if any)")

    # def _remove_completed_tasks(self):
    #     self.log.debug("checking for completed tasks")
    #     for task in self.task_list:
    #         if task.config.get("block") and task.is_finished():
    #             task.shutdown()
    #             self.task_list.remove(task)

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

        # self._remove_completed_tasks()


class Task(object):
    def __init__(self, config, parent_log):
        self.log = None
        self.config = config
        self.process = None
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
        blocks = force_shutdown = stop_on_fail = [True, False]
        possible_keys = ["start-stage", "stop-stage", "block", "force-shutdown", "stop-on-fail", "label", "command"]
        default_config = {"start-stage": "prepare", "block": False, "force-shutdown": True, "stop-stage": "shutdown",
                          "stop-on-fail": False}
        default_config.update(self.config)
        self.config = default_config

        # check keys in config
        for key in self.config.keys():
            if key not in possible_keys:
                self.log.warning("Ignoring unknown option %s in task config! %s", key, self.config)

        if self.config["start-stage"] not in stages or self.config["stop-stage"] not in stages:
            self.log.error("Invalid stage name in task config!")
            raise ValueError

        if self.config["block"] not in blocks or self.config["force-shutdown"] not in force_shutdown or self.config[
            "stop-on-fail"] not in stop_on_fail:
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

        with tempfile.NamedTemporaryFile(prefix="task_out", delete=False) as self.stdout, tempfile.NamedTemporaryFile(
                prefix="task_err", delete=False) as self.stderr:
            self.process = shell_exec(task_cmd, cwd="/tmp", stdout=self.stdout, stderr=self.stderr)
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

    def shutdown(self, force=True):
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
            self.log.debug("Task %s stdout:\n %s", self, fds_stdout.read())
            self.log.debug("Task %s stderr:\n %s", self, fds_stderr.read())
        # TODO: print contents and/or move them to artifacts
        os.remove(self.stderr.name)
        os.remove(self.stdout.name)

    def __str__(self):
        return self.config.get("command")  # TODO: use label from config, not command

    def __repr__(self):
        return str(self.config)
