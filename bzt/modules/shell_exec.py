from bzt.utils import shell_exec, shutdown_process, ensure_is_dict, BetterDict
from bzt.engine import ScenarioExecutor
import time
import tempfile
import os
from bzt import AutomatedShutdown


class ShellExecutor(ScenarioExecutor):
    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.tasks = []

    def prepare(self):
        """
        Configure Tasks
        :return:
        """
        tasks = self.get_scenario().get("tasks")
        for task_config in tasks:
            self.tasks.append(Task(task_config, self.log))
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
        self._check_completed_tasks()
        return not self.tasks

    def shutdown(self):
        """
        should forcefully kill all tasks at end
        :return:
        """
        self.process_stage("shutdown")
        if self.tasks:
            self.log.warning("Some no-blocking tasks was not completed before shutdown")

    def _start_tasks(self, cur_stage):
        self.log.debug("starting tasks on stage %s", cur_stage)
        for task in self.tasks:
            if task.config.get("start-stage") == cur_stage:
                task.startup()

    def _shutdown_tasks(self, cur_stage):
        self.log.debug("shutting down tasks on stage %s", cur_stage)
        for task in self.tasks:
            if task.config.get("stop-stage") == cur_stage:
                self.log.debug("shutting task down, %s, %d", cur_stage, task.process.pid)
                task.shutdown()  # TODO: implement graceful shutdown with check
                self.tasks.remove(task)

    def _check_completed_tasks(self):
        self.log.debug("checking if tasks has been completed")
        for task in self.tasks:
            if task.config.get("blocking") and task.is_finished():
                task.shutdown()
                self.tasks.remove(task)

    def _wait_for_blocking_tasks(self):
        for task in self.tasks:
            if task.config.get("block"):
                while not task.is_finished():
                    self.log.debug("Waiting for blocking tasks to complete...")
                    time.sleep(1)  # NOTE: use threading lock?

    def process_stage(self, cur_stage):
        self.log.debug("Checking tasks, stage: %s", cur_stage)
        self._start_tasks(cur_stage)
        self._wait_for_blocking_tasks()
        self._shutdown_tasks(cur_stage)

        self._check_completed_tasks()




class Task(object):
    def __init__(self, config, parent_log):
        self.log = None
        self.config = config
        self.process = None
        self.stdout = None
        self.stderr = None
        self.is_failed = False
        self.prepare(parent_log)

    def prepare(self, parent_log):
        """
        Parse config, apply config, merge with defaults
        :return:
        """

        self.log = parent_log.getChild(self.__class__.__name__)
        default_config = {"start-stage": "prepare", "block": False, "force-shutdown": True, "stop-stage": "shutdown",
                          "stop-on-fail": False}
        default_config.update(self.config)
        self.config = default_config

    def startup(self):
        """
        Run task
        :return:
        """
        task_cmd = self.config.get("command")
        # self.log.debug("Task command line %s", task_cmd)
        with tempfile.NamedTemporaryFile(prefix="task_out", delete=False) as self.stdout, tempfile.NamedTemporaryFile(
                prefix="task_err", delete=False) as self.stderr:
            self.process = shell_exec(task_cmd, cwd="/tmp", stdout=self.stdout, stderr=self.stderr)
        self.log.debug("Task %s, PID: %d", self, self.process.pid)

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                self.log.debug("Task %s exit code: %s", self, ret_code)
                self.is_failed = True
                if self.config.get("stop-on-fail"):
                    self.log.error("Task %s failed with stop-on-fail option, shutting down", self)
                    raise AutomatedShutdown
            return True
        # self.stderr.flush()
        # self.stdout.flush()
        return False

    def shutdown(self, force=True):
        """
        Shutdown force/grace
        :return:
        """
        if not self.is_finished():
            self.log.info("shutting task down")
            shutdown_process(self.process, self.log)
        else:
            self.log.info("Task already completed")

        with open(self.stderr.name) as fds_stderr, open(self.stdout.name) as fds_stdout:
                self.log.debug("Task %s stdout:\n %s", self,  fds_stdout.read())
                self.log.debug("Task %s stderr:\n %s", self, fds_stderr.read())
        os.remove(self.stderr.name)
        os.remove(self.stdout.name)

    def __str__(self):
        return self.config.get("command")
