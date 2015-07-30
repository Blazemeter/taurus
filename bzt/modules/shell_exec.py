import shlex
from bzt.utils import shell_exec, shutdown_process, ensure_is_dict, BetterDict
from bzt.engine import ScenarioExecutor
import time
import tempfile
import os
from bzt import ManualShutdown


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
        self.check_tasks("prepare")

    def startup(self):
        """
        :return:
        """
        self.check_tasks("startup")

    def check(self):
        """
        If any tasks in check stage -
        :return:
        """
        return True

    def shutdown(self):
        """
        should forcefully kill all tasks at end
        :return:
        """
        self.check_tasks("shutdown")

    def check_tasks(self, cur_stage):
        self.log.debug("Checking tasks, stage: %s", cur_stage)
        for task in self.tasks:
            if task.config.get("start-stage") == cur_stage:
                task.startup()

        for task in self.tasks:
            if task.config.get("block") and not task.is_finished():
                while not task.is_finished():
                    self.log.debug("Waiting for tasks to complete...")
                    time.sleep(1)

        for task in self.tasks:
            if task.config.get("stop-stage") == cur_stage:
                self.log.debug("shutting task down, stop_stage, %d", task.process.pid)
                task.shutdown()


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
        task_cmd = " ".join(shlex.split(self.config.get("command")))
        # self.log.debug("Task command line %s", task_cmd)
        with tempfile.NamedTemporaryFile(prefix="task_out", delete=False) as self.stdout, tempfile.NamedTemporaryFile(
                prefix="task_out", delete=False) as self.stderr:
            self.process = shell_exec(task_cmd, cwd="/tmp", stdout=self.stdout, stderr=self.stderr, shell=True)
        self.log.debug("Task PID: %d", self.process.pid)

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            with open(self.stderr.name) as fds_stderr, open(self.stdout.name) as fds_stdout:
                self.log.debug("Task stdout:\n %s", fds_stdout.read())
                self.log.debug("Task stderr:\n %s", fds_stderr.read())
            if ret_code != 0:
                self.log.debug("Task exit code: %s", ret_code)
                self.is_failed = True
                if self.config.get("stop-on-fail"):
                    self.log.error("Task failed, shutting down.")
                    raise ManualShutdown
            return True
        self.stderr.flush()
        self.stdout.flush()
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

        os.remove(self.stderr.name)
        os.remove(self.stdout.name)
