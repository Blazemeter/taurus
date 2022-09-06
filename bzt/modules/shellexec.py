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
from subprocess import CalledProcessError, PIPE

from bzt import TaurusConfigError, RCProvider
from bzt.engine import Service
from bzt.utils import ensure_is_dict, Environment, shutdown_process, shell_exec, communicate


class ShellExecutor(Service):
    """
    :type env: Environment
    :type prepare_tasks: list[Task]
    :type startup_tasks: list[Task]
    :type check_tasks: list[Task]
    :type shutdown_tasks: list[Task]
    :type postprocess_tasks: list[Task]
    """

    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.prepare_tasks = []
        self.startup_tasks = []
        self.check_tasks = []
        self.shutdown_tasks = []
        self.postprocess_tasks = []
        self.env = Environment(self.log)

    def _load_tasks(self, stage, container):
        if not isinstance(self.parameters.get(stage, []), list):
            self.parameters[stage] = [self.parameters[stage]]

        for index, stage_task in enumerate(self.parameters.get(stage, [])):
            stage_task = ensure_is_dict(self.parameters[stage], index, "command")
            task_config = self.parameters[stage][index]
            default_cwd = self.settings.get("default-cwd", None)
            cwd = self.engine.find_file(task_config.get("cwd", default_cwd))
            if cwd is None:
                working_dir = self.engine.default_cwd
            elif cwd == 'artifacts-dir':
                working_dir = self.engine.artifacts_dir
            else:
                working_dir = cwd

            # make copy of env for every task
            env = Environment(self.log, self.env)
            env.set(task_config.get('env'))
            env.add_path({"PYTHONPATH": working_dir})

            task = Task(task_config, self.log, working_dir, env)

            f_name = 'shellexec_%s_%s' % (stage, index)
            if task.out:
                task.out = open(task.out, 'at')
            else:
                if task.is_background:
                    out = self.engine.create_artifact(f_name, '.out')
                    self.log.debug('STDOUT of background task "%s" redirected to "%s"' % (task, out))
                    task.out = open(out, 'at')
                else:
                    task.out = PIPE

            if task.err:
                task.err = open(task.err, 'at')
            else:
                if task.is_background:
                    err = self.engine.create_artifact(f_name, '.err')
                    self.log.debug('STDERR of background task "%s" redirected to "%s"' % (task, err))
                    task.err = open(err, 'at')
                else:
                    task.err = PIPE

            container.append(task)
            self.log.debug("Added %s task: %s", stage, stage_task)

    def prepare(self):
        """
        Configure Tasks
        :return:
        """
        self.env.set(self.settings.get('env'))

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
            self._set_stop_reason_vars(task)
            task.start()

        for task in self.check_tasks + self.startup_tasks:
            task.shutdown()

    def post_process(self):
        for task in self.shutdown_tasks + self.check_tasks + self.startup_tasks + self.prepare_tasks:
            task.shutdown()

        for task in self.postprocess_tasks:
            self._set_stop_reason_vars(task)
            task.start()
            task.shutdown()

    def _set_stop_reason_vars(self, task):
        if isinstance(self.engine.stopping_reason, RCProvider):
            rc = self.engine.stopping_reason.get_rc()
            task.env.set({
                "TAURUS_STOPPING_REASON": str(self.engine.stopping_reason)
            })
        elif self.engine.stopping_reason:
            rc = 1
        else:
            rc = 0

        task.env.set({
            "TAURUS_EXIT_CODE": rc,
            "TAURUS_STOPPING_REASON": str(self.engine.stopping_reason)
        })


class Task(object):
    """
    :type process: subprocess.Popen
    :type env: Environment
    """

    def __init__(self, config, parent_log, working_dir, env):
        """
        :type env: Environment
        """
        self.log = parent_log.getChild(self.__class__.__name__)
        self.working_dir = working_dir
        self.env = env

        self.command = config.get("command", TaurusConfigError("Parameter is required: command"))
        self.is_background = config.get("background", False)

        self.out = config.get("out", None)
        self.err = config.get("err", None)

        self.ignore_failure = config.get("ignore-failure", False)
        self.process = None

    def start(self):
        """
        Start task
        """
        if self.process:
            self.check()
            self.log.info("Process already running: %s", self)
            return

        self.log.info("Starting shell command: %s", self)
        self.process = shell_exec(args=self.command, stdout=self.out, stderr=self.err, cwd=self.working_dir,
                                  env=self.env.get(), shell=True)
        if self.is_background:
            self.log.debug("Task started, PID: %d", self.process.pid)
        else:
            self._get_results()

    def check(self):
        if not self.process or self.process.returncode is not None:  # not started or already finished task
            return True

        if self.process.poll() is None:
            self.log.debug('Task is not finished yet: %s', self)
            return False

        self._get_results()
        return True

    def _get_results(self):
        stdout, stderr = communicate(self.process)

        if stdout and (self.out == PIPE):
            self.log.debug("Output for %s:\n%s", self, stdout)

        if stderr and (self.err == PIPE):
            self.log.warning("Errors for %s:\n%s", self, stderr)

        self.log.debug("Task was finished with exit code %s: %s", self.process.returncode, self)
        if not self.ignore_failure and self.process.returncode != 0:
            if self.out != PIPE:
                self.log.warning("Output for %s:\n%s", self, stdout)
            raise CalledProcessError(self.process.returncode, self)

    def shutdown(self):
        """
        If task was not completed, kill process, provide output
        else provide output
        :return:
        """
        self.check()

        if self.process and self.process.returncode is None:
            self.log.info("Background task was not completed, shutting it down: %s", self)
            shutdown_process(self.process, self.log)

        self.process = None
        for stream in (self.out, self.err):
            if stream and stream != PIPE:
                stream.close()

    def __repr__(self):
        return self.command
