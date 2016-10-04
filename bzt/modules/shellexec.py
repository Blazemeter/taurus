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
import subprocess
from subprocess import CalledProcessError

from bzt.engine import Service
from bzt.six import iteritems
from bzt.utils import ensure_is_dict
from bzt.utils import shutdown_process, BetterDict, is_windows

ARTIFACTS_DIR_ENVVAR = "TAURUS_ARTIFACTS_DIR"


class ShellExecutor(Service):
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
            task_config = self.parameters[stage][index]
            default_cwd = self.settings.get("default-cwd", None)
            cwd = task_config.get("cwd", default_cwd)
            if cwd is None:
                working_dir = self.engine.default_cwd
            elif cwd == 'artifacts-dir':
                working_dir = self.engine.artifacts_dir
            else:
                working_dir = cwd

            env = BetterDict()
            env.merge({k: os.environ.get(k) for k in os.environ.keys()})
            env.merge(self.settings.get('env'))
            env.merge(task_config.get('env'))
            env.merge({"PYTHONPATH": working_dir})
            if os.getenv("PYTHONPATH"):
                env['PYTHONPATH'] = os.getenv("PYTHONPATH") + os.pathsep + env['PYTHONPATH']
            env[ARTIFACTS_DIR_ENVVAR] = self.engine.artifacts_dir

            for name, value in iteritems(env):
                env[str(name)] = str(value)

            task = Task(task_config, self.log, working_dir, env)
            container.append(task)
            self.log.debug("Added %s task: %s", stage, stage_task)

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

    def __init__(self, config, parent_log, working_dir, env):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.working_dir = working_dir
        self.env = env
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
        """
        if self.process:
            self.check()
            self.log.info("Process still running: %s", self)
            return

        if self.out is not None and self.out != subprocess.PIPE:
            out = open(self.out, 'at')
        else:
            out = self.out

        if self.err is not None and self.err != subprocess.PIPE:
            err = open(self.err, 'at')
        else:
            err = self.err

        kwargs = {
            'args': self.command,
            'stdout': out,
            'stderr': err,
            'cwd': self.working_dir,
            'env': self.env,
            'shell': True
        }
        # FIXME: shouldn't we bother closing opened descriptors?
        if not is_windows():
            kwargs['preexec_fn'] = os.setpgrp
            kwargs['close_fds'] = True

        self.log.info("Starting shell command: %s", self)
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
