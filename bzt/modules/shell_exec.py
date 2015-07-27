"""
---
settings:
  shell_hooks:
  - do some preparations: export TEST=1
  - check our server:
    stage: check
    command: our_check_script.bash



shell_hooks:
- shell_hook_one: "cmd.exe"
- shell_hook_two:
  command: "cmd.exe -k && netstat -anb 1"
  post-stage:true
  block: yes
- shell_hook_three:
  command: "cmd.exe -k && netstat -anb 1"
  stage: shutdown
  start-stage: pre process
  shutdown-stage: post shutdown
  block: no
  stop-on-fail: yes
  shutdown: force


command
blocking: yes/no
stages: prepare, startup, shutdown, check, post_process
post-stage: true/false
stop-on-fail: yes/no
shutdown:force/grace
stdout: my_shell_out.txt
stderr: my_shell_err.txt


"""

from bzt.utils import shell_exec
from bzt.engine import EngineModule

class ShellExecutor(EngineModule):
    def __init__(self):
        super(ShellExecutor, self).__init__()
        self.shells = []
        self._stage = None
        self.possible_stages = ["prepare", "post-prepare",
                                "startup", "post-startup",
                                "check", "post-check",
                                "postprocess", "post-postprocess",
                                "shutdown", "post-shutdown"]

    def add_shell(self, shell_config):
        """
        Create shell, add it to self.shells
        """
        pass

    def configure(self, config):
        """
        Parse config, create shell objects
        :param config:
        :return:
        """
        if config:
            for shell_config in config:
                shell = Shell(shell_config)
                self.shells.append(shell)
        else:
            self.log.debug("No shell hooks configured")

    @property
    def stage(self, stage):
        return self._stage

    @stage.setter
    def stage(self, stage):
        if stage not in self.possible_stages:
            self.log.error("Unknown stage")
        else:
            self._stage = stage
            self.log.debug("Stage changed to %s", stage)
            self.poll_shells(stage)

    def poll_shells(self, pre_stage):
        for shell in self.shells:
            shell.poll()

    def shutdown(self):
        pass



class Shell(object):
    def __init__(self, config):
        self.config = config

    def prepare(self):
        """
        Parse config, apply config
        :return:
        """
        pass

    def startup(self):
        """
        Run task
        :return:
        """
        pass

    def shutdown(self, force=True):
        """
        Shutdown
        :return:
        """
        pass

    def check(self):
        """
        check if running
        :return:
        """
        pass