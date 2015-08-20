import os
import time
from tests import setup_test_logging, BZTestCase, local_paths_config, __dir__, RecordingHandler
from tests.mocks import EngineEmul
from bzt.modules.shell_exec import ShellExecutor
from bzt import AutomatedShutdown

setup_test_logging()
ABS_PATH = lambda _x: os.path.abspath(__dir__() + _x)


class TestShellExec(BZTestCase):
    def setUp(self):
        super(TestShellExec, self).setUp()
        self.engine_obj = EngineEmul()
        self.engine_obj.artifacts_base_dir = ABS_PATH("/../../build/test")
        self.paths = [ABS_PATH("/../../bzt/10-base.json"), local_paths_config()]
        self.engine_obj.configure(self.paths)
        self.engine_obj.config.merge({"provisioning": "local"})
        self.shell_executor_config = self.engine_obj.config["modules"]["shell-executor"]
        self.tasks = []
        self.baseconfig = {"execution": {"executor": "shell-executor", "scenario": {"tasks": self.tasks}}}

class TestBlockingTasks(TestShellExec):

    def test_task_prepare_prepare(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label": "blocking,prepare,prepare,no-stop", "command": "sleep 2", "block": True,
                "start-stage": "prepare",
                "stop-stage": "prepare", "force-shutdown": False, "stop-on-fail": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Task sleep 2 already completed, no shutdown needed", log_recorder.debug_buff.getvalue())

    def test_task_prepare_shutdown(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label": "blocking,prepare,shutdown,no-stop", "command": "sleep 2", "block": True,
                "start-stage": "prepare",
                "stop-stage": "shutdown", "force-shutdown": False, "stop-on-fail": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Task sleep 2 already completed, no shutdown needed", log_recorder.debug_buff.getvalue())

    def test_task_output(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label": "task one", "command": "echo hello", "block": True,
                "start-stage": "prepare",
                "stop-stage": "check", "force-shutdown": False, "stop-on-fail": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Task echo hello stdout:\n hello", log_recorder.debug_buff.getvalue())

    def test_task_start_error(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label": "task one", "command": "nothing", "block": True,
                "start-stage": "check",
                "stop-stage": "shutdown", "force-shutdown": False, "stop-on-fail": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Exception while starting task", log_recorder.err_buff.getvalue())

    def test_task_stop_on_fail(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"command": "python -m nosuchmodule", "block": True, "stop-on-fail": True}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        try:
            obj.prepare()
            self.fail()
        except AutomatedShutdown:
            pass


class TestNonBlockingTasks(TestShellExec):

    def test_task_prepare_prepare(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"command": "sleep 10", "block": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Task sleep 10 was not completed, shutting it down", log_recorder.info_buff.getvalue())

    def test_task_prepare_shutdown(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"command": "sleep 1", "block": False, "stop-stage": "shutdown"}
        blocking_task = {"command": "sleep 2", "block": True}
        self.tasks.append(task)
        self.tasks.append(blocking_task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Task sleep 1 already completed, no shutdown needed", log_recorder.debug_buff.getvalue())

    def test_task_output(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"command": "echo hello", "block": False,
                "start-stage": "prepare",
                "stop-stage": "check", "force-shutdown": False, "stop-on-fail": False}
        blocking_task = {"command": "sleep 1", "block": True}
        self.tasks.append(task)
        self.tasks.append(blocking_task)

        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Task echo hello stdout:\n hello", log_recorder.debug_buff.getvalue())

    def test_task_start_error(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label": "task one", "command": "nothing", "block": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)

        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        self.assertIn("Exception while starting task", log_recorder.err_buff.getvalue())

    def test_task_stop_on_fail(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"command": "python -m nosuchmodule", "block": False, "stop-on-fail": True, "stop-stage":"prepare"}
        blocking_task = {"command": "sleep 1", "block": True}
        self.tasks.append(task)
        self.tasks.append(blocking_task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        try:
            obj.prepare()
            self.fail()
        except AutomatedShutdown:
            pass

class TestTasksConfigs(TestShellExec):

    def test_unknown_key_in_config(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)
        obj.settings = self.shell_executor_config
        task = {"invalid": "invalid", "command": "sleep 10", "block": False,
                "start-stage": "prepare",
                "stop-stage": "shutdown", "force-shutdown": True, "stop-on-fail": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))
        obj.prepare()

        self.assertIn("Ignoring unknown option", log_recorder.warn_buff.getvalue())

    def test_wrong_stage(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)
        obj.settings = self.shell_executor_config
        task = {"invalid": "invalid", "command": "sleep 10", "block": False,
                "start-stage": "prepare",
                "stop-stage": "shutTdown", "force-shutdown": True, "stop-on-fail": False}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))

        obj.prepare()

        # try:
        #     obj.prepare()
        #     self.fail()
        # except ValueError:
        #     pass

        self.assertIn("Invalid stage name in task config!", log_recorder.err_buff.getvalue())

        #
        #     def test_shell_exec(self):
        #         """
        #
        #         :return:
        #         """
        #         l = "ls > /tmp/ll.txt"
        #         x = shell_exec(l, shell=True)
        #         c = x.communicate()
        #         print(c)
