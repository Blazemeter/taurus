from tests import setup_test_logging, BZTestCase, local_paths_config, __dir__
from tests.mocks import EngineEmul
from bzt.modules.shell_exec import ShellExecutor
import tempfile
import os
import yaml
import time

setup_test_logging()
ABS_PATH = lambda _x: os.path.abspath(__dir__() + _x)

class TestShellExec(BZTestCase):
    def setUp(self):
        super(TestShellExec, self).setUp()
        self.paths = local_paths_config()
        self.engine_obj = EngineEmul()
        self.engine_obj.artifacts_base_dir = ABS_PATH("/../../build/test")
        self.paths = [ABS_PATH("/../../bzt/10-base.json"), local_paths_config()]
        self.engine_obj.configure(self.paths)
        self.shell_executor_config = self.engine_obj.config["modules"]["shell-executor"]
        self.tasks = []
        self.baseconfig = {"execution": {"executor": "shell-executor", "scenario": {"tasks":self.tasks}}}


    def test_create_simple_tasks(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        obj.engine.config.merge(yaml.load(open("tests/yaml/shell_hook_start").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    def test_create_simple_task_block(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label":"my-task", "command":"sleep 3", "block":True}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    def test_create_simple_task_no_block(self):
        obj = ShellExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.shell_executor_config
        task = {"label":"my-task", "command":"echo $PWD", "block":True}
        self.tasks.append(task)
        obj.engine.config.merge(self.baseconfig)
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("shell-executor"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    # def test_
    #
    #
    #     except BaseException as exc:
    #         self.assertIn("Nothing to test", exc.args[0])
    #     obj.shutdown()