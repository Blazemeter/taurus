import os
from tests import setup_test_logging, BZTestCase, RecordingHandler
from tests.mocks import EngineEmul
from bzt.modules.shellexec import ShellExecutor
from bzt import AutomatedShutdown
import yaml
from tempfile import NamedTemporaryFile
from bzt.utils import BetterDict
import time

setup_test_logging()


class TaskTestCase(BZTestCase):
    def setUp(self):
        self.obj = ShellExecutor()
        self.obj.parameters = BetterDict()
        self.obj.engine = EngineEmul()
        self.log_recorder = RecordingHandler()
        self.obj.log.addHandler(self.log_recorder)

    def tearDown(self):
        self.obj.log.removeHandler(self.log_recorder)


class TestBlockingTasks(TaskTestCase):
    def test_task_prepare(self):
        task = "sleep 1 && echo 123"
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()
        self.assertIn("Task sleep 1 && echo 123 stdout:\n 123", self.log_recorder.info_buff.getvalue())

    def test_nonbackground_startup(self):
        task = "echo hello"
        self.obj.parameters.merge({"startup": [task]})
        try:
            self.obj.prepare()
            self.fail()
        except ValueError:
            pass

    def test_nonbackground_prepare(self):
        task = {"command": "echo hello", "background": True}
        task2 = 'sleep 1'
        self.obj.parameters.merge({"prepare": [task, task2]})
        try:
            self.obj.prepare()
        except ValueError:
            self.fail()

    def test_task_stop_on_fail(self):
        task = {"command": "python -m nosuchmodule", "ignore-failure": False}
        self.obj.parameters.merge({"prepare": [task]})
        try:
            self.obj.prepare()
            self.fail()
        except AutomatedShutdown:
            pass


class TestNonBlockingTasks(TaskTestCase):
    def test_background_task_shutdown(self):
        task = {"command": "sleep 10", "background": True}
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.post_process()
        self.assertIn("Background task sleep 10 was not completed, shutting it down", self.log_recorder.info_buff.getvalue())

    def test_background_task_completed(self):
        task = {"command": "sleep 1", "background": True}
        blocking_task = {"command": "sleep 2", "background": False}
        self.obj.parameters.merge({"prepare": [task, blocking_task]})
        self.obj.prepare()
        self.obj.post_process()
        self.assertIn("Task: sleep 1 was finished with exit code: 0", self.log_recorder.debug_buff.getvalue())

    def test_background_task_output(self):
        task = {"command": "echo hello", "background": True}
        blocking_task = {"command": "sleep 1", "background": False}
        self.obj.parameters.merge({"prepare": [task, blocking_task]})
        self.obj.prepare()
        self.obj.check()
        self.obj.shutdown()
        self.assertIn("Task echo hello stdout:\n hello", self.log_recorder.info_buff.getvalue())

    def test_background_task_stop_on_fail(self):
        task = {"command": "python -m nosuchmodule", "background": True, "ignore-failure": False}
        blocking_task = {"command": "sleep 1", "block": True}
        self.obj.parameters.merge({"prepare": [task, blocking_task]})
        try:
            self.obj.prepare()
            self.fail()
        except AutomatedShutdown:
            pass

    def test_background_task_check_stage(self):
        task = {"command": "sleep 2 && pwd", "background": True}
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.startup()
        for _x in range(0, 3):
            self.obj.check()
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIn("Task: sleep 2 && pwd was not finished yet", self.log_recorder.debug_buff.getvalue())

class TestTasksConfigs(TaskTestCase):
    def test_shell_exec(self):
        out_file = os.path.join(self.obj.engine.artifacts_dir, 'out.txt')
        err_file = 'err.txt'
        with NamedTemporaryFile() as file1, NamedTemporaryFile() as file2:
            command = "echo 1 > {file1} && sleep 1 && echo 2 > {file2} && dmesg | grep pci"
            task = {"command": command.format(file1=file1.name, file2=file2.name), "out": out_file, "err": err_file}
            self.obj.parameters.merge({"prepare": [task]})
            self.obj.prepare()
            self.assertEqual(open(file1.name).read(), '1\n')
            self.assertEqual(open(file2.name).read(), '2\n')
            self.assertTrue(os.path.exists(out_file))
            self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, err_file)))

    def test_config(self):
        self.obj.engine.config.merge(yaml.load(open("tests/yaml/shell_hook_start").read()))
        self.obj.parameters = self.obj.engine.config.get("services")[0]
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
