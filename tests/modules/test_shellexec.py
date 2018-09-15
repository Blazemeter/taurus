import os
import time
from subprocess import CalledProcessError

from bzt.engine import Service
from bzt.modules.shellexec import ShellExecutor
from bzt.utils import BetterDict, is_windows
from tests import BZTestCase
from tests.mocks import EngineEmul


class TaskTestCase(BZTestCase):
    def setUp(self):
        super(TaskTestCase, self).setUp()
        self.obj = ShellExecutor()
        self.obj.parameters = BetterDict()
        self.obj.engine = EngineEmul()
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.engine.default_cwd = os.getcwd()
        self.sniff_log()


class TestBlockingTasks(TaskTestCase):
    def test_task_prepare(self):
        self.obj.settings['env'] = {"VAR": 1}
        if is_windows():
            task = "dir .. && cd .."
        else:
            task = "ls .. && cd .."
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()

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
        except CalledProcessError:
            pass

    def test_print_out(self):
        task = {"command": "pwd", "out": None}
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()


class TestNonBlockingTasks(TaskTestCase):
    def test_background_task_shutdown(self):
        task = {"command": "sleep 10", "background": True}
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.post_process()
        self.assertIn("Background task was not completed, shutting it down: sleep 10",
                      self.log_recorder.info_buff.getvalue())

    def test_background_task_completed(self):
        task = {"command": "sleep 1", "background": True}
        blocking_task = {"command": "sleep 2", "background": False}
        self.obj.parameters.merge({"prepare": [task, blocking_task]})
        self.obj.prepare()
        self.obj.post_process()
        self.assertIn("Task was finished with exit code 0: sleep 1", self.log_recorder.debug_buff.getvalue())

    def test_background_task_output(self):
        task = {"command": "echo hello", "background": True}
        blocking_task = {"command": "sleep 1", "background": False}
        self.obj.parameters.merge({"prepare": [task, blocking_task]})
        self.obj.prepare()
        self.obj.check()
        self.obj.shutdown()
        self.assertIn("Output for echo hello:\n", self.log_recorder.debug_buff.getvalue())

    def test_background_task_stop_on_fail(self):
        task = {"command": "python -m nosuchmodule", "background": True, "ignore-failure": False}
        blocking_task = {"command": "sleep 1", "block": True}
        self.obj.parameters.merge({"prepare": [task, blocking_task]})
        try:
            self.obj.prepare()
            self.obj.post_process()
            self.fail()
        except CalledProcessError:
            pass

    def test_background_task_check_stage(self):
        task = {"command": "sleep 5 && pwd", "background": True}
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.startup()
        for _x in range(0, 3):
            self.obj.check()
            time.sleep(1)
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIn("Task: sleep 5 && pwd is not finished yet", self.log_recorder.debug_buff.getvalue())


class TestTasksConfigs(TaskTestCase):
    def test_shell_exec(self):
        out_file = os.path.join(self.obj.engine.artifacts_dir, 'out.txt')
        err_file = os.path.join(self.obj.engine.artifacts_dir, 'err.txt')
        file1 = self.obj.engine.create_artifact('file_1.out', "")
        file2 = self.obj.engine.create_artifact('file_2.out', "")
        command = "echo 1 > {file1} && sleep 1 && echo 2 > {file2}"
        task = {"command": command.format(file1=file1, file2=file2), "out": out_file, "err": err_file}
        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.assertEqual(open(file1).read().strip(), '1')
        self.assertEqual(open(file2).read().strip(), '2')
        self.assertTrue(os.path.exists(out_file))
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, err_file)))

    def test_config(self):
        self.obj.engine.config.merge({'services': [
            {'startup': [{'command': 'sleep 10 && echo 111', 'background': True}],
             'check': [{'command': 'dmesg | grep nvidia', 'ignore-failure': True}, 'pwd'], 'module': 'shellexec'}]})
        self.obj.parameters = self.obj.engine.config.get(Service.SERV)[0]
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
