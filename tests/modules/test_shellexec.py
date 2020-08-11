import os
import time
from subprocess import CalledProcessError

from bzt import ManualShutdown
from bzt.engine import Service
from bzt.modules.shellexec import ShellExecutor
from bzt.utils import BetterDict, is_windows, temp_file
from tests import BZTestCase
from tests.mocks import EngineEmul


class TaskTestCase(BZTestCase):
    def setUp(self):
        super(TaskTestCase, self).setUp()
        self.obj = ShellExecutor()
        self.obj.parameters = BetterDict()
        self.obj.engine = EngineEmul()
        self.obj.engine.config.merge({
            "provisioning": "local",
            "modules": {
                "local": {"class": "bzt.modules.provisioning.Local"},
                "cloud": {"class": "bzt.modules.blazemeter.CloudProvisioning"},
            }})
        self.obj.engine.default_cwd = os.getcwd()
        self.sniff_log(self.obj.log)


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

    def test_long_buf(self):
        """ subprocess (tast) became blocked and blocks parent (shellexec)
        if exchange buffer (PIPE) is full because of wait() """
        file_name = temp_file()
        if is_windows():
            task = "type "
            buf_len = 2 ** 10 * 4  # 4K
        else:
            task = "tail "
            buf_len = 2 ** 10 * 64  # 64K
        task += file_name
        buf = '*' * (buf_len + 1)
        with open(file_name, "w+") as _file:
            _file.write(buf)

        self.obj.parameters.merge({"prepare": [task]})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()
        out = self.log_recorder.debug_buff.getvalue()
        self.assertIn(buf, out)

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

    def test_default_prov_context_for_local(self):
        command = "my_echo"
        var_name, var_value = "VAR_NAME", "VAR_VALUE"
        self.obj.parameters.merge({"startup": [{"command": command}]})
        self.obj.settings.get("env", force_set=True).update({var_name: var_value})

        self.obj.prepare()

        commands = [task.command for task in self.obj.prepare_tasks + self.obj.startup_tasks +
                    self.obj.check_tasks + self.obj.shutdown_tasks + self.obj.postprocess_tasks]
        env_vars = self.obj.env.get()

        # handle as usual
        self.assertEqual(env_vars.get(var_name), var_value)
        self.assertIn(command, commands)

    def test_default_prov_context_for_cloud(self):
        command = "my_echo"
        var_name, var_value = "VAR_NAME", "VAR_VALUE"
        self.obj.parameters.merge({"startup": [{"command": command}]})
        self.obj.settings.get("env", force_set=True).update({var_name: var_value})
        self.obj.engine.config.merge({"provisioning": "cloud"})

        self.obj.prepare()

        commands = [task.command for task in self.obj.prepare_tasks + self.obj.startup_tasks +
                    self.obj.check_tasks + self.obj.shutdown_tasks + self.obj.postprocess_tasks]
        env_vars = self.obj.env.get()

        # mustn't be processed for cloud provisioning case
        self.assertNotEqual(env_vars.get(var_name), var_value)
        self.assertNotIn(command, commands)

    def test_same_prov_context(self):
        command = "my_echo"
        var_name, var_value = "VAR_NAME", "VAR_VALUE"
        target_prov = "cloud"

        self.obj.parameters.merge({"cloud": {"startup": [{"command": command}]}})
        self.obj.settings.get("env", force_set=True).update({var_name: var_value})
        self.obj.settings = BetterDict.from_dict({target_prov: self.obj.settings})
        self.obj.engine.config.merge({"provisioning": "cloud"})

        self.obj.prepare()

        commands = [task.command for task in self.obj.prepare_tasks + self.obj.startup_tasks +
                    self.obj.check_tasks + self.obj.shutdown_tasks + self.obj.postprocess_tasks]
        env_vars = self.obj.env.get()

        # must be handled because current prov specified as target
        self.assertEqual(env_vars.get(var_name), var_value)
        self.assertIn(command, commands)

    def test_different_prov_context(self):
        command = "my_echo"
        var_name, var_value = "VAR_NAME", "VAR_VALUE"
        target_prov = "cloud"

        self.obj.parameters.merge({target_prov: {"startup": [{"command": command}]}})
        self.obj.settings.get("env", force_set=True).update({var_name: var_value})
        self.obj.settings = BetterDict.from_dict({target_prov: self.obj.settings})
        self.obj.engine.config.merge({"provisioning": "local"})     # the same as setUp value, just for emphasizing

        self.obj.prepare()

        commands = [task.command for task in self.obj.prepare_tasks + self.obj.startup_tasks +
                    self.obj.check_tasks + self.obj.shutdown_tasks + self.obj.postprocess_tasks]
        env_vars = self.obj.env.get()

        # mustn't be handled because settings are specified for different prov
        self.assertNotEqual(env_vars.get(var_name), var_value)
        self.assertNotIn(command, commands)


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
        if is_windows():
            varspec = '%TAURUS_EXIT_CODE%'
        else:
            varspec = "$TAURUS_EXIT_CODE"
        self.obj.parameters.merge({"shutdown": ["echo " + varspec]})
        self.obj.parameters.merge({"post-process": ["echo " + varspec]})
        self.obj.prepare()
        self.obj.shutdown()
        getvalue = self.log_recorder.debug_buff.getvalue()
        self.log_recorder.debug_buff.truncate(0)
        self.log_recorder.debug_buff.seek(0)
        self.assertIn("Output for echo " + varspec + ":\n0", getvalue)

        self.obj.engine.stopping_reason = ManualShutdown()
        self.obj.post_process()
        buff_getvalue = self.log_recorder.debug_buff.getvalue()
        self.assertIn("Task was finished with exit code 0: sleep 1", buff_getvalue)
        self.assertIn("Output for echo " + varspec + ":\n2", buff_getvalue)

    def test_background_task_output(self):
        temp = temp_file()
        try:
            with open(temp, "at") as temp_f:
                temp_f.write("*" * (2 ** 16 + 1))
            if is_windows():
                cmd = "type"
            else:
                cmd = "cat"
            command1 = "%s %s" % (cmd, temp)
            command2 = "sleep 1"
            task = {"command": command1, "background": True}
            blocking_task = {"command": command2, "background": False}
            self.obj.parameters.merge({"prepare": [task, blocking_task]})
            self.obj.prepare()
            self.obj.check()
            self.obj.shutdown()
            out = self.log_recorder.debug_buff.getvalue()
            self.assertIn("code 0: %s" % command1, out)
            self.assertIn("code 0: %s" % command2, out)
        finally:
            os.remove(temp)

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
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIn("Task is not finished yet: sleep 5 && pwd", self.log_recorder.debug_buff.getvalue())


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
