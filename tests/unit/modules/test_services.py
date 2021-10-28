import json
import os
import shutil
import sys
import zipfile
from os.path import join

from bzt import NormalShutdown, ToolError, TaurusConfigError
from bzt.engine import Service, Provisioning, EngineModule
from bzt.modules._locustio import LocustIOExecutor
from bzt.modules.blazemeter import CloudProvisioning
from bzt.modules.services import Unpacker, InstallChecker, AndroidEmulatorLoader, AppiumLoader, PipInstaller, PythonTool
from bzt.utils import get_files_recursive, EXE_SUFFIX, JavaVM, Node, is_windows
from tests.unit import BZTestCase, RESOURCES_DIR, EngineEmul
from tests.unit.mocks import ModuleMock, BZMock


class TestPipInstaller(BZTestCase):
    def setUp(self):
        super(TestPipInstaller, self).setUp()
        self.obj = PipInstaller()
        self.obj.engine = EngineEmul()

    def test_install(self):
        self.sniff_log(self.obj.log)
        self.obj.parameters['packages'] = ['test-package']
        self.obj.versions['test-package'] = "0.0.0"
        self.obj.pip_cmd = [join(RESOURCES_DIR, "python-pip", 'python-pip' + EXE_SUFFIX)]

        self.obj.prepare()
        self.assertEqual(self.obj.packages, ['test-package'])
        self.assertTrue(os.path.exists(self.obj.engine.temp_pythonpath))
        self.obj.post_process()  # remove directory afterwards
        if not is_windows():
            self.assertFalse(os.path.exists(self.obj.engine.temp_pythonpath))

        self.assertIn("Successfully installed test-package-0.0.0", self.log_recorder.info_buff.getvalue())

    def test_versions(self):
        self.obj.parameters['packages'] = ['one', 'two==2', {'name': 'three'}, {'name': 'four', 'version': '4'}]
        self.obj.pip_cmd = [join(RESOURCES_DIR, "python-pip", 'python-pip' + EXE_SUFFIX)]
        self.obj.prepare()

        self.assertEqual(self.obj.packages, ['one', 'two', 'three', 'four'])
        self.assertEqual(self.obj.versions, {'two': '2', 'four': '4'})

    def test_packages_installation(self):
        self.obj.parameters['packages'] = ['not-installed', 'new-version==1', 'installed==0']
        self.obj.pip_cmd = [join(RESOURCES_DIR, "python-pip", 'python-pip' + EXE_SUFFIX)]
        self.obj.prepare()

        self.assertEqual(self.obj.packages, ['not-installed', 'new-version'])
        self.assertEqual(self.obj.versions, {'new-version': '1'})


class TestPythonTool(BZTestCase):
    class PythonToolExample(PythonTool):
        PACKAGES = ['test-package']

    def setUp(self):
        self.engine = EngineEmul()
        super(TestPythonTool, self).setUp()

    def tearDown(self):
        super(TestPythonTool, self).tearDown()

    def test_check_and_install(self):
        self.obj = self.PythonToolExample(engine=self.engine, settings={})
        self.sniff_log(self.obj.log)
        self.obj.installer.pip_cmd = [join(RESOURCES_DIR, "python-pip", 'python-pip' + EXE_SUFFIX)]

        self.obj.check_if_installed()
        self.obj.install()
        self.obj.post_process()

        self.assertIn("Checking PythonToolExample.", self.log_recorder.debug_buff.getvalue())
        self.assertIn("PythonToolExample check failed.", self.log_recorder.warn_buff.getvalue())
        self.assertIn("Installing PythonToolExample.", self.log_recorder.debug_buff.getvalue())

    def test_set_installer_temp_setting(self):
        self.obj = self.PythonToolExample(engine=self.engine, settings={})
        self.assertEqual(self.obj.installer.temp, False)

        self.obj = self.PythonToolExample(engine=self.engine, settings={"version": "0.0.0"})
        self.assertEqual(self.obj.installer.temp, True)


class TestZipFolder(BZTestCase):
    def test_pack_and_send_to_blazemeter(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()

        obj.engine.config.merge({
            "execution": {
                "executor": "selenium",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2},
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/junit/java_package"}},
            "modules": {
                "selenium": "bzt.modules._selenium.SeleniumExecutor",
                "cloud": "bzt.modules.blazemeter.CloudProvisioning",
                "junit": "bzt.modules.java.JUnitTester"},
            "provisioning": "cloud"
        })
        obj.engine.unify_config()

        obj.parameters = obj.engine.config['execution'][0]
        obj.settings["token"] = "FakeToken"
        mock = BZMock(obj.user)
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {
                "result": [{"id": 1, 'name': 'Taurus Cloud Test', "configuration": {"type": "taurus"}}]
            },
        })
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1, 'workspaceId': 1}},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Cloud+Test': {
                "result": {"id": 1, "configuration": {"type": "taurus"}}
            },
            'https://a.blazemeter.com/api/v4/tests/1/files': {}
        })
        mock.mock_patch.update({'https://a.blazemeter.com/api/v4/tests/1': {"result": {}}})
        obj.prepare()

        unpack_cfgs = obj.engine.config.get(Service.SERV)
        self.assertEqual(len(unpack_cfgs), 1)
        self.assertEqual(unpack_cfgs[0]['module'], Unpacker.UNPACK)
        self.assertEqual(unpack_cfgs[0][Unpacker.FILES], ['java_package.zip'])
        self.assertTrue(zipfile.is_zipfile(obj.engine.artifacts_dir + '/java_package.zip'))

    @staticmethod
    def __get_user_info():
        with open(RESOURCES_DIR + "json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def test_receive_and_unpack_on_worker(self):
        obj = Unpacker()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            "execution": {
                "executor": "selenium",
                "concurrency": 5500,
                "scenario": {
                    "script": "java_package.zip"}},
            "modules": {
                "selenium": "bzt.modules._selenium.SeleniumExecutor",
                "cloud": "bzt.modules.blazemeter.CloudProvisioning"},
            "provisioning": "local"
        })
        obj.engine.file_search_paths = [obj.engine.artifacts_dir]

        obj.parameters["files"] = ["java_package.zip"]

        # create archive and put it in artifact dir
        source = RESOURCES_DIR + "selenium/junit/java_package"
        zip_name = obj.engine.create_artifact('java_package', '.zip')
        with zipfile.ZipFile(zip_name, 'w') as zip_file:
            for filename in get_files_recursive(source):
                zip_file.write(filename, filename[len(os.path.dirname(source)):])

        obj.prepare()

        # check unpacked tree
        destination = obj.engine.artifacts_dir + '/java_package'
        result_tree = set(filename[len(destination):] for filename in get_files_recursive(destination))
        original_tree = set(filename[len(source):] for filename in get_files_recursive(source))
        self.assertEqual(result_tree, original_tree)

    def test_no_work_prov(self):
        obj = Service()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'cloud'
        obj.parameters['run-at'] = 'local'
        self.assertFalse(obj.should_run())
        obj.parameters['run-at'] = 'cloud'
        self.assertTrue(obj.should_run())


class TestToolInstaller(BZTestCase):
    def test_regular(self):
        obj = InstallChecker()
        obj.engine = EngineEmul()
        obj.engine.config.get("modules")["base"] = EngineModule.__module__ + "." + EngineModule.__name__
        obj.engine.config.get("modules")["dummy"] = ModuleMock.__module__ + "." + ModuleMock.__name__
        self.assertRaises(NormalShutdown, obj.prepare)

    def test_problematic(self):
        obj = InstallChecker()
        obj.engine = EngineEmul()
        obj.engine.config.get("modules")["err"] = "hello there"
        self.assertRaises(ToolError, obj.prepare)

    def test_include_only_good(self):
        obj = InstallChecker()
        obj.engine = EngineEmul()
        obj.engine.config.get("modules")["base"] = EngineModule.__module__ + "." + EngineModule.__name__
        obj.engine.config.get("modules")["dummy"] = ModuleMock.__module__ + "." + ModuleMock.__name__
        obj.engine.config.get("modules")["err"] = "hello there"
        obj.settings["include"] = ["base", "dummy"]
        self.assertRaises(NormalShutdown, obj.prepare)

    def test_exclude_problematic(self):
        obj = InstallChecker()
        obj.engine = EngineEmul()
        obj.engine.config.get("modules")["err"] = "hello there"
        obj.settings["exclude"] = ["err"]
        self.assertRaises(NormalShutdown, obj.prepare)

    def test_include_string(self):
        obj = InstallChecker()
        obj.engine = EngineEmul()
        obj.engine.config.get("modules")["base"] = EngineModule.__module__ + "." + EngineModule.__name__
        obj.engine.config.get("modules")["dummy"] = ModuleMock.__module__ + "." + ModuleMock.__name__
        obj.engine.config.get("modules")["err"] = "hello there"
        obj.settings["include"] = "base,dummy"
        self.assertRaises(NormalShutdown, obj.prepare)

    def test_python_module(self):
        obj = InstallChecker()
        obj.engine = EngineEmul()
        obj.engine.config.get("modules")["locust"] = LocustIOExecutor.__module__ + "." + LocustIOExecutor.__name__
        obj.engine.user_pythonpath = obj.engine.temp_pythonpath
        obj.settings["include"] = "locust"

        tmp_exec = sys.executable
        try:
            sys.executable = join(RESOURCES_DIR, "python-pip", 'python-pip' + EXE_SUFFIX)
            obj.prepare()
        except NormalShutdown:
            pass
        finally:
            sys.executable = tmp_exec

        self.assertFalse(obj.engine.config.get("modules").get('locust').get('temp'))


class TestAndroidEmulatorLoader(BZTestCase):
    def setUp(self):
        engine = EngineEmul()
        engine.config.merge({'services': {'android-emulator-loader': {}}})
        self.check_if_emulator_started = AndroidEmulatorLoader.tool_is_started
        AndroidEmulatorLoader.tool_is_started = lambda slf: True
        self.android = AndroidEmulatorLoader()
        self.android.engine = engine
        self.android.settings = engine.config['services']['android-emulator-loader']

    def tearDown(self):
        AndroidEmulatorLoader.tool_is_started = self.check_if_emulator_started

    def test_no_sdk(self):
        os.environ['ANDROID_HOME'] = ''
        self.assertRaises(TaurusConfigError, self.android.prepare)

    def test_sdk_from_conf(self):
        os.environ['ANDROID_HOME'] = ''
        self.android.settings['path'] = 'from_config'
        self.assertRaises(ToolError, self.android.prepare)
        self.assertIn('from_config', self.android.tool_path)

    def test_sdk_from_env(self):
        sdk_path = join(self.android.engine.artifacts_dir, 'there_is_no_sdk')
        os.environ['ANDROID_HOME'] = sdk_path
        self.assertRaises(ToolError, self.android.prepare)
        self.assertIn(sdk_path, self.android.tool_path)

    def test_no_avd(self):
        self.create_fake_android_emulator()
        self.android.prepare()
        self.assertRaises(TaurusConfigError, self.android.startup)

    def test_two_way(self):
        config_path = join(self.android.engine.artifacts_dir, 'sdk', 'tools', 'emulator' + EXE_SUFFIX)
        self.android.settings['path'] = config_path
        env_path = 'from_env'
        os.environ['ANDROID_HOME'] = env_path
        self.create_fake_android_emulator()
        self.android.settings['avd'] = 'my_little_android'
        self.android.prepare()
        self.assertEqual(config_path, self.android.tool_path)
        self.android.startup()
        self.android.shutdown()
        self.android.post_process()

    def create_fake_android_emulator(self):
        sdk_dir = join(self.android.engine.artifacts_dir, 'sdk')
        src_dir = RESOURCES_DIR + 'android-emulator'
        dest_dir = join(sdk_dir, 'tools')
        os.mkdir(sdk_dir)
        os.mkdir(dest_dir)
        tool_path = join(dest_dir, 'emulator' + EXE_SUFFIX)
        shutil.copy2(join(src_dir, 'emulator' + EXE_SUFFIX), dest_dir)
        os.chmod(tool_path, 0o755)
        shutil.copy2(join(src_dir, 'emulator.py'), join(dest_dir, 'emulator.py'))
        self.android.settings['path'] = tool_path


class TestAppiumLoader(BZTestCase):
    def setUp(self):
        engine = EngineEmul()
        engine.config.merge({'services': {'appium-loader': {}}})
        self.check_if_appium_started = AppiumLoader.tool_is_started
        AppiumLoader.tool_is_started = lambda slf: True
        self.appium = AppiumLoader()
        self.appium.engine = engine
        self.appium.settings = engine.config['services']['appium-loader']
        self.check_if_node_installed = Node.check_if_installed
        self.check_if_java_installed = JavaVM.check_if_installed
        Node.check_if_installed = lambda slf: True
        JavaVM.check_if_installed = lambda slf: True

    def tearDown(self):
        AppiumLoader.tool_is_started = self.check_if_appium_started
        Node.check_if_installed = self.check_if_node_installed
        JavaVM.check_if_installed = self.check_if_java_installed

    def test_appium_not_installed(self):
        self.appium.settings['path'] = 'wrong_path'
        self.assertRaises(ToolError, self.appium.prepare)

    def test_appium_full_cycle(self):
        self.create_fake_appium()
        self.appium.prepare()
        self.appium.startup()
        self.appium.shutdown()
        self.appium.post_process()

    def create_fake_appium(self):
        src_dir = RESOURCES_DIR + 'appium'
        dest_dir = self.appium.engine.artifacts_dir
        shutil.copy2(join(src_dir, 'appium' + EXE_SUFFIX), dest_dir)
        os.chmod(join(dest_dir, 'appium' + EXE_SUFFIX), 0o755)
        shutil.copy2(join(src_dir, 'appium.py'), dest_dir)
        self.appium.settings['path'] = join(dest_dir, 'appium' + EXE_SUFFIX)
