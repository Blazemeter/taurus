"""
Implementations for small services

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

import copy
import json
import os
import time
import zipfile
import sys
import shutil

from urllib.request import urlopen
from urllib.error import URLError

from bzt import NormalShutdown, ToolError, TaurusConfigError, TaurusInternalException
from bzt.engine import Service, HavingInstallableTools, Singletone
from bzt.modules.javascript import NPMPackage, NPM
from bzt.utils import get_stacktrace, communicate, BetterDict, TaurusCalledProcessError, Environment
from bzt.utils import get_full_path, shutdown_process, shell_exec, RequiredTool, is_windows
from bzt.utils import replace_in_config, JavaVM, Node, CALL_PROBLEMS, exec_and_communicate

if not is_windows():
    try:
        from pyvirtualdisplay.smartdisplay import SmartDisplay as Display
    except ImportError:
        from pyvirtualdisplay import Display


class PipInstaller(Service):
    def __init__(self, packages=None, temp_flag=True):
        super(PipInstaller, self).__init__()
        self.packages = packages or []
        self.versions = BetterDict()
        self.engine = None
        self.temp = temp_flag
        self.target_dir = None
        self.interpreter = sys.executable
        self.pip_cmd = [self.interpreter, "-m", "pip"]

    def _check_pip(self):
        cmdline = self.pip_cmd + ["--version"]
        try:
            exec_and_communicate(cmdline)
        except TaurusCalledProcessError as exc:
            self.log.debug(exc)
            raise TaurusInternalException("pip module not found for interpreter %s" % self.interpreter)

    def _get_installed(self):
        cmdline = self.pip_cmd + ["list"]
        out, _ = exec_and_communicate(cmdline)
        out = out.split('\n')[2:-1]
        return dict(zip([line.split(' ')[0] for line in out], [line.strip().split(' ')[-1] for line in out]))

    def _missed(self, packages):
        installed = self._get_installed()
        missed = []
        for package in packages:
            if package not in installed or package in self.versions and installed[package] != self.versions[package]:
                missed.append(package)
        return missed

    def _convert_config_versions(self):
        """
        extract from packages config:
          packages:
            - one
            - two==0.0.0
            - name: three
              version: 0.0.0
        and add to self.packages and self.versions
        """
        packages_list = self.parameters.get("packages", None)
        if not packages_list:
            return

        for package_data in packages_list:
            package, version = None, None
            if isinstance(package_data, dict):
                package, version = package_data['name'], package_data.get("version", None)
            elif isinstance(package_data, str):
                package_params = package_data.split("==")
                package, version = package_params[0], package_params[1] if len(package_params) > 1 else None

            self.packages.append(package)
            if version:
                self.versions[package] = version

    def prepare_pip(self):
        """
        pip-installer expect follow definition:
        - service pip-install
          temp: false   # install to ~/.bzt instead of artifacts dir
          packages:
          - first_pkg
          - second_pkg
        """
        self._check_pip()

        self._convert_config_versions()
        if not self.packages:
            return

        # install into artifacts dir if temp, otherwise into .bzt
        self.temp = self.settings.get("temp", self.temp)
        self.temp = self.parameters.get("temp", self.temp)

        self.target_dir = self.engine.temp_pythonpath if self.temp else self.engine.user_pythonpath

        if not os.path.exists(self.target_dir):
            os.makedirs(get_full_path(self.target_dir), exist_ok=True)

    def prepare(self):
        self.prepare_pip()
        if not self.all_packages_installed():
            self.install()

    def all_packages_installed(self):
        self.packages = self._missed(self.packages)
        self.versions = {package: self.versions[package]
                         for package in self.versions.keys() if package in self.packages}
        return False if self.packages else True

    def install(self):
        if not self.packages:
            self.log.debug("Nothing to install")
            return
        cmdline = self.pip_cmd + ["install", "-t", self.target_dir]
        for package in self.packages:
            version = self.versions.get(package, None)
            cmdline += [f"{package}=={version}"] if version else [package]
        cmdline += ["--upgrade"]
        self.log.debug("pip-installer cmdline: '%s'" % ' '.join(cmdline))
        try:
            out, err = exec_and_communicate(cmdline)
        except TaurusCalledProcessError as exc:
            self.log.debug(exc)
            for line in exc.output.split('\n'):
                if line.startswith("ERROR"):
                    self.log.error(" ".join(line.split(" ")[1:]))
            return
        if "Successfully installed" in out:
            self.log.info(out.split("\n")[-2])
            for err_line in err.split("\n"):
                if err_line.startswith('WARNING'):
                    self.log.warning(" ".join(err_line.split(" ")[1:]))
                if err_line.startswith('ERROR'):
                    self.log.error(" ".join(err_line.split(" ")[1:]))
        self.log.debug("pip-installer stdout: \n%s" % out)
        if err:
            self.log.debug("pip-installer stderr:\n%s" % err)

    def get_version(self, package):
        installed = self._get_installed()
        return installed[package]

    def post_process(self):
        # might be forbidden on win as tool still work
        if self.packages and self.temp and not is_windows() and os.path.exists(self.target_dir):
            self.log.debug("remove packages: %s" % self.packages)

            shutil.rmtree(self.target_dir)  # it removes all content of directory in reality, not only self.packages


class PythonTool(RequiredTool):
    PACKAGES = []

    def __init__(self, engine, settings, **kwargs):
        tool_path = engine.temp_pythonpath
        version = settings.get("version", None)
        dry_install = settings.get("dry-install", False)
        super(PythonTool, self).__init__(tool_path=tool_path, version=version, dry_install=dry_install, **kwargs)
        self.installer = PipInstaller(temp_flag=True if version else False)
        self.installer.engine = engine
        packages = copy.deepcopy(self.PACKAGES)
        if self.version:
            packages[0] += "==" + self.version
        self.installer.parameters = BetterDict.from_dict({'packages': packages})

    def check_if_installed(self):
        self.log.debug(f"Checking {self.tool_name}.")
        self.installer.prepare_pip()
        result = self.installer.all_packages_installed()
        if not result:
            self.log.warning(f"{self.tool_name} check failed.")
        return result

    def install(self):
        if self.dry_install:
            self.log.info(f"Dry installation for {self.tool_name}")
            return
        self.log.debug(f"Installing {self.tool_name}.")
        self.installer.install()

    def get_version(self):
        return self.installer.get_version(self.PACKAGES[0])

    def post_process(self):
        self.installer.post_process()


class Unpacker(Service):
    UNPACK = 'unpacker'
    FILES = 'files'

    def __init__(self):
        super(Unpacker, self).__init__()
        self.files = []

    def prepare(self):
        packed_list = copy.deepcopy(self.parameters.get(Unpacker.FILES, self.files))
        unpacked_list = []
        for archive in packed_list:
            full_archive_path = self.engine.find_file(archive)
            self.log.debug('Unpacking %s', archive)
            with zipfile.ZipFile(full_archive_path) as zip_file:
                zip_file.extractall(self.engine.artifacts_dir)

            archive = os.path.basename(archive)
            unpacked_list.append(archive[:-4])

        replace_in_config(self.engine.config, packed_list, unpacked_list, log=self.log)


class InstallChecker(Service, Singletone):
    @staticmethod
    def _parse_module_filter(filter_value):
        if isinstance(filter_value, str):
            filter = set(filter_value.strip().split(","))
        elif isinstance(filter_value, (list, dict)):
            filter = set(filter_value)
        else:
            filter = set()
        return filter

    def prepare(self):
        modules = self.engine.config.get("modules")
        problems = []
        include_set = self._parse_module_filter(self.settings.get("include", []))
        exclude_set = self._parse_module_filter(self.settings.get("exclude", []))
        for mod_name in modules:
            if include_set and mod_name not in include_set:
                continue
            if exclude_set and mod_name in exclude_set:
                continue

            try:
                self._check_module(mod_name)
            except KeyboardInterrupt:
                raise  # let the engine handle it
            except BaseException as exc:
                self.log.error("Failed to instantiate module %s", mod_name)
                self.log.debug("%s", get_stacktrace(exc))
                problems.append(mod_name)

        if problems:
            raise ToolError("There were errors while checking for installed tools for %s, see details above" % problems)

        raise NormalShutdown("Done checking for tools installed, will exit now")

    def _check_module(self, mod_name):
        mod = self.engine.instantiate_module(mod_name)

        if not isinstance(mod, HavingInstallableTools):
            self.log.debug("Module %s has no install needs", mod_name)
            return

        self.log.info("Checking installation needs for: %s", mod_name)
        mod.install_required_tools()
        self.log.info("Module is fine: %s", mod_name)


class AndroidEmulatorLoader(Service):
    def __init__(self):
        super(AndroidEmulatorLoader, self).__init__()
        self.emulator_process = None
        self.tool_path = ''
        self.startup_timeout = None
        self.avd = ''
        self.stdout = None
        self.stderr = None

    def prepare(self):
        self.startup_timeout = self.settings.get('timeout', 30)
        config_tool_path = self.settings.get('path', '')
        if config_tool_path:
            self.tool_path = get_full_path(config_tool_path)
            os.environ['ANDROID_HOME'] = get_full_path(config_tool_path, step_up=2)
            self.settings['path'] = self.tool_path
        else:
            # try to read sdk path from env..
            sdk_path = os.environ.get('ANDROID_HOME')
            if sdk_path:
                env_tool_path = os.path.join(sdk_path, 'tools', 'emulator')
                self.tool_path = get_full_path(env_tool_path)
                self.settings['path'] = self.tool_path
            else:
                message = 'Taurus can''t find Android SDK automatically, you must point to emulator with modules.'
                message += 'android-emulator.path config parameter or set ANDROID_HOME environment variable'
                raise TaurusConfigError(message)

        tool = AndroidEmulator(tool_path=self.tool_path, log=self.log)
        if not tool.check_if_installed():
            tool.install()

    def startup(self):
        self.log.debug('Starting android emulator...')
        exc = TaurusConfigError('You must choose an emulator with modules.android-emulator.avd config parameter')
        self.avd = self.settings.get('avd', exc)
        self.stdout = open(os.path.join(self.engine.artifacts_dir, 'emulator-%s.out' % self.avd), 'ab')
        self.stderr = open(os.path.join(self.engine.artifacts_dir, 'emulator-%s.err' % self.avd), 'ab')
        self.emulator_process = shell_exec([self.tool_path, '-avd', self.avd], stdout=self.stdout, stderr=self.stderr)
        start_time = time.time()
        while not self.tool_is_started():
            time.sleep(1)
            if time.time() - start_time > self.startup_timeout:
                raise ToolError("Android emulator %s cannot be loaded" % self.avd)
        self.log.info('Android emulator %s was started successfully', self.avd)

    def tool_is_started(self):
        adb_path = os.path.join(get_full_path(self.tool_path, step_up=2), 'platform-tools', 'adb')
        if not os.path.isfile(adb_path):
            self.log.debug('adb is not found in sdk, trying to use an external one..')
            adb_path = 'adb'
        cmd = [adb_path, "shell", "getprop", "sys.boot_completed"]
        self.log.debug("Trying: %s", cmd)
        try:
            proc = shell_exec(cmd)
            out, _ = communicate(proc)
            return out.strip() == '1'
        except BaseException as exc:
            raise ToolError('Checking if android emulator starts is impossible: %s', exc)

    def shutdown(self):
        if self.emulator_process:
            self.log.debug('Stopping android emulator...')
            shutdown_process(self.emulator_process, self.log)
        if not self.stdout.closed:
            self.stdout.close()
        if not self.stderr.closed:
            self.stderr.close()
        _file = self.stdout.name
        if not os.stat(_file).st_size:
            os.remove(_file)
        _file = self.stderr.name
        if not os.stat(_file).st_size:
            os.remove(_file)


class AppiumLoader(Service):
    def __init__(self):
        super(AppiumLoader, self).__init__()
        self.env = Environment(log=self.log)
        self.appium_process = None
        self.tool_path = ''
        self.tools_dir = "~/.bzt/selenium-taurus/appium-server"
        self.default_path = None
        self.startup_timeout = None
        self.addr = ''
        self.port = ''
        self.stdout = None
        self.stderr = None
        self.appium_python = None
        self.appium_server = None

    def prepare(self):
        self.startup_timeout = self.settings.get('timeout', 30)
        self.addr = self.settings.get('addr', '127.0.0.1')
        self.port = self.settings.get('port', 4723)
        self.tool_path = self.settings.get('path', 'appium')
        self.tools_dir = get_full_path(self.settings.get("tools-dir", self.tools_dir))
        self.default_path = os.path.join(self.tools_dir, "node_modules/.bin/appium")
        self.env.add_path({"NODE_PATH": os.path.join(self.tools_dir, "node_modules")})

        self.install_required_tools()

    def install_required_tools(self):
        node = Node(env=self.env, log=self.log)
        npm = NPM(env=self.env, log=self.log)
        self.appium_python = AppiumPython(engine=self.engine, settings=self.settings, log=self.log)
        self.appium_server = AppiumServer(path=self.tool_path, def_path=self.default_path, tools_dir=self.tools_dir,
                                          node_tool=node, npm_tool=npm)
        required_tools = [node, npm, JavaVM(log=self.log), self.appium_python, self.appium_server]
        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

    def startup(self):
        self.log.debug('Starting Appium...')
        self.stdout = open(os.path.join(self.engine.artifacts_dir, 'appium.out'), 'wt')
        self.stderr = open(os.path.join(self.engine.artifacts_dir, 'appium.err'), 'wt')
        self.appium_process = shell_exec([self.appium_server.tool_path, "--log-no-colors"],
                                         stdout=self.stdout, stderr=self.stderr)

        start_time = time.time()
        while not self.tool_is_started():
            time.sleep(1)
            if time.time() - start_time > self.startup_timeout:
                raise ToolError("Appium cannot be loaded")

        self.log.info('Appium was started successfully')

    def tool_is_started(self):
        try:
            response = urlopen("http://%s:%s%s" % (self.addr, self.port, '/wd/hub/sessions'))
            resp_str = response.read()
            if not isinstance(resp_str, str):
                resp_str = resp_str.decode()
            return isinstance(json.loads(resp_str), dict)
        except (URLError, ValueError):
            return False

    def shutdown(self):
        if self.appium_process:
            self.log.debug('Stopping appium...')
            shutdown_process(self.appium_process, self.log)
        if not self.stdout.closed:
            self.stdout.close()
        if not self.stderr.closed:
            self.stderr.close()
        _file = self.stdout.name
        if not os.stat(_file).st_size:
            os.remove(_file)
        _file = self.stderr.name
        if not os.stat(_file).st_size:
            os.remove(_file)

    def post_process(self):
        self.appium_python.post_process()


class AppiumPython(PythonTool):
    PACKAGES = ["Appium-Python-Client"]


class AppiumServer(NPMPackage):
    PACKAGE_NAME = "appium@1.22.0"

    def __init__(self, path=None, def_path=None, **kwargs):
        super(AppiumServer, self).__init__(**kwargs)
        self.tool_path = path
        self.default_path = def_path

    def check_if_installed(self):
        self.log.debug("Trying %s...", self.tool_name)
        if not self._check_path(self.tool_path):
            self.tool_path = self.default_path
            return self._check_path(self.default_path)
        return True

    def _check_path(self, path):
        cmd = [path, '--version']
        try:
            out, err = exec_and_communicate(cmd)
        except CALL_PROBLEMS as exc:
            self.log.debug("Failed to check %s: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)
        return True


class AndroidEmulator(RequiredTool):
    def __init__(self, **kwargs):
        super(AndroidEmulator, self).__init__(installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug("Trying %s...", self.tool_name)

        cmd = [self.tool_path, '-list-avds']
        try:
            output = exec_and_communicate(cmd)
        except CALL_PROBLEMS as exc:
            self.log.debug("Failed to check %s: %s", self.tool_name, exc)
            return False

        self.log.debug("%s output: %s", self.tool_name, output)
        return True


class VirtualDisplay(Service, Singletone):
    """
    Selenium executor
    :type virtual_display: Display
    """

    SHARED_VIRTUAL_DISPLAY = None

    def __init__(self):
        super(VirtualDisplay, self).__init__()
        self.virtual_display = None

    def get_virtual_display(self):
        return self.virtual_display

    def set_virtual_display(self):
        if is_windows():
            self.log.warning("Cannot have virtual display on Windows, ignoring")
            return

        if VirtualDisplay.SHARED_VIRTUAL_DISPLAY:
            self.virtual_display = VirtualDisplay.SHARED_VIRTUAL_DISPLAY
        else:
            width = self.parameters.get("width", 1024)
            height = self.parameters.get("height", 768)
            self.virtual_display = Display(size=(width, height))
            self.virtual_display.start()
            msg = "Starting virtual display[%s] %s"
            self.log.info(msg, (width, height), self.virtual_display.new_display_var)
            VirtualDisplay.SHARED_VIRTUAL_DISPLAY = self.virtual_display

            self.engine.shared_env.set({'DISPLAY': os.environ['DISPLAY']})  # backward compatibility

    def free_virtual_display(self):
        if self.virtual_display and self.virtual_display.is_alive():
            self.virtual_display.stop()
        if VirtualDisplay.SHARED_VIRTUAL_DISPLAY:
            VirtualDisplay.SHARED_VIRTUAL_DISPLAY = None

    def startup(self):
        self.set_virtual_display()

    def check_virtual_display(self):
        if self.virtual_display:
            if not self.virtual_display.is_alive():
                self.log.info("Virtual display out: %s", self.virtual_display.stdout)
                self.log.warning("Virtual display err: %s", self.virtual_display.stderr)
                raise TaurusInternalException("Virtual display failed: %s" % self.virtual_display.return_code)

    def check(self):
        self.check_virtual_display()
        return False

    def shutdown(self):
        self.free_virtual_display()
