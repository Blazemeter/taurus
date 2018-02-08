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
import subprocess
import time
import zipfile

from bzt.six import communicate

try:
    from pyvirtualdisplay.smartdisplay import SmartDisplay as Display
except ImportError:
    from pyvirtualdisplay import Display

from bzt import NormalShutdown, ToolError, TaurusConfigError, TaurusInternalException
from bzt.engine import Service, HavingInstallableTools, Singletone
from bzt.six import get_stacktrace, urlopen, URLError
from bzt.utils import get_full_path, shutdown_process, shell_exec, RequiredTool, is_windows
from bzt.utils import replace_in_config, JavaVM, Node


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
            unpacked_list.append(archive[:-4])  # TODO: replace with top-level archive content

        replace_in_config(self.engine.config, packed_list, unpacked_list, log=self.log)


class InstallChecker(Service, Singletone):
    def prepare(self):
        modules = self.engine.config.get("modules")
        problems = []
        for mod_name in modules.keys():
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

        tool = AndroidEmulator(self.tool_path, "", self.log)
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
        self.appium_process = None
        self.tool_path = ''
        self.startup_timeout = None
        self.addr = ''
        self.port = ''
        self.stdout = None
        self.stderr = None

    def prepare(self):
        self.startup_timeout = self.settings.get('timeout', 30)
        self.addr = self.settings.get('addr', '127.0.0.1')
        self.port = self.settings.get('port', 4723)
        self.tool_path = self.settings.get('path', 'appium')

        required_tools = [Node(self.log),
                          JavaVM(self.log),
                          Appium(self.tool_path, "", self.log)]

        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

    def startup(self):
        self.log.debug('Starting appium...')
        self.stdout = open(os.path.join(self.engine.artifacts_dir, 'appium.out'), 'ab')
        self.stderr = open(os.path.join(self.engine.artifacts_dir, 'appium.err'), 'ab')
        self.appium_process = shell_exec([self.tool_path], stdout=self.stdout, stderr=self.stderr)

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


class Appium(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(Appium, self).__init__("Appium", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        cmd = [self.tool_path, '--version']
        self.log.debug("Trying %s: %s", self.tool_name, cmd)
        try:
            output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (subprocess.CalledProcessError, OSError) as exc:
            self.log.debug("Failed to check %s: %s", self.tool_name, exc)
            return False

    def install(self):
        raise ToolError("Automatic installation of %s is not implemented. Install it manually" % self.tool_name)


class AndroidEmulator(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(AndroidEmulator, self).__init__("AndroidEmulator", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        cmd = [self.tool_path, '-list-avds']
        self.log.debug("Trying %s: %s", self.tool_name, cmd)
        try:
            output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (subprocess.CalledProcessError, OSError) as exc:
            self.log.debug("Failed to check %s: %s", self.tool_name, exc)
            return False

    def install(self):
        raise ToolError("Automatic installation of %s is not implemented. Install it manually" % self.tool_name)


class VirtualDisplay(Service, Singletone):
    """
    Selenium executor
    :type virtual_display: Display
    """

    SHARED_VIRTUAL_DISPLAY = {}

    def __init__(self):
        super(VirtualDisplay, self).__init__()
        self.virtual_display = None

    def get_virtual_display(self):
        return self.virtual_display

    def set_virtual_display(self):
        if is_windows():
            self.log.warning("Cannot have virtual display on Windows, ignoring")
            return

        if self.engine in VirtualDisplay.SHARED_VIRTUAL_DISPLAY:
            self.virtual_display = VirtualDisplay.SHARED_VIRTUAL_DISPLAY[self.engine]
        else:
            width = self.parameters.get("width", 1024)
            height = self.parameters.get("height", 768)
            self.virtual_display = Display(size=(width, height))
            msg = "Starting virtual display[%s]: %s"
            self.log.info(msg, self.virtual_display.size, self.virtual_display.new_display_var)
            self.virtual_display.start()

            # roll DISPLAY back for online report browser
            if self.virtual_display.old_display_var:
                os.environ["DISPLAY"] = self.virtual_display.old_display_var
            else:
                del os.environ["DISPLAY"]

            VirtualDisplay.SHARED_VIRTUAL_DISPLAY[self.engine] = self.virtual_display
            self.engine.shared_env.set({"DISPLAY": self.virtual_display.new_display_var})

    def free_virtual_display(self):
        if self.virtual_display and self.virtual_display.is_alive():
            os.environ["DISPLAY"] = self.virtual_display.new_display_var
            self.virtual_display.stop()
        if self.engine in VirtualDisplay.SHARED_VIRTUAL_DISPLAY:
            del VirtualDisplay.SHARED_VIRTUAL_DISPLAY[self.engine]
            self.engine.shared_env.set({"DISPLAY": None})

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
