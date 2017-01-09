"""
Implementation for Appium tool

Copyright 2017 BlazeMeter Inc.

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
import time

from bzt.engine import Service
from bzt.utils import get_full_path, shutdown_process, shell_exec, RequiredTool
from bzt.modules.selenium import Node, JavaVM


class AppiumTools(Service):
    def __init__(self):
        super(AppiumTools, self).__init__()
        self.appium_process = None
        self.emulator_process = None
        self.sdk_path = ''

    def prepare(self):
        self.sdk_path = self.settings.get('sdk-path', '~/.bzt/android-sdk')
        self.sdk_path = get_full_path(self.sdk_path)
        self.settings['sdk-path'] = self.sdk_path
        required_tools = [Node(),
                          JavaVM(),
                          Appium(),
                          AndroidSDK()]  # todo: iOS

        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()
                # todo: set up $ANDROID_HOME

    def startup(self):
        self.log.debug('Starting appium...')
        self.appium_process = shell_exec(['appium'])

        self.log.debug('Starting android emulator...')
        emulator_path = get_full_path(os.path.join(self.sdk_path, 'tools/emulator'))
        self.emulator_process = shell_exec([emulator_path, '-avd', 'nexus_and7_x86'])

        time.sleep(3)

    def shutdown(self):
        if self.appium_process:
            self.log.debug('Stopping appium...')
            shutdown_process(self.appium_process, self.log)
        if self.emulator_process:
            self.log.debug('Stopping android emulator...')
            shutdown_process(self.emulator_process, self.log)


class Appium(RequiredTool):
    def check_if_installed(self):
        return True

    def install(self):
        pass


class AndroidSDK(RequiredTool):
    def check_if_installed(self):
        return True

    def install(self):
        pass
