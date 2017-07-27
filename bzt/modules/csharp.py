"""
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

from bzt import TaurusConfigError
from bzt.modules import SubprocessedExecutor
from bzt.engine import HavingInstallableTools
from bzt.utils import get_full_path


class NUnitExecutor(SubprocessedExecutor, HavingInstallableTools):
    def install_required_tools(self):
        # TODO: check for mono (in case of non-windows) and nuget
        # TODO: `nuget install NUNit` and NUnit.Console
        # TODO: `nuget install Selenium?`
        pass

    def prepare(self):
        super(NUnitExecutor, self).prepare()
        self.script = get_full_path(self.get_script_path())
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)

        self.install_required_tools()
        self.reporting_setup(suffix=".ldjson")

    def _collect_files(self, extensions):
        file_list = []
        if os.path.isdir(self.script):
            for root, _, files in os.walk(self.script):
                for test_file in files:
                    if os.path.splitext(test_file)[1].lower() in extensions:
                        path = get_full_path(os.path.join(root, test_file))
                        file_list.append(path)
        else:
            if os.path.splitext(self.script)[1].lower() in extensions:
                file_list.append(get_full_path(self.script))
        return file_list

    def startup(self):
        # mono packages/NUnit.ConsoleRunner.3.7.0/tools/nunit3-console.exe
        #   SeleniumSuite/bin/Release/SeleniumSuite.dll -noresult
        script_dir = self.script
        target_assembly = self._collect_files({".dll"})
        cmdline = ["packages/NUnit.ConsoleRunner.3.7.0/tools/nunit3-console.exe"]
        cmdline += target_assembly
        cmdline += ["-noresult"]
        self._start_subprocess(cmdline, cwd=script_dir)
