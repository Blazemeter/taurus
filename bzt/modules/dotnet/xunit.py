"""
Copyright 2018 BlazeMeter Inc.

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
from subprocess import check_output, STDOUT, CalledProcessError

from bzt import TaurusConfigError, ToolError
from bzt.modules import SubprocessedExecutor
from bzt.engine import HavingInstallableTools
from bzt.utils import get_full_path, RequiredTool, is_windows


class XUnitExecutor(SubprocessedExecutor, HavingInstallableTools):
    def __init__(self):
        super(XUnitExecutor, self).__init__()
        self.runner_dir = os.path.join(get_full_path(__file__, step_up=3), "resources", "XUnitRunner")
        self.runner_executable = os.path.join(self.runner_dir, "XUnitRunner.dll")
        self.dotnet = Dotnet("dotnet", "", self.log)

    def install_required_tools(self):
        if not is_windows():
            if not self.dotnet.check_if_installed():
                self.dotnet.install()

    def prepare(self):
        super(XUnitExecutor, self).prepare()
        self.script = get_full_path(self.get_script_path())
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)

        self.install_required_tools()
        self.reporting_setup(suffix=".ldjson")

    def startup(self):
        cmdline = [self.dotnet.tool_path]

        cmdline += [self.runner_executable,
                    "--target=" + self.script,
                    "--report-file=" + self.report_file]

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iterations=' + str(load.iterations)]
        if load.hold:
            cmdline += ['--duration=' + str(int(load.hold))]

        self._start_subprocess(cmdline)


class Dotnet(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(Dotnet, self).__init__(".NET Core CLI", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug("Checking for Dotnet")
        try:
            output = check_output([self.tool_path, '--version'], stderr=STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("%s is not operable or not available. Consider installing it" % self.tool_name)
