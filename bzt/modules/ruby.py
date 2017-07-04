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
import traceback
import os

from subprocess import CalledProcessError, check_output, STDOUT

from bzt import ToolError, TaurusConfigError
from bzt.modules import SubprocessedExecutor
from bzt.engine import HavingInstallableTools
from bzt.utils import RequiredTool, is_windows, get_full_path, TclLibrary


class RSpecTester(SubprocessedExecutor, HavingInstallableTools):
    """
    RSpec tests runner
    """

    def __init__(self):
        super(RSpecTester, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "rspec_taurus_plugin.rb")
        self.script = None

    def prepare(self):
        super(RSpecTester, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)

        self.reporting_setup(suffix='.ldjson')

    def install_required_tools(self):
        tools = []
        tools.append(TclLibrary(self.log))
        tools.append(Ruby(self.settings.get("interpreter", "ruby"), "", self.log))
        tools.append(RSpec("", "", self.log))
        tools.append(TaurusRSpecPlugin(self.plugin_path, ""))
        self._check_tools(tools)

    def startup(self):
        """
        run rspec plugin
        """
        interpreter = self.settings.get("interpreter", "ruby")

        rspec_cmdline = [
            interpreter,
            self.plugin_path,
            "--report-file",
            self.report_file,
            "--test-suite",
            self.script
        ]
        load = self.get_load()
        if load.iterations:
            rspec_cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            rspec_cmdline += ['--hold-for', str(load.hold)]

        self._start_subprocess(rspec_cmdline)


class Ruby(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(Ruby, self).__init__("Ruby", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = check_output([self.tool_path, '--version'], stderr=STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class RSpec(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(RSpec, self).__init__("RSpec", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            rspec_exec = "rspec.bat" if is_windows() else "rspec"
            output = check_output([rspec_exec, '--version'], stderr=STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("RSpec check exception: %s", traceback.format_exc())
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class TaurusRSpecPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusRSpecPlugin, self).__init__("TaurusRSpecPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus RSpec plugin isn't implemented")
