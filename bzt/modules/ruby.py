import subprocess
import traceback

import os
from bzt import ToolError, TaurusConfigError

from bzt.engine import SubprocessedExecutor, HavingInstallableTools
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

    def prepare(self):
        super(RSpecTester, self).prepare()
        self.install_required_tools()

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
            self.execution.get("report-file"),
            "--test-suite",
            self.get_scenario().get("script", TaurusConfigError("No script specified"))
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
            output = subprocess.check_output([self.tool_path, '--version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (subprocess.CalledProcessError, OSError):
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
            output = subprocess.check_output([rspec_exec, '--version'], stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (subprocess.CalledProcessError, OSError):
            self.log.debug("RSpec check exception: %s", traceback.format_exc())
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class TaurusRSpecPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusRSpecPlugin, self).__init__("TaurusRSpecPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus RSpec plugin isn't implemented")
