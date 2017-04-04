import subprocess
import traceback

import os
from bzt import ToolError

from bzt.engine import SubprocessedExecutor, HavingInstallableTools
from bzt.utils import RequiredTool, is_windows, get_full_path, TclLibrary


class RSpecTester(SubprocessedExecutor, HavingInstallableTools):
    """
    RSpec tests runner
    """

    def __init__(self, rspec_config, executor):
        super(RSpecTester, self).__init__(rspec_config, executor)
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "rspec_taurus_plugin.rb")

    def prepare(self):
        self.run_checklist()

    def run_checklist(self):
        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(Ruby(self.settings.get("interpreter", "ruby"), "", self.log))
        self.required_tools.append(RSpec("", "", self.log))
        self.required_tools.append(TaurusRSpecPlugin(self.plugin_path, ""))

        self._check_tools()

    def run_tests(self):
        """
        run rspec plugin
        """
        interpreter = self.settings.get("interpreter", "ruby")

        rspec_cmdline = [
            interpreter,
            self.plugin_path,
            "--report-file",
            self.settings.get("report-file"),
            "--test-suite",
            self.script
        ]

        if self.load.iterations:
            rspec_cmdline += ['--iterations', str(self.load.iterations)]

        if self.load.hold:
            rspec_cmdline += ['--hold-for', str(self.load.hold)]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.process = self.executor.execute(rspec_cmdline,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=self.env)

    def is_finished(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            self.log.debug("Test runner exit code: %s", ret_code)
            # rspec returns non-zero code when some tests fail, no need to throw an exception here
            if ret_code != 0:
                self.is_failed = True
            return True
        return False


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
