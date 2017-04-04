import sys

import os
from bzt import ToolError

from bzt.engine import SubprocessedExecutor, HavingInstallableTools
from bzt.utils import get_full_path, TclLibrary, RequiredTool


class NoseTester(SubprocessedExecutor, HavingInstallableTools):
    """
    Python selenium tests runner
    """

    def __init__(self):
        super(NoseTester, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "nose_plugin.py")

    def prepare(self):
        super(NoseTester, self).prepare()
        self.install_required_tools()
        self.script = self.settings.get("script", self.script)

    def install_required_tools(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warning("You are using python3, make sure that your scripts are able to run in python3!")

        self._check_tools([TclLibrary(self.log), TaurusNosePlugin(self.plugin_path, "")])

    def startup(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, '--report-file', self.execution.get("report-file")]

        load = self.get_load()
        if load.iterations:
            nose_command_line += ['-i', str(load.iterations)]

        if load.hold:
            nose_command_line += ['-d', str(load.hold)]

        nose_command_line += [self.script]
        self._start_subprocess(nose_command_line)


class TaurusNosePlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNosePlugin, self).__init__("TaurusNosePlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus nose plugin isn't implemented")
