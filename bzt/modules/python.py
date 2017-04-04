
class NoseTester(AbstractSeleniumExecutor):
    """
    Python selenium tests runner
    """

    def __init__(self, nose_config, executor):
        super(NoseTester, self).__init__(nose_config, executor)
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "nose_plugin.py")

    def prepare(self):
        self.run_checklist()

    def run_checklist(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warning("You are using python3, make sure that your scripts are able to run in python3!")

        self.required_tools.append(TclLibrary(self.log))
        self.required_tools.append(TaurusNosePlugin(self.plugin_path, ""))

        self.check_tools()

    def run_tests(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, '--report-file', self.settings.get("report-file")]

        if self.load.iterations:
            nose_command_line += ['-i', str(self.load.iterations)]

        if self.load.hold:
            nose_command_line += ['-d', str(self.load.hold)]

        nose_command_line += [self.script]

        std_out = open(self.settings.get("stdout"), "wt")
        self.opened_descriptors.append(std_out)
        std_err = open(self.settings.get("stderr"), "wt")
        self.opened_descriptors.append(std_err)

        self.process = self.executor.execute(nose_command_line,
                                             stdout=std_out,
                                             stderr=std_err,
                                             env=self.env)


class TaurusNosePlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNosePlugin, self).__init__("TaurusNosePlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus nose plugin isn't implemented")

