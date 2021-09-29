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

from bzt import TaurusConfigError, TaurusInternalException
from bzt.modules import SubprocessedExecutor
from bzt.utils import get_full_path, is_windows, RequiredTool, RESOURCES_DIR, CALL_PROBLEMS


class CSharpExecutor(SubprocessedExecutor):
    def __init__(self):
        super(CSharpExecutor, self).__init__()
        self.runner_dir = os.path.join(RESOURCES_DIR, "dotnet", "DotnetTestRunner")
        self.dotnet = None
        self.executor_name = None
        self.runner_executable = "dotnet"

    def install_required_tools(self):
        if is_windows():
            return

        self.dotnet = self._get_tool(Dotnet)
        self.log.debug("Checking for dotnet")
        if not self.dotnet.check_if_installed():
            self.dotnet.install()

    def prepare(self):
        super(CSharpExecutor, self).prepare()
        self.env.set({"CopyRetryCount": 100})
        self.script = get_full_path(self.get_script_path())
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)

        self.install_required_tools()
        self.reporting_setup(suffix=".ldjson")

    def startup(self):
        if not self.executor_name:
            raise TaurusInternalException("C# executor is not specified, use NUnit or XUnit instead.")

        cmdline = []

        cmdline += [self.runner_executable, "run", "--project", self.runner_dir, "--", self.executor_name,
                    "--target", self.script,
                    "--report-file", self.report_file]

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]
        if load.hold:
            cmdline += ['--duration', str(int(load.hold))]
        if load.concurrency:
            cmdline += ['--concurrency', str(int(load.concurrency))]
        if load.ramp_up:
            cmdline += ['--ramp-up', str(int(load.ramp_up))]

        self.process = self._execute(cmdline)


class NUnitExecutor(CSharpExecutor):
    def __init__(self):
        super(NUnitExecutor, self).__init__()
        self.executor_name = "nUnit"


class XUnitExecutor(CSharpExecutor):
    def __init__(self):
        super(XUnitExecutor, self).__init__()
        self.executor_name = "xUnit"


class Dotnet(RequiredTool):
    def __init__(self, **kwargs):
        super(Dotnet, self).__init__(tool_path="dotnet", installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug('Trying %s: %s', self.tool_name, self.tool_path)
        try:
            out, err = self.call([self.tool_path, '--version'])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        self.log.debug("%s check stdout: %s", self.tool_name, out)
        if err:
            self.log.warning("%s check stderr: %s", self.tool_name, err)
        return True
