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
import sys

import yaml

from bzt import TaurusConfigError
from bzt.engine import HavingInstallableTools
from bzt.modules import SubprocessedExecutor
from bzt.six import string_types, text_type
from bzt.utils import RequiredTool, CALL_PROBLEMS
from bzt.utils import get_full_path, RESOURCES_DIR


class RobotExecutor(SubprocessedExecutor, HavingInstallableTools):
    def __init__(self):
        super(RobotExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "robot_runner.py")
        self.variables_file = None
        self.tags = None

    def resource_files(self):
        files = super(RobotExecutor, self).resource_files()
        scenario = self.get_scenario()
        if "variables" in scenario and isinstance(scenario["variables"], (string_types, text_type)):
            files.append(scenario["variables"])
        return files

    def prepare(self):
        super(RobotExecutor, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for robot executor")

        self.reporting_setup(suffix=".ldjson")

        scenario = self.get_scenario()
        variables = scenario.get("variables")
        if variables:
            if isinstance(variables, (string_types, text_type)):
                self.variables_file = get_full_path(variables)
            elif isinstance(variables, dict):
                self.variables_file = self.engine.create_artifact("robot-vars", ".yaml")
                with open(self.variables_file, 'wb') as fds:
                    yml = yaml.safe_dump(variables,
                                         default_flow_style=False, explicit_start=True, canonical=False,
                                         allow_unicode=True,
                                         encoding='utf-8', width=float("inf"))
                    fds.write(yml)
            else:
                raise TaurusConfigError("`variables` is neither file nor dict")
        tags = scenario.get("tags", None)
        if tags:
            if isinstance(tags, (string_types, text_type)):
                self.tags = tags
            else:
                raise TaurusConfigError("`tags` is not a string or text")

    def install_required_tools(self):
        tools = [self._get_tool(TaurusRobotRunner, tool_path=self.runner_path),
                 self._get_tool(Robot, python=self.settings.get("interpreter", sys.executable))]
        self._check_tools(tools)

    def startup(self):
        executable = self.settings.get("interpreter", sys.executable)

        cmdline = [executable, self.runner_path, '--report-file', self.report_file]

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            cmdline += ['--duration', str(load.hold)]

        if self.variables_file is not None:
            cmdline += ['--variablefile', self.variables_file]

        if self.tags is not None:
            cmdline += ['--include', self.tags]

        cmdline += [self.script]
        self.process = self._execute(cmdline)


class TaurusRobotRunner(RequiredTool):
    def __init__(self, tool_path, **kwargs):
        super(TaurusRobotRunner, self).__init__(tool_path=tool_path, installable=False, **kwargs)


class Robot(RequiredTool):
    def __init__(self, python, **kwargs):
        super(Robot, self).__init__(installable=False, **kwargs)
        self.python = python

    def check_if_installed(self):
        self.log.debug('Checking Robot Framework: %s' % self.tool_path)
        try:
            out, err = self.call([self.python, '-c', 'import robot; print(robot.__version__)'])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("Robot output: %s", out)
        return True
