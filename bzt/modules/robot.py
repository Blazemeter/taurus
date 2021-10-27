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
from bzt.modules import SubprocessedExecutor
from bzt.modules.services import PythonTool
from bzt.utils import RequiredTool, CALL_PROBLEMS
from bzt.utils import get_full_path, RESOURCES_DIR


class RobotExecutor(SubprocessedExecutor):
    def __init__(self):
        super(RobotExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "robot_runner.py")
        self.variables_file = None
        self.output_file = None
        self.log_file = None
        self.tags = None
        self.robot = None

    def resource_files(self):
        files = super(RobotExecutor, self).resource_files()
        scenario = self.get_scenario()
        if "variables" in scenario and isinstance(scenario["variables"], str):
            files.append(scenario["variables"])
        return files

    def prepare(self):
        super(RobotExecutor, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for robot executor")

        self.reporting_setup(suffix=".ldjson")
        self.output_file = self.engine.create_artifact("output", ".xml")
        self.log_file = self.engine.create_artifact("log", ".html")

        scenario = self.get_scenario()
        variables = scenario.get("variables")
        if variables:
            if isinstance(variables, str):
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
            if isinstance(tags, str):
                self.tags = tags
            else:
                raise TaurusConfigError("`tags` is not a string or text")

    def install_required_tools(self):
        self.robot = self._get_tool(Robot, engine=self.engine, settings=self.settings)
        self._check_tools([self.robot, self._get_tool(TaurusRobotRunner, tool_path=self.runner_path)])

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

        cmdline += ['--outputfile', self.output_file]
        cmdline += ['--logfile', self.log_file]

        user_cmd = self.settings.get("cmdline")
        if user_cmd:
            cmdline.append("--cmdline")
            cmdline.append(f'"{user_cmd.replace("=", " ").replace("-", "")}"')

        cmdline += [self.script]
        self.process = self._execute(cmdline)

    def post_process(self):
        self.robot.post_process()
        super(RobotExecutor, self).post_process()


class Robot(PythonTool):
    PACKAGES = ["robotframework", "apiritif", "robotframework-seleniumlibrary"]


class TaurusRobotRunner(RequiredTool):
    def __init__(self, tool_path, **kwargs):
        super(TaurusRobotRunner, self).__init__(tool_path=tool_path, installable=False, **kwargs)
