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

from subprocess import CalledProcessError

from bzt import ToolError
from bzt.utils import RequiredTool
from bzt.utils import shell_exec


class TaurusPytestRunner(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusPytestRunner, self).__init__("TaurusPytestRunner", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus pytest runner isn't implemented")


class TaurusRobotRunner(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusRobotRunner, self).__init__("TaurusRobotRunner", tool_path, download_link)

    def install(self):
        raise ToolError("Robot Taurus runner should've been included in Taurus distribution")


class Robot(RequiredTool):
    def __init__(self, python_executable, parent_logger):
        super(Robot, self).__init__("RobotFramework", "")
        self.python_executable = python_executable
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking Robot Framework: %s' % self.tool_path)
        try:
            checker = shell_exec([self.python_executable, '-c', 'import robot; print(robot.__version__)'])
            output = checker.communicate()
            self.log.debug("Robot output: %s", output)
            if checker.returncode != 0:
                return False
        except (CalledProcessError, OSError):
            return False
        return True

    def install(self):
        raise ToolError("You must install robot framework")
