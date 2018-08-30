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
from subprocess import check_output, CalledProcessError, STDOUT

from bzt import ToolError
from bzt.utils import RequiredTool


class Mono(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(Mono, self).__init__("Mono", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            output = check_output([self.tool_path, '--version'], stderr=STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            return False

    def install(self):
        raise ToolError("%s is not operable or not available. Consider installing it" % self.tool_name)


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
