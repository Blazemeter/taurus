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

from bzt.utils import RequiredTool, CALL_PROBLEMS


class TaurusPytestRunner(RequiredTool):
    def __init__(self, tool_path, **kwargs):
        super(TaurusPytestRunner, self).__init__(tool_path=tool_path, installable=False, **kwargs)


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
            if err:
                out += err
            self.log.debug("Robot output: %s", out)
            return True
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False
