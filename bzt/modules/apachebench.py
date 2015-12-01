"""
Module holds all stuff regarding Apache Benchmark tool usage

Copyright 2015 BlazeMeter Inc.

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

import subprocess
import traceback
from subprocess import CalledProcessError

from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ResultsReader
from bzt.utils import shell_exec, RequiredTool


class ApacheBenchExecutor(ScenarioExecutor):
    """
    ApacheBench executor module
    """
    def prepare(self):
        self._check_installed()
        pass

    def _check_installed(self):
        pass

    def startup(self):
        pass

    def check(self):
        pass

    def shutdown(self):
        pass


class DataLogReader(ResultsReader):
    """ Class to read ___ """
    def _read(self, last_pass=False):
        pass

    def _calculate_datapoints(self, final_pass=False):
        pass


class ApacheBench(RequiredTool):
    """
    ApacheBench tool
    """
    def __init__(self, parent_logger, tool_path):
        super(ApacheBench, self).__init__("ApacheBench", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug("Trying ApacheBench: %s", self.tool_path)
        try:
            apache_bench = shell_exec([self.tool_path], stderr=subprocess.STDOUT)
            apache_bench_out, apache_bench_err = apache_bench.communicate()
            self.log.debug("ApacheBench check: %s", apache_bench_out)
            if apache_bench_err:
                self.log.warning("ApacheBench check stderr: %s", apache_bench_err)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("Check failed: %s", traceback.format_exc())
            self.log.error("Apache Benchmark check failed. Consider installing it")
            return False

    def install(self):
        raise RuntimeError("Please install Apache Benchmark tool manually")

