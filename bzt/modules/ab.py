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

import platform
import time
import traceback
import subprocess
from subprocess import CalledProcessError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ResultsReader, ConsolidatingAggregator
from bzt.utils import shutdown_process, shell_exec, RequiredTool

EXE_SUFFIX = ".exe" if platform.system() == "Windows" else ""


class ABExecutor(ScenarioExecutor):
    """
    AB executor module
    """

    def __init__(self):
        super(ABExecutor, self).__init__()
        self.process = None
        self.start_time = None
        self.end_time = None
        self.reader = None
        self.log.warning("___ABExecutor.__init__!")

    def prepare(self):
        self._check_installed()
        self.log.warning("___ABExecutor.prepare!")
        self.reader = DataLogReader(self.engine.artifacts_dir, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def _check_installed(self):
        ab_path = "ab" + EXE_SUFFIX
        required_tools = [AB(ab_path, self.log)]
        for tool in required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s", tool.tool_name)
                tool.install()

    def startup(self):
        self.log.warning("___ABExecutor.startup!")

        self.start_time = time.time()

        concurrency = self.execution.get("concurrency", 1)
        iterations = self.execution.get("iterations", 1)
        requests = self.get_scenario().get("requests", ["http://blazedemo.com"])

        request = requests[0] + '/'  # TODO: process list of requests

        cmd_line = "ab -c %s -n %s %s" % (concurrency, iterations, request)

        ab = shell_exec(cmd_line, stderr=subprocess.STDOUT)
        ab_out, ab_err = ab.communicate()
        key = 'Time taken for tests:'
        pos = ab_out.find(key) + len(key)
        test_time = float(ab_out[pos:pos+100].split()[0])

        self.reader.output = [int(time.time()), request, concurrency, test_time, 0, 0, 0, None, '']

    def _val(self, outlist, key):
        return outlist[outlist.index(key) + 1]

    def check(self):
        self.log.warning("___ABExecutor.check!")
        return True

    def shutdown(self):
        self.log.warning("___ABExecutor.shutdown!")
        #shutdown_process(self.process, self.log)

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("AB worked for %s seconds", self.end_time - self.start_time)


class DataLogReader(ResultsReader):
    """ Class to read ___ """

    def __init__(self, basedir, parent_logger):
        super(DataLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.output = None

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :param last_pass:
        :return: timestamp, label, concurrency, rt, latency, rc, error
        """
        if not last_pass:
            raise StopIteration
        yield self.output


class AB(RequiredTool):
    """
    AB tool
    """
    def __init__(self, tool_path, parent_logger):
        super(AB, self).__init__("AB", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug("Trying AB: %s", self.tool_path)
        try:
            ab = shell_exec([self.tool_path], stderr=subprocess.STDOUT)
            ab_out, ab_err = ab.communicate()
            self.log.debug("AB check: %s", ab_out)
            if ab_err:
                self.log.warning("AB check stderr: %s", ab_err)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("Check failed: %s", traceback.format_exc())
            self.log.error("Apache Benchmark check failed. Consider installing it")
            return False

    def install(self):
        raise RuntimeError("Please install Apache Benchmark tool manually")
