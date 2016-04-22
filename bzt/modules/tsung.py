"""
Module holds all stuff regarding usage of Apache Benchmark

Copyright 2016 BlazeMeter Inc.

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

import logging
import time
from os import path

from bzt.engine import FileLister, Scenario, ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.utils import shell_exec, shutdown_process, RequiredTool


class TsungExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Tsung executor module
    """

    def __init__(self):
        super(TsungExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__out = None
        self.__err = None
        self.__stats_file = None
        self.tsung_config = None
        self.tool_path = None
        self.stats_reader = None
        self.start_time = None
        self.widget = None

    def prepare(self):
        scenario = self.get_scenario()
        self.tool_path = self._check_installed()

        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            self.tsung_config = self._get_script()
        elif scenario.get("requests"):
            self.tsung_config = self._generate_tsung_config()
        else:
            raise ValueError("You must specify either a script or a list of requests to run Tsung")

        # TODO: create stats dir

        self.__stats_file = self.engine.create_artifact("tsung-stats", ".log")

        self.stats_reader = TsungStatsReader(self.__stats_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.stats_reader)

        self.__out = open(self.engine.create_artifact("tsung", ".out"), 'w')
        self.__err = open(self.engine.create_artifact("tsung", ".err"), 'w')

    def _get_script(self):
        """

        :return: script path
        """
        scenario = self.get_scenario()
        if Scenario.SCRIPT not in scenario:
            return None

        fname = scenario[Scenario.SCRIPT]
        if fname is not None:
            return self.engine.find_file(fname)
        else:
            return None

    def _generate_tsung_config(self):
        config_file = self.engine.create_artifact("tsung-config", ".xml")
        scenario = self.get_scenario()
        load = self.get_load()
        raise NotImplementedError("Tsung._generate_tsung_config()")

    def startup(self):
        args = [self.tool_path]
        args += ['-f', self.tsung_config]
        args += ['-l', self.engine.artifacts_dir]
        args += ['-n']  # TODO: configurable?
        args += ['-w', '0']
        args += ['-p', '1000000']  # TODO: configurable?
        args += ['start']
        self.start_time = time.time()
        self.process = self.execute(args, stdout=self.__out, stderr=self.__err)

    def check(self):
        if self.widget:
            self.widget.update()

        ret_code = self.process.poll()
        if ret_code is None:
            return False
        self.log.info("tsung exit code: %s", ret_code)
        if ret_code != 0:
            raise RuntimeError("tsung exited with non-zero code %s" % ret_code)
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.__out and not self.__out.closed:
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()

    def _check_installed(self):
        tool_path = self.settings.get('path', 'tsung')
        tsung = Tsung(tool_path, self.log)
        if not tsung.check_if_installed():
            raise RuntimeError("You must install Tsung manually to use it, see %s" % tsung.INSTALLATION_DOCS)
        return tool_path

    def get_widget(self):
        if not self.widget:
            if self.get_load().hold:
                label = "Tsung"
            else:
                label = None
            self.widget = SidebarWidget(self, label)
        return self.widget

    def resource_files(self):
        raise NotImplementedError("Tsung.resource_files()")


class TsungStatsReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(TsungStatsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.skipped_header = False

    def __open_fds(self):
        if not path.isfile(self.filename):
            self.log.debug("Stats file not appeared yet")
            return False

        if not path.getsize(self.filename):
            self.log.debug("Stats file is empty: %s", self.filename)
            return False

        if not self.fds:
            self.fds = open(self.filename)

        return True

    def __del__(self):
        if self.fds:
            self.fds.close()

    def _read(self, last_pass=False):
        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None
        if last_pass:
            lines = self.fds.readlines()
            self.fds.close()
        else:
            lines = self.fds.readlines(1024 * 1024)

        yield None


class Tsung(RequiredTool):
    INSTALLATION_DOCS = "http://tsung.erlang-projects.org/user_manual/installation.html"

    def __init__(self, tool_path, parent_logger):
        super(Tsung, self).__init__("Tsung", tool_path)
        self.tool_path = tool_path
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking Tsung at %s' % self.tool_path)
        try:
            shell_exec([self.tool_path, '-v'])
        except OSError:
            return False
        return True
