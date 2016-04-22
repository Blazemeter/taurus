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
import csv
import logging
import os
import time
import traceback

from bzt.engine import FileLister, Scenario, ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.utils import shell_exec, shutdown_process, RequiredTool
from bzt.six import etree


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
        self.tsung_artifacts_basedir = None
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

        basedir_prefix = "tsung_taurus_%s" % id(self)
        self.tsung_artifacts_basedir = os.path.join(self.engine.artifacts_dir, basedir_prefix)
        os.makedirs(self.tsung_artifacts_basedir)

        self.stats_reader = TsungStatsReader(self.tsung_artifacts_basedir, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.stats_reader)

        self.__out = open(self.engine.create_artifact("tsung", ".out"), 'w')
        self.__err = open(self.engine.create_artifact("tsung", ".err"), 'w')

    def _get_script(self):
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
        args += ['-l', self.tsung_artifacts_basedir]
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
            self.widget = SidebarWidget(self, "Tsung")
        return self.widget

    def resource_files(self):
        raise NotImplementedError("Tsung.resource_files()")


class TsungStatsReader(ResultsReader):
    def __init__(self, tsung_basedir, parent_logger):
        super(TsungStatsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.tsung_basedir = tsung_basedir
        self.filename = None
        self.fds = None
        self.delimiter = ";"
        self.offset = 0
        self.partial_buffer = ""
        self.skipped_header = False

    def _open_fds(self):
        if not self._locate_stats_file():
            return False

        if not os.path.isfile(self.filename):
            self.log.debug("Stats file not appeared yet")
            return False

        if not os.path.getsize(self.filename):
            self.log.debug("Stats file is empty: %s", self.filename)
            return False

        if not self.fds:
            self.fds = open(self.filename)

        return True

    def _locate_stats_file(self):
        basedir_contents = os.listdir(self.tsung_basedir)

        if not basedir_contents:
            self.log.debug("Tsung artifacts not appeared yet")
            return False
        if len(basedir_contents) != 1:
            self.log.warning("Multiple files in Tsung basedir %s, this shouldn't happen", self.tsung_basedir)
            return False

        self.filename = os.path.join(self.tsung_basedir, basedir_contents[0], "tsung.dump")
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()

    def _read(self, last_pass=False):
        while not self.fds and not self._open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading Tsung results")
        self.fds.seek(self.offset)
        if last_pass:
            lines = self.fds.readlines()
        else:
            lines = self.fds.readlines(1024 * 1024)
        self.offset = self.fds.tell()

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            if not self.skipped_header and line.startswith("#"):
                self.skipped_header = True
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            line = line.strip()
            fields = line.split(self.delimiter)

            tstamp = int(float(fields[0]))
            url = fields[4] + fields[5]
            rstatus = fields[6]
            etime = float(fields[8]) / 1000
            trname = fields[9]
            error = fields[10] or None

            concur = None
            con_time = 0
            latency = 0

            yield tstamp, url, concur, etime, con_time, latency, rstatus, error, trname


class TsungConfig(object):
    def __init__(self):
        self.log = logging.getLogger(self.__class__.__name__)
        root = etree.Element("tsung", loglevel="notice", version="1.0", dumptraffic="protocol")
        self.tree = etree.ElementTree(root)

    def load(self, filename):
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(filename)
        except BaseException as exc:
            self.log.debug("XML parsing error: %s", traceback.format_exc())
            raise RuntimeError("XML parsing failed for file %s: %s" % (filename, exc))

    def save(self, filename):
        self.log.debug("Saving Tsung config to: %s", filename)
        with open(filename, "wb") as fhd:
            self.tree.write(fhd, pretty_print=True, encoding="UTF-8", xml_declaration=True)

    def generate(self, scenario, load):
        self.__add_clients(scenario, load)
        self.__add_servers(scenario, load)
        self.__add_monitoring(scenario, load)
        self.__add_load(scenario, load)
        self.__add_options(scenario, load)
        self.__add_sessions(scenario, load)

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
