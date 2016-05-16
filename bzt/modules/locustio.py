"""
Module holds all stuff regarding Grinder tool usage

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
import json
import math
import os
import sys
import time
from imp import find_module
from subprocess import STDOUT

from bzt.engine import ScenarioExecutor, FileLister, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsProvider, DataPoint, KPISet
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.modules.jmeter import JTLReader
from bzt.six import PY3, iteritems
from bzt.utils import shutdown_process, RequiredTool, BetterDict


class LocustIOExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    def __init__(self):
        super(LocustIOExecutor, self).__init__()
        self.locustfile = None
        self.kpi_jtl = None
        self.process = None
        self.__out = None
        self.widget = None
        self.start_time = None
        self.is_master = False
        self.slaves_ldjson = None
        self.expected_slaves = 0
        self.reader = None

    def prepare(self):
        self.__check_installed()
        self.locustfile = self.get_locust_file()
        if not self.locustfile or not os.path.exists(self.locustfile):
            raise ValueError("Locust file not found: %s" % self.locustfile)

        self.is_master = self.execution.get("master", self.is_master)
        if self.is_master:
            slaves = self.execution.get("slaves", ValueError("Slaves count required when starting in master mode"))
            self.expected_slaves = int(slaves)

        self.engine.existing_artifact(self.locustfile)

        if self.is_master:
            self.slaves_ldjson = self.engine.create_artifact("locust-slaves", ".ldjson")
            self.reader = SlavesReader(self.slaves_ldjson, self.expected_slaves, self.log)
        else:
            self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
            self.reader = JTLReader(self.kpi_jtl, self.log, None)

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def __check_installed(self):
        tool = LocustIO(self.log)
        if not tool.check_if_installed():
            if PY3:
                raise RuntimeError("LocustIO is not currently compatible with Python 3.x")
            raise RuntimeError("Unable to locate locustio package. Please install it like this: pip install locustio")

    def startup(self):
        self.start_time = time.time()
        load = self.get_load()
        hatch = load.concurrency / load.ramp_up if load.ramp_up else load.concurrency
        wrapper = os.path.join(os.path.abspath(os.path.dirname(__file__)),
                               os.pardir,
                               "resources",
                               "locustio-taurus-wrapper.py")

        env = BetterDict()
        env.merge({"PYTHONPATH": self.engine.artifacts_dir + os.pathsep + os.getcwd()})
        if os.getenv("PYTHONPATH"):
            env['PYTHONPATH'] = os.getenv("PYTHONPATH") + os.pathsep + env['PYTHONPATH']

        args = [sys.executable, os.path.realpath(wrapper), '-f', os.path.realpath(self.locustfile)]
        args += ['--logfile=%s' % self.engine.create_artifact("locust", ".log")]
        args += ["--no-web", "--only-summary", ]
        args += ["--clients=%d" % load.concurrency, "--hatch-rate=%d" % math.ceil(hatch), ]
        if load.iterations:
            args.append("--num-request=%d" % load.iterations)

        if self.is_master:
            args.extend(["--master", '--expect-slaves=%s' % self.expected_slaves])
            env["SLAVES_LDJSON"] = self.slaves_ldjson
        else:
            env["JTL"] = self.kpi_jtl

        host = self.get_scenario().get("default-address", None)
        if host:
            args.append("--host=%s" % host)

        self.__out = open(self.engine.create_artifact("locust", ".out"), 'w')
        self.process = self.execute(args, stderr=STDOUT, stdout=self.__out, env=env)

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :rtype: SidebarWidget
        """
        if not self.widget:
            if self.locustfile is not None:
                label = "Script: %s" % os.path.basename(self.locustfile)
            else:
                label = None
            self.widget = SidebarWidget(self, label)
        return self.widget

    def check(self):
        # TODO: when we're in master mode and get no results and exceeded duration - shut down then
        if self.widget:
            self.widget.update()

        retcode = self.process.poll()
        if retcode is not None:
            self.log.info("Locust exit code: %s", retcode)
            if retcode != 0:
                raise RuntimeError("Locust exited with non-zero code: %s" % retcode)

            return True

        return False

    def resource_files(self):
        if not self.locustfile:
            self.locustfile = self.get_locust_file()

        return [self.locustfile]

    def get_locust_file(self):
        scenario = self.get_scenario()
        locustfile = scenario.get(Scenario.SCRIPT, ValueError("Please specify locusfile in 'script' option"))
        locustfile = self.engine.find_file(locustfile)
        return locustfile

    def shutdown(self):
        try:
            shutdown_process(self.process, self.log)
        finally:
            if self.__out:
                self.__out.close()

    def post_process(self):
        no_master_results = (self.is_master and not self.reader.cumulative)
        no_local_results = (not self.is_master and self.reader and not self.reader.buffer)
        if no_master_results or no_local_results:
            raise RuntimeWarning("Empty results, most likely Locust failed")


class LocustIO(RequiredTool):
    def __init__(self, parent_logger):
        super(LocustIO, self).__init__("LocustIO", "")
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        try:
            find_module("locust")
            self.already_installed = True
        except ImportError:
            self.log.error("LocustIO is not installed, see http://docs.locust.io/en/latest/installation.html")
            return False
        return True

    def install(self):
        raise NotImplementedError()


class SlavesReader(ResultsProvider):
    def __init__(self, filename, num_slaves, parent_logger):
        """
        :type filename: str
        :type num_slaves: int
        :type parent_logger: logging.Logger
        """
        super(SlavesReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.join_buffer = {}
        self.num_slaves = num_slaves
        self.fds = None
        self.read_buffer = ""

    def _calculate_datapoints(self, final_pass=False):
        if not self.fds:
            self.__open_file()

        if self.fds:
            self.read_buffer += self.fds.read(1024 * 1024)
            while "\n" in self.read_buffer:
                _line = self.read_buffer[:self.read_buffer.index("\n") + 1]
                self.read_buffer = self.read_buffer[len(_line):]
                self.fill_join_buffer(json.loads(_line))

        max_full_ts = self.get_max_full_ts()

        if max_full_ts is not None:
            for point in self.merge_datapoints(max_full_ts):
                yield point

    def merge_datapoints(self, max_full_ts):
        for key in sorted(self.join_buffer.keys(), key=int):
            if int(key) <= max_full_ts:
                sec_data = self.join_buffer.pop(key)
                self.log.debug("Processing complete second: %s", key)
                point = DataPoint(int(key))
                for sid, item in iteritems(sec_data):
                    point.merge_point(self.point_from_locust(key, sid, item))
                point.recalculate()
                yield point

    def get_max_full_ts(self):
        max_full_ts = None
        for key in sorted(self.join_buffer.keys(), key=int):
            if len(key) >= self.num_slaves:
                max_full_ts = int(key)
        return max_full_ts

    def __del__(self):
        if self.fds:
            self.fds.close()

    def __open_file(self):
        if os.path.exists(self.filename):
            self.log.debug("Opening %s", self.filename)
            self.fds = open(self.filename, 'rt')
        else:
            self.log.debug("File not exists: %s", self.filename)

    def fill_join_buffer(self, data):
        self.log.debug("Got slave data: %s", data)
        for stats_item in data['stats']:
            for timestamp in stats_item['num_reqs_per_sec'].keys():
                if timestamp not in self.join_buffer:
                    self.join_buffer[timestamp] = {}
                self.join_buffer[timestamp][data['client_id']] = data

    @staticmethod
    def point_from_locust(timestamp, sid, data):
        """
        :type timestamp: str
        :type sid: str
        :type data: dict
        :rtype: DataPoint
        """
        point = DataPoint(int(timestamp))
        point[DataPoint.SOURCE_ID] = sid
        overall = KPISet()
        for item in data['stats']:
            if timestamp not in item['num_reqs_per_sec']:
                continue

            kpiset = KPISet()
            kpiset[KPISet.SAMPLE_COUNT] = item['num_reqs_per_sec'][timestamp]
            kpiset[KPISet.CONCURRENCY] = data['user_count']
            if item['num_requests']:
                avg_rt = (item['total_response_time'] / 1000.0) / item['num_requests']
                kpiset.sum_rt = item['num_reqs_per_sec'][timestamp] * avg_rt
            point[DataPoint.CURRENT][item['name']] = kpiset
            overall.merge_kpis(kpiset)

        point[DataPoint.CURRENT][''] = overall
        point.recalculate()
        return point
