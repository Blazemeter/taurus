"""
Module holds all stuff regarding Locust tool usage

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
from collections import Counter

from bzt import TaurusConfigError
from bzt.engine import ScenarioExecutor, FileLister, HavingInstallableTools, SelfDiagnosable
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsProvider, DataPoint, KPISet
from bzt.modules.python.generators import LocustIOScriptBuilder
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.jmeter import JTLReader
from bzt.six import iteritems
from bzt.utils import get_full_path, FileReader, CALL_PROBLEMS
from bzt.utils import shutdown_process, RequiredTool, dehumanize_time, RESOURCES_DIR


class LocustIOExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools, SelfDiagnosable):
    def __init__(self):
        super(LocustIOExecutor, self).__init__()
        self.process = None
        self.is_master = False
        self.expected_slaves = 0
        self.scenario = None
        self.script = None
        self.log_file = None

    def prepare(self):
        self.stdout = open(self.engine.create_artifact("locust", ".out"), 'w')
        self.stderr = open(self.engine.create_artifact("locust", ".err"), 'w')

        self.install_required_tools()
        self.scenario = self.get_scenario()
        self.__setup_script()
        self.engine.existing_artifact(self.script)

        # path to taurus dir. It's necessary for bzt usage inside tools/helpers
        self.env.add_path({"PYTHONPATH": get_full_path(__file__, step_up=3)})

        self.is_master = self.execution.get("master", self.is_master)

        if self.is_master:
            count_error = TaurusConfigError("Slaves count required when starting in master mode")
            self.expected_slaves = int(self.execution.get("slaves", count_error))
            slaves_ldjson = self.engine.create_artifact("locust-slaves", ".ldjson")
            self.reader = SlavesReader(slaves_ldjson, self.expected_slaves, self.log)
            self.env.set({"SLAVES_LDJSON": slaves_ldjson})
        else:
            kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
            self.reader = JTLReader(kpi_jtl, self.log)
            self.env.set({"JTL": kpi_jtl})

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def install_required_tools(self):
        tool = self._get_tool(LocustIO)
        if not tool.check_if_installed():
            tool.install()

    def startup(self):
        load = self.get_load()
        concurrency = load.concurrency or 1

        if self.is_master:
            concurrency = math.ceil(concurrency / float(self.expected_slaves))

        if load.ramp_up:
            hatch = concurrency / float(load.ramp_up)
        else:
            hatch = concurrency

        wrapper = os.path.join(RESOURCES_DIR, "locustio-taurus-wrapper.py")

        if load.duration:
            self.env.set({"LOCUST_DURATION": dehumanize_time(load.duration)})

        self.env.add_path({"PYTHONPATH": get_full_path(__file__, step_up=3)})
        self.env.add_path({"PYTHONPATH": self.engine.artifacts_dir})

        self.log_file = self.engine.create_artifact("locust", ".log")
        args = [sys.executable, wrapper, '-f', self.script]
        args += ['--logfile=%s' % self.log_file]
        args += ["--no-web", "--only-summary", ]
        args += ["--clients=%d" % concurrency, "--hatch-rate=%f" % hatch]
        if load.iterations:
            num_requests = load.iterations * concurrency
            self.env.set({"LOCUST_NUMREQUESTS": num_requests})

        if self.is_master:
            args.extend(["--master", '--expect-slaves=%s' % self.expected_slaves])

        host = self.get_scenario().get("default-address")
        if host:
            args.append('--host=%s' % host)

        self.process = self.execute(args)

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :rtype: ExecutorWidget
        """
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "Locust.io: " + label.split('/')[1])
        return self.widget

    def check(self):
        # TODO: when we're in master mode and get no results and exceeded duration - shut down then
        retcode = self.process.poll()
        if retcode is not None:
            if retcode != 0:
                self.log.warning("Locust exited with non-zero code: %s", retcode)

            return True
        return False

    def resource_files(self):
        script = self.get_script_path()
        if script:
            return [script]
        else:
            return []

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("generated_locust", ".py")
        locust_test = LocustIOScriptBuilder(self.scenario, self.log)
        locust_test.build_source_code()
        locust_test.save(filename)
        return filename

    def __setup_script(self):
        self.script = self.get_script_path()
        if not self.script:
            if "requests" in self.scenario:
                self.script = self.__tests_from_requests()
            else:
                msg = "There must be a script file or requests for its generation "
                msg += "to run Locust (%s)" % self.execution.get('scenario')
                raise TaurusConfigError(msg)

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def has_results(self):
        master_results = self.is_master and self.reader.cumulative
        local_results = not self.is_master and self.reader and self.reader.buffer
        if master_results or local_results:
            return True
        else:
            return False

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Locust STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Locust STDERR:\n" + contents)
        if self.log_file is not None and os.path.exists(self.log_file):
            with open(self.log_file) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Locust log:\n" + contents)
        return diagnostics


class LocustIO(RequiredTool):
    def __init__(self, **kwargs):
        super(LocustIO, self).__init__(tool_path="locust", installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug("Trying %s: %s", self.tool_name, self.tool_path)
        try:
            out, err = self.call([self.tool_path, "--version"])
            if err:
                out += err
            self.log.debug("%s output: %s", self.tool_name, out)
            return True
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False


class SlavesReader(ResultsProvider):
    def __init__(self, filename, num_slaves, parent_logger):
        """
        :type filename: str
        :type num_slaves: int
        :type parent_logger: logging.Logger
        """
        super(SlavesReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.join_buffer = {}
        self.num_slaves = num_slaves
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.read_buffer = ""

    def _calculate_datapoints(self, final_pass=False):
        read = self.file.get_bytes(size=1024 * 1024, last_pass=final_pass)
        if not read or not read.strip():
            return
        self.read_buffer += read
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
            kpiset[KPISet.BYTE_COUNT] = item['total_content_length']
            if item['num_requests']:
                avg_rt = (item['total_response_time'] / 1000.0) / item['num_requests']
                kpiset.sum_rt = item['num_reqs_per_sec'][timestamp] * avg_rt

            for err in data['errors'].values():
                if err['name'] == item['name']:
                    new_err = KPISet.error_item_skel(err['error'], None, err['occurences'], KPISet.ERRTYPE_ERROR,
                                                     Counter(), None)
                    KPISet.inc_list(kpiset[KPISet.ERRORS], ("msg", err['error']), new_err)
                    kpiset[KPISet.FAILURES] += err['occurences']

            kpiset[KPISet.SUCCESSES] = kpiset[KPISet.SAMPLE_COUNT] - kpiset[KPISet.FAILURES]
            point[DataPoint.CURRENT][item['name']] = kpiset
            overall.merge_kpis(kpiset)

        point[DataPoint.CURRENT][''] = overall
        point.recalculate()
        return point
