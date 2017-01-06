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
import time
from collections import OrderedDict
from imp import find_module
from subprocess import STDOUT

from bzt import ToolError, TaurusConfigError
from bzt.engine import ScenarioExecutor, FileLister, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsProvider, DataPoint, KPISet
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.jmeter import JTLReader
from bzt.modules.services import HavingInstallableTools
from bzt.six import PY3, iteritems
from bzt.utils import shutdown_process, RequiredTool, BetterDict, dehumanize_time, ensure_is_dict, PythonGenerator


class LocustIOExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools):
    def __init__(self):
        super(LocustIOExecutor, self).__init__()
        self.kpi_jtl = None
        self.process = None
        self.__out = None
        self.is_master = False
        self.slaves_ldjson = None
        self.expected_slaves = 0
        self.scenario = None
        self.script = None

    def prepare(self):
        self.install_required_tools()
        self.scenario = self.get_scenario()
        self.__setup_script()

        self.is_master = self.execution.get("master", self.is_master)
        if self.is_master:
            count_error = TaurusConfigError("Slaves count required when starting in master mode")
            slaves = self.execution.get("slaves", count_error)
            self.expected_slaves = int(slaves)

        self.engine.existing_artifact(self.script)

        if self.is_master:
            self.slaves_ldjson = self.engine.create_artifact("locust-slaves", ".ldjson")
            self.reader = SlavesReader(self.slaves_ldjson, self.expected_slaves, self.log)
        else:
            self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
            self.reader = JTLReader(self.kpi_jtl, self.log, None)

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def install_required_tools(self):
        tool = LocustIO(self.log)
        if not tool.check_if_installed():
            tool.install()

    def startup(self):
        self.start_time = time.time()
        load = self.get_load()
        concurrency = load.concurrency or 1
        if load.ramp_up:
            hatch = math.ceil(concurrency / load.ramp_up)
        else:
            hatch = concurrency

        wrapper = os.path.join(os.path.abspath(os.path.dirname(__file__)),
                               os.pardir,
                               "resources",
                               "locustio-taurus-wrapper.py")

        env = BetterDict()
        env.merge({"PYTHONPATH": self.engine.artifacts_dir + os.pathsep + os.getcwd()})
        if os.getenv("PYTHONPATH"):
            env['PYTHONPATH'] = os.getenv("PYTHONPATH") + os.pathsep + env['PYTHONPATH']

        args = [sys.executable, os.path.realpath(wrapper), '-f', os.path.realpath(self.script)]
        args += ['--logfile=%s' % self.engine.create_artifact("locust", ".log")]
        args += ["--no-web", "--only-summary", ]
        args += ["--clients=%d" % concurrency, "--hatch-rate=%d" % hatch]
        if load.iterations:
            args.append("--num-request=%d" % load.iterations)

        env['LOCUST_DURATION'] = dehumanize_time(load.duration)
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
        self.scenario = self.get_scenario()
        script = self.scenario.get(Scenario.SCRIPT, None)
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
        try:
            shutdown_process(self.process, self.log)
        finally:
            if self.__out:
                self.__out.close()

    def has_results(self):
        master_results = self.is_master and self.reader.cumulative
        local_results = not self.is_master and self.reader and self.reader.buffer
        if master_results or local_results:
            return True
        else:
            return False


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
        if PY3:
            raise ToolError("LocustIO is not currently compatible with Python 3.x")
        msg = "Unable to locate locustio package. Please install it like this: pip install locustio"
        raise ToolError(msg)


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
            kpiset[KPISet.BYTE_COUNT] = item['total_content_length']
            if item['num_requests']:
                avg_rt = (item['total_response_time'] / 1000.0) / item['num_requests']
                kpiset.sum_rt = item['num_reqs_per_sec'][timestamp] * avg_rt
            point[DataPoint.CURRENT][item['name']] = kpiset
            overall.merge_kpis(kpiset)

        point[DataPoint.CURRENT][''] = overall
        point.recalculate()
        return point


class LocustIOScriptBuilder(PythonGenerator):
    IMPORTS = """
from gevent import sleep
from re import findall, compile
from locust import HttpLocust, TaskSet, task
"""

    def build_source_code(self):
        self.log.debug("Generating Python script for LocustIO")
        header_comment = self.gen_comment("This script was generated by Taurus", indent=0)
        scenario_class = self.gen_class_definition("UserBehaviour", ["TaskSet"])
        swarm_class = self.gen_class_definition("GeneratedSwarm", ["HttpLocust"])
        imports = self.add_imports()

        self.root.append(header_comment)
        self.root.append(imports)
        self.root.append(scenario_class)
        self.root.append(swarm_class)

        swarm_class.append(self.gen_statement('task_set = UserBehaviour', indent=4))

        default_address = self.scenario.get("default-address", "")
        swarm_class.append(self.gen_statement('host = "%s"' % default_address, indent=4))

        swarm_class.append(self.gen_statement('min_wait = %s' % 0, indent=4))
        swarm_class.append(self.gen_statement('max_wait = %s' % 0, indent=4))
        swarm_class.append(self.gen_new_line(indent=0))

        scenario_class.append(self.gen_decorator_statement('task(1)'))

        scenario_class.append(self.__gen_task())
        scenario_class.append(self.gen_new_line(indent=0))

    def __gen_task(self):
        task = self.gen_method_definition("generated_task", ['self'])

        think_time = dehumanize_time(self.scenario.get('think-time', None))
        timeout = dehumanize_time(self.scenario.get("timeout", 30))
        global_headers = self.scenario.get("headers", None)
        if not self.scenario.get("keepalive", True):
            global_headers['Connection'] = 'close'

        for req in self.scenario.get_requests():
            method = req.method.lower()
            if method not in ('get', 'delete', 'head', 'options', 'path', 'put', 'post'):
                raise TaurusConfigError("Wrong Locust request type: %s" % method)

            if req.timeout:
                local_timeout = dehumanize_time(req.timeout)
            else:
                local_timeout = timeout
            self.__gen_check(method, req, task, local_timeout, global_headers)

            if req.think_time:
                task.append(self.gen_statement("sleep(%s)" % dehumanize_time(req.think_time)))
            else:
                if think_time:
                    task.append(self.gen_statement("sleep(%s)" % think_time))
            task.append(self.gen_new_line())
        return task

    @staticmethod
    def __get_params_line(req, timeout, headers):
        param_dict = {'url': '"%s"' % req.url, 'timeout': timeout}
        if req.body:
            if isinstance(req.body, dict):
                param_dict['data'] = json.dumps(req.body)
            else:
                param_dict['data'] = '"%s"' % req.body

        if headers:
            param_dict['headers'] = json.dumps(headers)
        keys = (list(param_dict.keys()))
        keys.sort()
        return ', '.join(['%s=%s' % (key, param_dict[key]) for key in keys])

    def __gen_check(self, method, req, task, timeout, global_headers):
        assertions = req.config.get("assert", [])
        first_assert = True
        if assertions:
            statement = 'with self.client.%s(%s, catch_response=True) as response:'
        else:
            statement = "self.client.%s(%s)"
        headers = OrderedDict()
        if global_headers:
            sorted_headers = OrderedDict(sorted(global_headers.items(), key=lambda t: t[0]))
            headers.update(sorted_headers)
        if req.headers:
            headers.update(req.headers)
        task.append(self.gen_statement(statement % (method, self.__get_params_line(req, timeout, headers))))

        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]

            self.__gen_assertion(task, assertion, first_assert)
            first_assert = False

        if assertions:
            task.append(self.gen_statement('else:', indent=12))
            task.append(self.gen_statement('response.success()', indent=16))

    def __gen_assertion(self, task, assertion, is_first):
        subject = assertion.get("subject", Scenario.FIELD_BODY)
        values = [str(_assert) for _assert in assertion['contains']]
        if subject == 'body':
            content = 'response.content'
        elif subject == 'http-code':
            content = 'str(response.status_code)'
        else:
            raise TaurusConfigError('Wrong subject for Locust assertion: %s' % subject)

        if assertion.get('not', False):
            attr_not = ''
            func_name = 'any'
        else:
            attr_not = ' not'
            func_name = 'all'

        if assertion.get("regexp", True):
            expression = 'findall(compile(str(val)), %(content)s)' % {'content': content}
        else:
            expression = 'str(val) in %s' % content

        statement = 'if%(not)s %(func)s(%(expression)s for val in %(values)s):'
        statement = statement % {'not': attr_not, 'func': func_name, 'expression': expression, 'values': values}
        if not is_first:
            statement = 'el' + statement
        task.append(self.gen_statement(statement, indent=12))

        statement = 'response.failure("%(values)s%(not)s found in %(subject)s")'
        statement = statement % {'values': values, 'not': attr_not, 'subject': subject}
        task.append(self.gen_statement(statement, indent=16))
