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
from collections import OrderedDict, Counter

from bzt import TaurusConfigError
from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsProvider, DataPoint, KPISet
from bzt.modules.console import ExecutorWidget
from bzt.modules.jmeter import JTLReader
from bzt.modules.services import PythonTool
from bzt.requests_model import HTTPRequest
from bzt.utils import iteritems, get_full_path, ensure_is_dict, PythonGenerator, FileReader, CALL_PROBLEMS
from bzt.utils import shutdown_process, dehumanize_time, RESOURCES_DIR


class LocustIOExecutor(ScenarioExecutor):
    def __init__(self):
        super(LocustIOExecutor, self).__init__()
        self.process = None
        self.is_master = False
        self.expected_workers = 0
        self.scenario = None
        self.script = None
        self.log_file = None
        self.locust = None

    def prepare(self):
        super(LocustIOExecutor, self).prepare()
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
            count_error = TaurusConfigError("Workers count required when starting in master mode")
            self.expected_workers = int(self.execution.get("workers", count_error))
            workers_ldjson = self.engine.create_artifact("locust-workers", ".ldjson")
            self.reader = WorkersReader(workers_ldjson, self.expected_workers, self.log)
            self.env.set({"WORKERS_LDJSON": workers_ldjson})
        else:
            kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
            self.reader = JTLReader(kpi_jtl, self.log)
            self.env.set({"JTL": kpi_jtl})

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def install_required_tools(self):
        self.locust = self._get_tool(Locust, engine=self.engine, settings=self.settings)
        if not self.locust.check_if_installed():
            self.locust.install()

    def startup(self):
        load = self.get_load()
        concurrency = load.concurrency or 1
        run_time = 10  # seconds

        if self.is_master:
            concurrency = math.ceil(concurrency / float(self.expected_workers))

        if load.ramp_up:
            hatch = concurrency / float(load.ramp_up)
        else:
            hatch = concurrency

        wrapper = os.path.join(RESOURCES_DIR, "locustio-taurus-wrapper.py")

        if load.duration:
            run_time = dehumanize_time(load.duration)
            self.env.set({"LOCUST_DURATION": run_time})

        self.env.add_path({"PYTHONPATH": get_full_path(__file__, step_up=3)})
        self.env.add_path({"PYTHONPATH": self.engine.artifacts_dir})

        self.log_file = self.engine.create_artifact("locust", ".log")
        args = [sys.executable, wrapper, '-f', self.script]
        args += ['--logfile=%s' % self.log_file]
        args += ["--headless", "--only-summary", ]
        args += ["--users=%d" % concurrency, "--spawn-rate=%f" % hatch]
        args += ["--run-time=%d" % run_time, "--stop-timeout=10"]

        if load.iterations:
            num_requests = load.iterations * concurrency
            self.env.set({"LOCUST_NUMREQUESTS": num_requests})

        if self.is_master:
            args.extend(["--master", '--expect-workers=%s' % self.expected_workers])

        host = self.get_scenario().get("default-address")
        if host:
            args.append('--host=%s' % host)

        self.process = self._execute(args)

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
        locust_test = LocustIOScriptBuilder(self.scenario)
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

    def post_process(self):
        self.locust.post_process()
        super(LocustIOExecutor, self).post_process()

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


class Locust(PythonTool):
    PACKAGES = ["locust"]


class WorkersReader(ResultsProvider):
    def __init__(self, filename, num_workers, parent_logger):
        """
        :type filename: str
        :type num_workers: int
        :type parent_logger: logging.Logger
        """
        super(WorkersReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.join_buffer = {}
        self.num_workers = num_workers
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
        reader_id = self.file.name + "@" + str(id(self))
        for key in sorted(self.join_buffer.keys(), key=int):
            if int(key) <= max_full_ts:
                sec_data = self.join_buffer.pop(key)
                self.log.debug("Processing complete second: %s", key)
                point = DataPoint(int(key))
                point[DataPoint.SOURCE_ID] = reader_id
                for sid, item in iteritems(sec_data):
                    point.merge_point(self.point_from_locust(key, sid, item))
                point.recalculate()
                yield point

    def get_max_full_ts(self):
        max_full_ts = None
        for key in sorted(self.join_buffer.keys(), key=int):
            if len(key) >= self.num_workers:
                max_full_ts = int(key)
        return max_full_ts

    def fill_join_buffer(self, data):
        self.log.debug("Got worker data: %s", data)
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
            overall.merge_kpis(kpiset, sid)

        point[DataPoint.CURRENT][''] = overall
        point.recalculate()
        return point


class LocustIOScriptBuilder(PythonGenerator):
    IMPORTS = """
from gevent import sleep
from re import findall, compile
from locust import HttpUser, TaskSet, task, constant
"""

    def build_source_code(self):
        self.log.debug("Generating Python script for LocustIO")
        header_comment = self.gen_comment("This script was generated by Taurus", indent=0)
        scenario_class = self.gen_class_definition("UserBehaviour", ["TaskSet"])
        swarm_class = self.gen_class_definition("GeneratedSwarm", ["HttpUser"])
        imports = self.add_imports()

        self.root.append(header_comment)
        self.root.append(imports)
        self.root.append(scenario_class)
        self.root.append(swarm_class)

        swarm_class.append(self.gen_statement('tasks = [UserBehaviour]', indent=self.INDENT_STEP))

        default_address = self.scenario.get("default-address", "")
        swarm_class.append(self.gen_statement('host = "%s"' % default_address, indent=self.INDENT_STEP))

        swarm_class.append(self.gen_statement('wait_time = constant(%s)' % 0, indent=self.INDENT_STEP))
        swarm_class.append(self.gen_new_line())

        scenario_class.append(self.gen_decorator_statement('task(1)'))

        scenario_class.append(self.__gen_task())
        scenario_class.append(self.gen_new_line())

    def __gen_task(self):
        task = self.gen_method_definition("generated_task", ['self'])

        think_time = dehumanize_time(self.scenario.get_think_time())
        global_headers = self.scenario.get_headers()
        if not self.scenario.get("keepalive", True):
            global_headers['Connection'] = 'close'

        for req in self.scenario.get_requests():
            if not isinstance(req, HTTPRequest):
                msg = "Locust script generator doesn't support '%s' blocks, skipping"
                self.log.warning(msg, req.NAME)
                continue

            method = req.method.lower()
            if method not in ('get', 'delete', 'head', 'options', 'path', 'put', 'post'):
                raise TaurusConfigError("Wrong Locust request type: %s" % method)

            timeout = req.priority_option('timeout', default='30s')

            self.__gen_check(method, req, task, dehumanize_time(timeout), global_headers)

            if req.get_think_time():
                task.append(self.gen_statement("sleep(%s)" % dehumanize_time(req.get_think_time())))
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
            expression = 'val in %s' % content

        statement = 'if%(not)s %(func)s(%(expression)s for val in %(values)s):'
        if subject == 'body':
            bin_values = [bytes(val, 'UTF-8') for val in values]
            statement = statement % {'not': attr_not, 'func': func_name, 'expression': expression, 'values': bin_values}
        else:
            statement = statement % {'not': attr_not, 'func': func_name, 'expression': expression, 'values': values}
        if not is_first:
            statement = 'el' + statement
        task.append(self.gen_statement(statement, indent=12))

        statement = 'response.failure("%(values)s%(not)s found in %(subject)s")'
        statement = statement % {'values': values, 'not': attr_not, 'subject': subject}
        task.append(self.gen_statement(statement, indent=16))
