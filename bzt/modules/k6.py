"""
Copyright 2021 BlazeMeter Inc.

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
from bzt import TaurusConfigError, ToolError
from bzt.modules import ScenarioExecutor
from bzt.modules.console import ExecutorWidget
from bzt.modules.aggregator import ResultsReader, ConsolidatingAggregator
from bzt.utils import RequiredTool, CALL_PROBLEMS, FileReader, shutdown_process


class K6Executor(ScenarioExecutor):
    def __init__(self):
        super(K6Executor, self).__init__()
        self.output_file = None
        self.log_file = None
        self.script = None
        self.process = None
        self.k6 = None
        self.kpi_file = None

    def prepare(self):
        super(K6Executor, self).prepare()
        self.install_required_tools()

        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for k6 executor")

        self.stdout = open(self.engine.create_artifact("k6", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("k6", ".err"), "w")

        self.kpi_file = self.engine.create_artifact("kpi", ".csv")
        self.reader = K6LogReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        cmdline = [self.k6.tool_name, "run", "--out", f"csv={self.kpi_file}"]

        load = self.get_load()
        if load.concurrency:
            cmdline += ['--vus', str(load.concurrency)]

        if load.hold:
            cmdline += ['--duration', str(int(load.hold)) + "s"]

        if load.iterations:
            iterations = load.iterations * load.concurrency if load.concurrency else load.iterations
            cmdline += ['--iterations', str(iterations)]

        user_cmd = self.settings.get("cmdline")
        if user_cmd:
            cmdline += user_cmd.split(" ")

        cmdline += [self.script]
        self.process = self._execute(cmdline)

    def get_widget(self):
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "K6: " + label.split('/')[1])
        return self.widget

    def check(self):
        retcode = self.process.poll()
        if retcode is not None:
            ToolError(f"K6 tool exited with non-zero code: {retcode}")
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.kpi_file:
            self.engine.existing_artifact(self.kpi_file)
        super(K6Executor, self).post_process()

    def install_required_tools(self):
        self.k6 = self._get_tool(K6, config=self.settings)
        self.k6.tool_name = self.k6.tool_name.lower()
        if not self.k6.check_if_installed():
            self.k6.install()

    def resource_files(self):
        return [self.get_script_path(required=True)]


class K6LogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(K6LogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.data = {'timestamp': [], 'label': [], 'r_code': [], 'error_msg': [], 'http_req_duration': [],
                     'http_req_connecting': [], 'http_req_tls_handshaking': [], 'http_req_waiting': [],
                     'data_received': []}
        self.position = {'timestamp': None, 'metric_value': None, 'error': None,
                         'expected_response': None, 'name': None, 'status': None}

    def _read(self, last_pass=False):
        self.lines = list(self.file.get_lines(size=1024 * 1024, last_pass=last_pass))
        self.calculate = False
        self.vus = -1
        for line in self.lines:
            if line.startswith("metric_name"):
                parts = line[:-1].split(",")
                self.position['timestamp'] = parts.index('timestamp')
            elif line.startswith("http_reqs"):
                self.previous_timestamp = int(line.split(',')[self.position['timestamp']])
                break
        for line in self.lines:
            if line.startswith("metric_name"):
                parts = line[:-1].split(",")
                self.position['timestamp'] = parts.index('timestamp')
                self.position['metric_value'] = parts.index('metric_value')
                self.position['error'] = parts.index('error')
                self.position['expected_response'] = parts.index('expected_response')
                self.position['name'] = parts.index('name')
                self.position['status'] = parts.index('status')
            elif line.startswith("http_reqs"):
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    current_timestamp = int(parts[self.position['timestamp']])
                    if current_timestamp > self.previous_timestamp:
                        yield from self.calculate_timestamp_data()
                        self.previous_timestamp = current_timestamp
                    self.data['timestamp'].append(current_timestamp)
                    self.data['label'].append(parts[self.position['name']])
                    self.data['r_code'].append(parts[self.position['status']])
                    error = parts[self.position['error']]
                    if not error and parts[self.position['expected_response']] == 'false':
                        error = f"Response code: {parts[self.position['status']]}"
                    self.data['error_msg'].append(error)
            elif line.startswith("http_req_duration"):
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    self.data['http_req_duration'].append(float(parts[self.position['metric_value']]))
            elif line.startswith("http_req_connecting"):
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    self.data['http_req_connecting'].append(float(parts[self.position['metric_value']]))
            elif line.startswith("http_req_tls_handshaking"):
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    self.data['http_req_tls_handshaking'].append(float(parts[self.position['metric_value']]))
            elif line.startswith("http_req_waiting"):
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    self.data['http_req_waiting'].append(float(parts[self.position['metric_value']]))
            elif line.startswith("vus") and not line.startswith("vus_max") and self.vus == -1:
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    self.vus = (int(float(parts[self.position['metric_value']])))
            elif line.startswith("data_received"):
                parts = line.split(",")
                if len(parts) > self.position['status']:
                    self.data['data_received'].append(float(parts[self.position['metric_value']]))

        yield from self.calculate_timestamp_data()

    def calculate_timestamp_data(self):
        # calculate bandwidth
        bandwidth = 0
        if self.data['data_received']:
            for i in range(len(self.data['data_received'])):
                bandwidth = bandwidth + self.data['data_received'][i]
        if self.data['timestamp']:
            self.data

            for i in range(len(self.data['timestamp'])):
                # fill missing matrix items
                timestamp_length = len(self.data['timestamp'])
                if timestamp_length > len(self.data['label']):
                    self.data['label'] += [''] * (timestamp_length - len(self.data['label']))
                if timestamp_length > len(self.data['http_req_duration']):
                    self.data['http_req_duration'] += [0] * (timestamp_length - len(self.data['http_req_duration']))
                if timestamp_length > len(self.data['http_req_connecting']):
                    self.data['http_req_connecting'] += [0] * (timestamp_length - len(self.data['http_req_connecting']))
                if timestamp_length > len(self.data['http_req_tls_handshaking']):
                    self.data['http_req_tls_handshaking'] += [0] * (
                                timestamp_length - len(self.data['http_req_tls_handshaking']))
                if timestamp_length > len(self.data['http_req_waiting']):
                    self.data['http_req_waiting'] += [0] * (timestamp_length - len(self.data['http_req_waiting']))
                if timestamp_length > len(self.data['r_code']):
                    self.data['r_code'] += [0] * (timestamp_length - len(self.data['r_code']))
                if timestamp_length > len(self.data['error_msg']):
                    self.data['error_msg'] += [None] * (timestamp_length - len(self.data['error_msg']))
                kpi_set = (
                    self.data['timestamp'][i],
                    self.data['label'][i],
                    self.vus,
                    self.data['http_req_duration'][i] / 1000,
                    (self.data['http_req_connecting'][i] + self.data['http_req_tls_handshaking'][i]) / 1000,
                    self.data['http_req_waiting'][i] / 1000,
                    self.data['r_code'][i],
                    None if not self.data['error_msg'][i] else self.data['error_msg'][i],
                    '',
                    bandwidth)

                yield kpi_set
        self.data = {'timestamp': [], 'label': [], 'r_code': [], 'error_msg': [], 'http_req_duration': [],
                     'http_req_connecting': [], 'http_req_tls_handshaking': [], 'http_req_waiting': [],
                     'data_received': []}


class K6(RequiredTool):
    def __init__(self, config=None, **kwargs):
        super(K6, self).__init__(installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug('Checking K6 Framework: %s' % self.tool_path)
        try:
            out, err = self.call(['k6', 'version'])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("K6 output: %s", out)
        return True
