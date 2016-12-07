"""
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
import csv
import json
import math
import os
import socket
import string
import struct
import subprocess
import sys
import time
from abc import abstractmethod
from os import strerror
from subprocess import CalledProcessError

import datetime
import psutil

from bzt import resources, TaurusConfigError, ToolError, TaurusInternalException
from bzt.engine import ScenarioExecutor, FileLister, Scenario
from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet, ConsolidatingAggregator
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.services import HavingInstallableTools
from bzt.six import string_types, urlencode, iteritems, parse, StringIO, b, viewvalues
from bzt.utils import RequiredTool, IncrementableProgressBar
from bzt.utils import shell_exec, shutdown_process, BetterDict, dehumanize_time


class PBenchExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools):
    """
    :type pbench: PBenchTool
    :type widget: ExecutorWidget
    """

    def __init__(self):
        super(PBenchExecutor, self).__init__()
        self.pbench = None

    def prepare(self):
        self.install_required_tools()
        self._generate_files()
        self.reader = self.pbench.get_results_reader()
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def _prepare_pbench(self):
        if self.settings.get('enhanced', False):
            self.log.info("Using enhanced version for pbench tool")
            self.pbench = TaurusPBenchTool(self, self.log)
        else:
            self.log.info("Using stock version for pbench tool")
            self.pbench = OriginalPBenchTool(self, self.log)

    def _generate_files(self):
        self.pbench.generate_payload(self.get_scenario())
        self.pbench.generate_schedule(self.get_load())
        self.pbench.generate_config(self.get_scenario(), self.get_load(), self.get_hostaliases())
        self.pbench.check_config()

    def startup(self):
        self.start_time = time.time()
        self.pbench.start(self.pbench.config_file)

    def check(self):
        retcode = self.pbench.process.poll()
        if retcode is not None:
            if retcode != 0:
                raise ToolError("Phantom-benchmark exit code: %s" % retcode)

            return True

        return False

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :return:
        """
        if not self.widget:
            proto = "https" if self.pbench.use_ssl else 'http'
            label = "Target: %s://%s:%s" % (proto, self.pbench.hostname, self.pbench.port)
            self.widget = ExecutorWidget(self, label)
        return self.widget

    def shutdown(self):
        shutdown_process(self.pbench.process, self.log)

    def resource_files(self):
        scenario = self.get_scenario()
        script = scenario.get(Scenario.SCRIPT, None)
        if script:
            return [script]
        else:
            return []

    def install_required_tools(self):
        self._prepare_pbench()

        tool = PBench(self.log, self.pbench.path)
        if not tool.check_if_installed():
            tool.install()


class PBenchTool(object):
    SSL_STR = "transport_t ssl_transport = transport_ssl_t { timeout = 1s }\n transport = ssl_transport"

    def __init__(self, executor, base_logger):
        """
        :param executor: ScenarioExecutor
        :type base_logger: logging.Logger
        """
        super(PBenchTool, self).__init__()
        self.log = base_logger.getChild(self.__class__.__name__)
        self.executor = executor
        self.engine = executor.engine
        self.settings = executor.settings
        self.execution = executor.execution
        self.path = os.path.expanduser(self.settings.get('path', 'phantom'))
        self.modules_path = os.path.expanduser(self.settings.get("modules-path", "/usr/lib/phantom"))
        self.kpi_file = None
        self.stats_file = None
        self.config_file = None
        self.payload_file = None
        self.schedule_file = None
        self.process = None
        self.use_ssl = False
        self.hostname = 'localhost'
        self.port = 80
        self._target = {"scheme": None, "netloc": None}

    def generate_config(self, scenario, load, hostaliases):
        self.kpi_file = self.engine.create_artifact("pbench-kpi", ".txt")
        self.stats_file = self.engine.create_artifact("pbench-additional", ".ldjson")
        self.config_file = self.engine.create_artifact('pbench', '.conf')

        conf_path = os.path.join(os.path.abspath(os.path.dirname(resources.__file__)), 'pbench.conf')
        with open(conf_path) as _fhd:
            tpl = _fhd.read()

        instances = load.concurrency if load.concurrency else 1

        timeout = int(dehumanize_time(scenario.get("timeout", "10s")) * 1000)

        threads = 1 if psutil.cpu_count() < 2 else (psutil.cpu_count() - 1)
        threads = int(self.execution.get("worker-threads", threads))

        if self.hostname in hostaliases:
            address = hostaliases[self.hostname]
        else:
            address = socket.gethostbyname(self.hostname)
        params = {
            "modules_path": self.modules_path,
            "threads": threads,
            "log": self.engine.create_artifact("pbench", ".log"),
            "kpi_file": self.kpi_file,
            "full_log": self.engine.create_artifact("pbench-request-response", ".txt"),
            "full_log_level": self.execution.get("log-responses", "proto_warning"),  # proto_error, all
            "source": self._get_source(load),
            "ssl": self.SSL_STR if self.use_ssl else "",
            "reply_limits": "",  # TODO
            "address": address,
            "port": self.port,
            "timeout": timeout,
            "instances": instances,
            "stat_log": self.stats_file,
            "additional_modules": self._get_additional_modules()
        }

        with open(self.config_file, 'w') as _fhd:
            substituter = string.Template(tpl)
            _fhd.write(substituter.substitute(params))

    def generate_payload(self, scenario):
        script_path = scenario.get(Scenario.SCRIPT, None)
        if script_path is not None:
            self.payload_file = self.engine.find_file(script_path)
        else:
            self.payload_file = self.engine.create_artifact("pbench", '.src')
            self.log.info("Generating payload file: %s", self.payload_file)
            self._generate_payload_inner(scenario)

    @staticmethod
    def _estimate_schedule_size_rps(load, payload_count):
        ramp_up = load.ramp_up if load.ramp_up else 0.0

        iterations = float(load.iterations or "inf")
        iteration_limit_items = iterations * payload_count

        if load.iterations:
            whole_rampup_items = ramp_up * load.throughput / 2.0
            rampup_items = min(iteration_limit_items, whole_rampup_items)
        else:
            rampup_items = ramp_up * load.throughput / 2.0

        rampup_iterations = rampup_items / payload_count

        if load.hold and load.iterations:
            hold_iterations = load.iterations - rampup_iterations
            hold_iteration_limit = payload_count * hold_iterations
            whole_hold_items = load.hold * load.throughput
            hold_items = min(hold_iteration_limit, whole_hold_items)
        elif load.hold and not load.iterations:
            frac, _ = math.modf(rampup_iterations)
            hold_iterations = 2 - frac
            hold_iterations_items = payload_count * hold_iterations
            hold_duration_items = load.hold * load.throughput
            hold_items = min(hold_iterations_items, hold_duration_items)
        else:
            hold_items = 0.0

        return rampup_items + hold_items

    @staticmethod
    def _estimate_schedule_size_conc(load, payload_count):
        ramp_up = load.ramp_up if load.ramp_up else 0.0

        if load.iterations:
            return load.iterations * payload_count
        else:
            if ramp_up:
                instances = float(load.concurrency) if load.concurrency else 1.0
                concurrency_iterations = instances / payload_count
                upper_iteration_limit = int(concurrency_iterations) + 2
            elif load.hold:
                upper_iteration_limit = 2
            else:
                upper_iteration_limit = 1
            return upper_iteration_limit * payload_count

    def _estimate_schedule_size(self, load, payload_count):
        if load.throughput:
            return self._estimate_schedule_size_rps(load, payload_count)
        else:
            return self._estimate_schedule_size_conc(load, payload_count)

    @abstractmethod
    def _write_schedule_file(self, load, scheduler, sfd):
        pass

    def generate_schedule(self, load):
        self.schedule_file = self.execution.get("schedule-file", None)
        if self.schedule_file is None:
            self.schedule_file = self.engine.create_artifact("pbench", '.sched')
            self.log.info("Generating request schedule file: %s", self.schedule_file)
            with open(self.payload_file, 'rb') as pfd:
                scheduler = Scheduler(load, pfd, self.log)
                with open(self.schedule_file, 'wb') as sfd:
                    self._write_schedule_file(load, scheduler, sfd)
            self.log.info("Done generating schedule file")

    def check_config(self):
        cmdline = [self.path, 'check', self.config_file]
        self.log.debug("Check pbench config with command: %s", cmdline)
        try:
            subprocess.check_call(cmdline, stdout=subprocess.PIPE)
        except CalledProcessError as exc:
            raise ToolError("Config check has failed: %s" % exc)

    def start(self, config_file):
        cmdline = [self.path, 'run', config_file]
        stdout = sys.stdout if not isinstance(sys.stdout, StringIO) else None
        stderr = sys.stderr if not isinstance(sys.stderr, StringIO) else None
        try:
            self.process = self.executor.execute(cmdline,
                                                 stdout=stdout,
                                                 stderr=stderr)
        except OSError as exc:
            raise ToolError("Failed to start phantom-benchmark utility: %s (%s)" % (exc, cmdline))

    def _generate_payload_inner(self, scenario):
        requests = scenario.get_requests()
        num_requests = 0
        with open(self.payload_file, 'w') as fds:
            for request in requests:
                http = self._build_request(request, scenario)
                fds.write("%s %s\r\n%s\r\n" % (len(http), request.label.replace(' ', '_'), http))
                num_requests += 1

        if not num_requests:
            raise TaurusInternalException("No requests were generated, check your 'requests' section presence")

    def _build_request(self, request, scenario):
        path = self._get_request_path(request, scenario)
        http = "%s %s HTTP/1.1\r\n" % (request.method, path)
        headers = BetterDict()
        headers.merge({"Host": self.hostname})
        if not scenario.get("keepalive", True):
            headers.merge({"Connection": 'close'})  # HTTP/1.1 implies keep-alive by default
        body = ""
        if isinstance(request.body, dict):
            if request.method != "GET":
                body = urlencode(request.body)
        elif isinstance(request.body, string_types):
            body = request.body
        elif request.body:
            msg = "Cannot handle 'body' option of type %s: %s"
            raise TaurusConfigError(msg % (type(request.body), request.body))

        if body:
            headers.merge({"Content-Length": len(body)})

        headers.merge(scenario.get("headers"))
        headers.merge(request.headers)
        for header, value in iteritems(headers):
            http += "%s: %s\r\n" % (header, value)
        http += "\r\n%s" % (body,)
        return http

    def _get_request_path(self, request, scenario):

        parsed_url = parse.urlparse(request.url)

        if not self._target.get("scheme"):
            self._target["scheme"] = parsed_url.scheme

        if not self._target.get("netloc"):
            self._target["netloc"] = parsed_url.netloc

        if parsed_url.scheme != self._target["scheme"] or parsed_url.netloc != self._target["netloc"]:
            raise TaurusConfigError("Address port and host must be the same")
        path = parsed_url.path
        if parsed_url.query:
            path += "?" + parsed_url.query
        else:
            if request.method == "GET" and isinstance(request.body, dict):
                path += "?" + urlencode(request.body)
        if not parsed_url.netloc:
            parsed_url = parse.urlparse(scenario.get("default-address", ""))
        self.hostname = parsed_url.netloc.split(':')[0] if ':' in parsed_url.netloc else parsed_url.netloc
        self.use_ssl = parsed_url.scheme == 'https'
        if parsed_url.port:
            self.port = parsed_url.port
        else:
            self.port = 443 if self.use_ssl else 80

        return path if len(path) else '/'

    @abstractmethod
    def _get_source(self, load):
        pass

    def get_results_reader(self):
        return PBenchKPIReader(self.kpi_file, self.log, self.stats_file)

    @abstractmethod
    def _get_additional_modules(self):
        pass


class OriginalPBenchTool(PBenchTool):
    NEWLINE = "\n"

    def _write_schedule_file(self, load, scheduler, sfd):
        cnt = 0
        payload_entry_count = None
        pbar = None
        start_time = time.time()
        for item in scheduler.generate():
            # item : (time_offset, payload_len, payload_offset, payload, marker, record_type, overall_len)
            time_offset, payload_len, _, payload, marker, _, _ = item

            if scheduler.iterations > 1 and payload_entry_count is None:
                payload_entry_count = scheduler.count
                estimated_size = self._estimate_schedule_size(load, payload_entry_count)
                self.log.debug("Estimated schedule size: %s", estimated_size)
                if estimated_size:
                    pbar = IncrementableProgressBar(maxval=estimated_size)
                    pbar.catchup(start_time, cnt)

            if time_offset < 0:  # special case, run worker with no delay
                time_offset = 0.0

            sfd.write(b("%s %s %s%s" % (payload_len, int(1000 * time_offset), marker, self.NEWLINE)))
            sfd.write(b("%s%s" % (payload, self.NEWLINE)))

            cnt += 1
            if pbar:
                pbar.increment()
        self.log.debug("Actual schedule size: %s", cnt)
        if pbar:
            pbar.finish()

    def _get_source(self, load):
        return 'source_t source_log = source_log_t { filename = "%s" }' % self.schedule_file

    def _get_additional_modules(self):
        return ""


class TaurusPBenchTool(PBenchTool):
    def _write_schedule_file(self, load, scheduler, sfd):
        prev_offset = 0
        accum_interval = 0.0
        cnt = 0
        payload_entry_count = None
        pbar = None
        start_time = time.time()
        for item in scheduler.generate():
            # item : (time_offset, payload_len, payload_offset, payload, marker, record_type, overall_len)
            time_offset, _, payload_offset, _, _, record_type, overall_len = item

            if scheduler.iterations > 1 and payload_entry_count is None:
                payload_entry_count = scheduler.count
                estimated_size = self._estimate_schedule_size(load, payload_entry_count)
                self.log.debug("Estimated schedule size: %s", estimated_size)
                if estimated_size:
                    pbar = IncrementableProgressBar(maxval=estimated_size)
                    pbar.catchup(start_time, cnt)

            if time_offset >= 0:
                accum_interval += 1000 * (time_offset - prev_offset)
                interval = int(math.floor(accum_interval))
                accum_interval -= interval
            else:
                interval = 0xFFFFFF

            type_and_delay = struct.pack("I", interval)[:-1] + b(chr(record_type))
            payload_len_bytes = struct.pack('I', overall_len)
            payload_offset_bytes = struct.pack('Q', payload_offset)

            sfd.write(type_and_delay + payload_len_bytes + payload_offset_bytes)

            if pbar:
                pbar.increment()
            cnt += 1
            prev_offset = time_offset
        self.log.debug("Actual schedule size: %s", cnt)
        if pbar:
            pbar.finish()

    def _get_source(self, load):
        tpl = 'source_t source_log = taurus_source_t { ammo = "%s"\n schedule = "%s"\n %s\n }'
        if load.duration:
            duration_limit = "max_test_duration=%ss" % int(load.duration)
        else:
            duration_limit = ""
        return tpl % (self.payload_file, self.schedule_file, duration_limit)

    def _get_additional_modules(self):
        res = 'setup_t module_setup = setup_module_t {	dir = "%s" list = { taurus_source } }\n' % self.modules_path
        return res


class Scheduler(object):
    REC_TYPE_SCHEDULE = 0
    REC_TYPE_LOOP_START = 1
    REC_TYPE_STOP = 2

    def __init__(self, load, payload_fhd, logger):
        super(Scheduler, self).__init__()
        self.need_start_loop = None
        self.log = logger
        self.load = load
        self.payload_fhd = payload_fhd
        if not load.duration and not load.iterations:
            self.iteration_limit = 1
        else:
            self.iteration_limit = load.iterations

        self.concurrency = load.concurrency if load.concurrency is not None else 1

        self.step_len = load.ramp_up / load.steps if load.steps and load.ramp_up else 0
        if load.throughput:
            self.ramp_up_slope = load.throughput / load.ramp_up if load.ramp_up else 0
            self.step_size = float(load.throughput) / load.steps if load.steps else 0
        else:
            self.ramp_up_slope = None
            self.step_size = float(self.concurrency) / load.steps if load.steps else 0

        self.count = 0.0
        self.time_offset = 0.0
        self.iterations = 0

    def _payload_reader(self):
        self.iterations = 1
        rec_type = self.REC_TYPE_SCHEDULE
        while True:
            payload_offset = self.payload_fhd.tell()
            line = self.payload_fhd.readline()
            if not line:  # rewind
                self.payload_fhd.seek(0)
                self.iterations += 1

                if self.need_start_loop is not None and self.need_start_loop and not self.iteration_limit:
                    self.need_start_loop = False
                    self.iteration_limit = self.iterations
                    rec_type = self.REC_TYPE_LOOP_START

                if self.iteration_limit and self.iterations > self.iteration_limit:
                    self.log.debug("Schedule iterations limit reached: %s", self.iteration_limit)
                    break

                continue

            if not line.strip():  # we're fine to skip empty lines between records
                continue

            parts = line.split(b(' '))
            if len(parts) < 2:
                raise TaurusInternalException("Wrong format for meta-info line: %s" % line)

            payload_len, marker = parts
            marker = marker.decode()
            payload_len = int(payload_len)
            payload = self.payload_fhd.read(payload_len).decode()
            yield payload_len, payload_offset, payload, marker.strip(), len(line), rec_type
            rec_type = self.REC_TYPE_SCHEDULE

    def generate(self):
        for payload_len, payload_offset, payload, marker, meta_len, record_type in self._payload_reader():
            if self.load.throughput:
                self.time_offset += self.__get_time_offset_rps()
                if self.time_offset > self.load.duration:
                    self.log.debug("Duration limit reached: %s", self.time_offset)
                    break
            else:  # concurrency schedule
                self.time_offset = self.__get_time_offset_concurrency()

            overall_len = payload_len + meta_len
            yield self.time_offset, payload_len, payload_offset, payload, marker, record_type, overall_len
            self.count += 1

    def __get_time_offset_concurrency(self):
        if not self.load.ramp_up or self.count >= self.concurrency:
            if self.need_start_loop is None:
                self.need_start_loop = True
            return -1  # special case, means no delay
        elif self.load.steps:
            step = math.floor(self.count / self.step_size)
            return step * self.step_len
        else:  # ramp-up case
            return self.count * self.load.ramp_up / self.concurrency

    def __get_time_offset_rps(self):
        if not self.load.ramp_up or self.time_offset > self.load.ramp_up:
            # limit iterations
            rps = self.load.throughput
            if self.need_start_loop is None:
                self.need_start_loop = True
        elif self.load.steps:
            rps = self.step_size * (math.floor(self.time_offset / self.step_len) + 1)
        else:  # ramp-up case
            xpos = math.sqrt(2 * self.count / self.ramp_up_slope)
            rps = xpos * self.ramp_up_slope

        return 1.0 / rps if rps else 0


class PBenchKPIReader(ResultsReader):
    """
    Class to read KPI
    :type stats_reader: PBenchStatsReader
    """

    def __init__(self, filename, parent_logger, stats_filename):
        super(PBenchKPIReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.csvreader = None
        self.offset = 0
        self.fds = None
        if stats_filename:
            self.stats_reader = PBenchStatsReader(stats_filename, parent_logger)
        else:
            self.stats_reader = None

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :type last_pass: bool
        """

        def mcs2sec(val):
            return int(int(val) / 1000.0) / 1000.0

        if self.stats_reader:
            self.stats_reader.read_file(last_pass)

        if not self.csvreader and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        self.log.debug("Reading: %s", self.filename)
        self.fds.seek(self.offset)  # not only Mac has this issue, DictReader on Linux also suffers from it
        for row in self.csvreader:
            label = row["label"]

            try:
                rtm = mcs2sec(row["elapsed"])
                ltc = mcs2sec(row["Latency"])
                cnn = mcs2sec(row["Connect"])
                # NOTE: actually we have precise send and receive time here...
            except:
                raise ToolError("PBench reader: failed record: %s" % row)

            if row["opretcode"] != "0":
                error = strerror(int(row["opretcode"]))
                rcd = error
            else:
                error = None
                rcd = row["responseCode"]

            tstmp = int(float(row["timeStamp"]) + rtm)
            byte_count = int(row["brecv"])
            concur = 0
            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, '', byte_count

        self.offset = self.fds.tell()

    def _calculate_datapoints(self, final_pass=False):
        for point in super(PBenchKPIReader, self)._calculate_datapoints(final_pass):
            if self.stats_reader:
                concurrency = self.stats_reader.get_data(point[DataPoint.TIMESTAMP])
            else:
                concurrency = 0

            for label_data in viewvalues(point[DataPoint.CURRENT]):
                label_data[KPISet.CONCURRENCY] = concurrency

            yield point

    def __open_fds(self):
        """
        Opens JTL file for reading
        """
        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet: %s", self.filename)
            return False

        fsize = os.path.getsize(self.filename)
        if not fsize:
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.log.debug("Opening file: %s", self.filename)
        self.fds = open(self.filename)
        fields = ("timeStamp", "label", "elapsed",
                  "Connect", "Send", "Latency", "Receive",
                  "internal",
                  "bsent", "brecv",
                  "opretcode", "responseCode")
        dialect = csv.excel_tab()
        self.csvreader = csv.DictReader(self.fds, fields, dialect=dialect)
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()


class PBenchStatsReader(object):
    MARKER = "\n},"

    def __init__(self, filename, parent_logger):
        super(PBenchStatsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.buffer = ''
        self.fds = None
        self.data = {}
        self.last_data = 0

    def read_file(self, last_pass=False):
        del last_pass

        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet: %s", self.filename)
            return False

        if not self.fds:
            self.log.debug("Opening file: %s", self.filename)
            self.fds = open(self.filename)

        self.buffer += self.fds.read()
        while self.MARKER in self.buffer:
            idx = self.buffer.find(self.MARKER) + len(self.MARKER)
            chunk_str = self.buffer[:idx - 1]
            self.buffer = self.buffer[idx + + 1:]
            chunk = json.loads("{%s}" % chunk_str)

            for date_str in chunk.keys():
                statistics = chunk[date_str]

                date_obj = datetime.datetime.strptime(date_str.split(".")[0], '%Y-%m-%d %H:%M:%S')
                date = int(time.mktime(date_obj.timetuple()))
                self.data[date] = 0

                for benchmark_name in statistics.keys():
                    if not benchmark_name.startswith("benchmark_io"):
                        continue
                    benchmark = statistics[benchmark_name]
                    for method in benchmark:
                        meth_obj = benchmark[method]
                        if "mmtasks" in meth_obj:
                            self.data[date] += meth_obj["mmtasks"][2]

                self.log.debug("Active instances stats for %s: %s", date, self.data[date])

    def get_data(self, tstmp):
        if tstmp in self.data:
            self.last_data = self.data[tstmp]
            return self.data[tstmp]
        else:
            self.log.debug("No active instances info for %s", tstmp)
            return self.last_data

    def __del__(self):
        if self.fds:
            self.fds.close()


class PBench(RequiredTool):
    def __init__(self, parent_logger, tool_path):
        super(PBench, self).__init__("PBench", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug("Trying phantom: %s", self.tool_path)
        try:
            pbench = shell_exec([self.tool_path], stderr=subprocess.STDOUT)
            pbench_out, pbench_err = pbench.communicate()
            self.log.debug("PBench check stdout: %s", pbench_out)
            if pbench_err:
                self.log.warning("PBench check stderr: %s", pbench_err)
            return True
        except (CalledProcessError, OSError):
            self.log.info("Phantom check failed")
            return False

    def install(self):
        raise ToolError("Please install PBench tool manually")
