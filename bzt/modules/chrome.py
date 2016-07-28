"""
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
import json
import logging
from collections import defaultdict, OrderedDict
from os import path

from bzt.modules.monitoring import Monitoring, MonitoringClient
from bzt.six import iteritems


class ChromeProfiler(Monitoring):
    def __init__(self):
        super(ChromeProfiler, self).__init__()
        self.client = None

    def prepare(self):
        log_file = self.parameters.get('trace-file', "trace.json")
        log_path = path.join(self.engine.artifacts_dir, log_file)
        self.client = ChromeLogClient(log_path, self.log)
        self.client.engine = self.engine
        self.client.connect()

    def startup(self):
        self.client.start()
        super(Monitoring, self).startup()

    def check(self):
        results = self.client.get_data()
        if results:
            for listener in self.listeners:
                listener.monitoring_data(results)
        return super(Monitoring, self).check()

    def shutdown(self):
        results = self.client.get_data()
        if results:
            for listener in self.listeners:
                listener.monitoring_data(results)
        self.client.disconnect()
        super(Monitoring, self).shutdown()

    def post_process(self):
        super(Monitoring, self).post_process()


class ChromeTraceReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None

    def read_trace(self):
        if not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        try:
            events = json.load(self.fds)
        except ValueError:
            self.log.debug("Can't parse trace, it might not be full")
            return

        for event in events:
            yield event

    def __open_fds(self):
        if not path.isfile(self.filename):
            return False
        fsize = path.getsize(self.filename)
        if not fsize:
            return False
        self.fds = open(self.filename, 'rt', buffering=1)
        return True

    def __del__(self):
        if self.fds is not None:
            self.fds.close()


class ChromeMetricExtractor(object):
    def __init__(self):
        self.tracing_start_ts = None
        self.process_names = {}  # pid -> str
        self.process_labels = {}  # pid -> str
        self.memory_per_process = defaultdict(OrderedDict)  # pid -> (ts -> float)

    def feed_event(self, event):
        if event.get("timestamp") and self.tracing_start_ts is None:
            self.tracing_start_ts = event["timestamp"]
        if event.get("cat") == "disabled-by-default-memory-infra":
            if event.get("name") == "periodic_interval":
                self.process_memory_event(event)
        elif event.get("cat") == "__metadata":
            self.process_metadata_event(event)

    def process_metadata_event(self, event):
        pid = event["pid"]
        if event.get("name") == "process_name":
            self.process_names[pid] = event['args']['name']
        elif event.get("name") == "process_labels":
            self.process_labels[pid] = event['args']['labels']

    def process_memory_event(self, event):
        pid = event["pid"]
        ts = float(event["ts"]) / 1000000
        if self.tracing_start_ts is not None:
            ts = ts - self.tracing_start_ts
        dumps = event['args']['dumps']
        if 'process_totals' not in dumps:
            return
        totals = dumps['process_totals']
        resident_mbytes = float(int(totals['resident_set_bytes'], 16)) / 1024 / 1024
        self.memory_per_process[pid][ts] = resident_mbytes

    def get_metrics(self):
        # yields (offset, metric, value)
        # offset is number of seconds since the start of Chrome
        if self.memory_per_process:
            for pid in sorted(self.memory_per_process):
                for ts, value in iteritems(self.memory_per_process[pid]):
                    name = self.process_names.get(pid, pid)
                    if pid in self.process_labels:
                        name += "(%s)" % self.process_labels[pid]
                    metric = '%s-mem-mb' % name
                    yield ts, metric, value


class ChromeLogClient(MonitoringClient):
    def __init__(self, log_file, parent_logger):
        super(ChromeLogClient, self).__init__()
        self.log_file = log_file
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.reader = ChromeTraceReader(log_file, self.log)
        self.extractor = ChromeMetricExtractor()
        self.engine = None

    def connect(self):
        pass

    def start(self):
        pass

    def get_data(self):
        for event in self.reader.read_trace():
            self.extractor.feed_event(event)

        # TODO: find a better way to find when Chrome was started
        if not path.exists(self.log_file):
            return []
        start_time = path.getctime(self.log_file)  #

        res = []
        for offset, metric, value in self.extractor.get_metrics():
            self.log.info("%s: %s=%s", offset, metric, value)
            item = {
                "ts": start_time + offset,
                "source": "chrome",
                metric: value
            }
            res.append(item)
        return res

    def disconnect(self):
        pass
