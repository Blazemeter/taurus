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
from abc import abstractmethod
from collections import defaultdict, OrderedDict
from datetime import datetime
from os import path

from bzt.engine import Reporter
from bzt.modules.monitoring import Monitoring, MonitoringClient
from bzt.six import iteritems
from bzt.utils import load_class


def epoch_to_str(timestamp):
    return datetime.fromtimestamp(timestamp).strftime('%H:%M:%S.%f')[:-3]


def average(values):
    if not values:
        return 0.0
    else:
        return float(sum(values)) / len(values)


def to_mb(bytes_value):
    return float(bytes_value) / 1024 / 1024


class ChromeProfiler(Monitoring):
    """
    Chrome profiler module

    :type client: ChromeClient
    """

    def __init__(self):
        super(ChromeProfiler, self).__init__()
        self.client = None

    def prepare(self):
        self.client = ChromeClient(self.log)
        self.client.engine = self.engine

        processors = self.settings.get("processors", {})
        for proc_name, proc in iteritems(processors):
            class_fqn = proc.get("class", ValueError("Class for performance processor %s is not specified" % proc_name))
            klass = load_class(class_fqn)
            processor = klass(proc, self.client, self.log)
            self.client.add_processor(processor)

        self.client.connect()

    def startup(self):
        self.client.start()

    def check(self):
        self.client.process_data()
        results = self.client.get_data()
        if results:
            for listener in self.listeners:
                listener.monitoring_data(results)
        return False

    def shutdown(self):
        self.client.process_data()
        results = self.client.get_data()
        if results:
            for listener in self.listeners:
                listener.monitoring_data(results)
        self.client.disconnect()

    def post_process(self):
        pass

    def get_aggr_metrics(self):
        self.client.process_data()
        return self.client.get_aggr_metrics()

    def get_custom_tables(self):
        self.client.process_data()
        if self.client is not None:
            return self.client.get_custom_tables()

    def get_tab_label(self):
        self.client.process_data()
        if self.client is not None:
            return self.client.get_tab_label()

    def get_metric_label(self, metric):
        if self.client is not None:
            return self.client.get_metric_label(metric)

    def get_custom_tables_json(self):
        return {
            "tables": self.get_custom_tables()
        }


class MetricExtractor(object):
    def __init__(self, processor, log):
        self.processor = processor
        self.log = log.getChild(self.__class__.__name__)
        self.tracing_tab_pid = None
        self.tracing_page_id = None

    def convert_ts(self, timestamp):
        return self.processor.convert_ts(timestamp)

    def offset_to_epoch(self, offset):
        return self.processor.start_time + offset

    def reset(self):
        self.tracing_tab_pid = None
        self.tracing_page_id = None

    def aggregate_by_ts(self, pid_stats, aggregate_func=average):
        # TODO: configurable granularity?
        per_ts = dict()  # ts -> [measurement at ts]
        for offset, value in iteritems(pid_stats):
            base_ts = int(self.convert_ts(offset))
            if base_ts not in per_ts:
                per_ts[base_ts] = []
            per_ts[base_ts].append(value)
        return {
            ts: aggregate_func(values_at_ts)
            for ts, values_at_ts in iteritems(per_ts)
        }

    def categories(self):
        return {"disabled-by-default-devtools.timeline"}

    def metrics(self):
        return {}

    def process_event(self, trace_event):
        if trace_event["name"] == "TracingStartedInPage":
            self.tracing_tab_pid = trace_event["pid"]
            self.tracing_page_id = trace_event["args"]["data"]["page"]

    def calc_metrics(self):
        return iter([])

    def calc_aggregates(self):
        return iter([])

    def get_custom_tables(self):
        return []


class MemoryMetricsExtractor(MetricExtractor):
    BROWSER_MEMORY = 'memory-browser'
    TAB_MEMORY = 'memory-tab'
    AVERAGE_BROWSER_MEMORY = 'memory-average-browser'
    AVERAGE_TAB_MEMORY = 'memory-average-tab'

    def __init__(self, processor, log):
        super(MemoryMetricsExtractor, self).__init__(processor, log)
        self.memory_per_process = defaultdict(dict)

    def reset(self):
        super(MemoryMetricsExtractor, self).reset()
        self.memory_per_process.clear()

    def categories(self):
        return super(MemoryMetricsExtractor, self).categories() | {"disabled-by-default-memory-infra"}

    def metrics(self):
        return {self.TAB_MEMORY: "Tab memory usage",
                self.BROWSER_MEMORY: "Browser memory usage",
                self.AVERAGE_TAB_MEMORY: "Average tab memory usage",
                self.AVERAGE_BROWSER_MEMORY: "Average browser memory usage"}

    def process_event(self, trace_event):
        super(MemoryMetricsExtractor, self).process_event(trace_event)

        if trace_event.get("name") == "periodic_interval":
            pid = trace_event["pid"]
            ets = trace_event['ts']
            dumps = trace_event['args']['dumps']
            if 'process_totals' not in dumps:
                return
            totals = dumps['process_totals']
            resident_mbytes = to_mb(int(totals['resident_set_bytes'], 16))
            self.memory_per_process[pid][ets] = resident_mbytes

    def reaggregate_by_ts(self, per_pid_stats, aggregate_func=average):
        # TODO: sub-second granularity
        per_ts = dict()  # ts -> (pid -> [measurement at ts])
        for pid in per_pid_stats:
            for offset, value in iteritems(per_pid_stats[pid]):
                base_ts = int(self.convert_ts(offset))
                if base_ts not in per_ts:
                    per_ts[base_ts] = {}
                if pid not in per_ts[base_ts]:
                    per_ts[base_ts][pid] = []
                per_ts[base_ts][pid].append(value)
        return {
            ts: {
                pid: aggregate_func(pid_measurements)
                for pid, pid_measurements in iteritems(stats_per_ts)
            }
            for ts, stats_per_ts in iteritems(per_ts)
        }

    def calc_metrics(self):
        tab_pid = self.tracing_tab_pid
        if tab_pid not in self.memory_per_process:
            return

        memory_per_ts = self.reaggregate_by_ts(self.memory_per_process)  # ts -> (pid -> memory)
        for offset in sorted(memory_per_ts):
            process_mems = memory_per_ts[offset]
            tab_memory_at_ts = process_mems[tab_pid]
            yield offset, self.TAB_MEMORY, tab_memory_at_ts
            browser_memory_at_ts = sum(per_process for _, per_process in iteritems(process_mems))
            yield offset, self.BROWSER_MEMORY, browser_memory_at_ts

    def calc_aggregates(self):
        tab_pid = self.tracing_tab_pid
        if tab_pid not in self.memory_per_process:
            return

        memory_per_ts = self.reaggregate_by_ts(self.memory_per_process)
        tab_memory = [process_stats[tab_pid] for ts, process_stats in iteritems(memory_per_ts)]
        yield self.AVERAGE_TAB_MEMORY, average(tab_memory)
        browser_memory = [sum(process_memory for _, process_memory in iteritems(process_stats))
                          for _, process_stats in iteritems(memory_per_ts)]
        yield self.AVERAGE_BROWSER_MEMORY, average(browser_memory)

    def get_custom_tables(self):
        rows = []
        metrics = self.metrics()
        for name, value in self.calc_aggregates():
            row = OrderedDict()
            row["Label"] = metrics[name]
            if isinstance(value, float):
                value = "%.2f" % value
            row["Value"] = value
            rows.append(row)
        table = {"id": "Memory", "name": "Memory metrics",
                 "description": "Memory metrics.",
                 "data": rows}
        return [table]


class TabNameExtractor(MetricExtractor):
    def __init__(self, processor, log):
        super(TabNameExtractor, self).__init__(processor, log)
        self.process_labels = {}

    def reset(self):
        super(TabNameExtractor, self).reset()
        self.process_labels.clear()

    def categories(self):
        return super(TabNameExtractor, self).categories() | {"__metadata"}

    def process_event(self, trace_event):
        super(TabNameExtractor, self).process_event(trace_event)
        if trace_event["name"] == "process_labels":
            pid = trace_event["pid"]
            label = trace_event["args"]["labels"]
            self.process_labels[pid] = label

    def get_tab_label(self):
        if self.tracing_tab_pid is None:
            return None
        elif self.tracing_tab_pid not in self.process_labels:
            return None
        else:
            return self.process_labels[self.tracing_tab_pid]


class PerformanceDataProcessor(object):
    def __init__(self, filename, chrome_client, log):
        self.chrome_client = chrome_client
        self.data_file = filename
        self.log = log.getChild(self.__class__.__name__)
        self.data_file_ctime = None
        self.should_reread = False

    def process_file(self):
        "Read (or reread) and process file"
        should_process = False
        if path.exists(self.data_file):
            ctime = path.getctime(self.data_file)
            if self.data_file_ctime is None:
                self.log.debug("Recording file %s ctime %s", self.data_file, epoch_to_str(ctime))
                self.data_file_ctime = ctime
                should_process = True
            elif self.data_file_ctime != ctime and abs(self.data_file_ctime - ctime) >= 2.0:
                self.log.debug("File %s ctime updated, resetting, (%s)", self.data_file, epoch_to_str(ctime))
                self.data_file_ctime = ctime
                self.reset_cache()
                should_process = True
        if should_process or self.should_reread:
            self.should_reread = False
            self.process_data()

    @abstractmethod
    def reset_cache(self):
        pass

    @abstractmethod
    def process_data(self):
        pass

    @abstractmethod
    def get_metric_label(self, metric):
        pass

    @abstractmethod
    def get_metrics(self):
        "Calculate metrics that are defined over time"
        pass

    @abstractmethod
    def get_aggr_metrics(self):
        "Calculate aggregated metrics (defined once per test run)"
        pass

    @abstractmethod
    def get_custom_tables(self):
        "Create custom tables with metrics"
        pass


class TraceProcessor(PerformanceDataProcessor):
    """
    Chrome performance client

    :type extractors: list[MetricExtractor]
    """
    def __init__(self, config, chrome_client, parent_log):
        trace_file_path = path.join(chrome_client.engine.artifacts_dir, config.get("file", "trace.json"))
        super(TraceProcessor, self).__init__(trace_file_path, chrome_client, parent_log)

        self.config = config

        # epoch timestamp of Chrome start
        # (used to convert offsets from Chrome start into epoch timestamps)
        self.start_time = None

        # Chrome clock tracing start offset and duration
        # (used to convert timestamps from trace events into offsets in seconds)
        self.tracing_start_ts = None
        self.tracing_duration = 0.0

        # list of metrics extractors
        # tab name extractor is always on
        self.extractors = []

        self.instantiate_extractors(self.config.get("extractors", []))

    def instantiate_extractors(self, extractors):
        for extractor_fqn in extractors:
            extractor_class = load_class(extractor_fqn)
            extractor = extractor_class(self, self.log)
            self.extractors.append(extractor)

    def convert_ts(self, timestamp):
        if self.tracing_start_ts is not None:
            offset = float(timestamp) / 1000000 - self.tracing_start_ts
            return max(offset, 0.0)
        else:
            return 0.0

    def process_data(self):
        with open(self.data_file, 'r') as fds:
            try:
                events = json.load(fds)
            except ValueError:
                self.should_reread = True
                return

        for event in events:
            self.process_event(event)

    def reset_cache(self):
        for extractor in self.extractors:
            extractor.reset()
        self.start_time = None
        self.tracing_start_ts = None
        self.tracing_duration = 0.0

    def process_event(self, event):
        # see https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/edit#
        # for trace event format
        if event.get("ts") and self.tracing_start_ts is None:
            self.tracing_start_ts = float(event["ts"]) / 1000000
        if event.get("ts"):
            ets = self.convert_ts(event['ts'])
            self.tracing_duration = max(self.tracing_duration, ets)

        categories = event.get("cat", "").split(",")

        if "__metadata" in categories and event.get("name") == "start_time":
            self.start_time = event["args"]["timestamp"]

        for extractor in self.extractors:
            if extractor.categories() & set(categories):
                extractor.process_event(event)

    def get_metric_label(self, metric):
        for extractor in self.extractors:
            metrics = extractor.metrics()
            if metric in metrics:
                return metrics[metric]

    def get_metrics(self):
        # yields (offset, metric, value) for all metrics that are defined in time
        # offset is number of seconds since the start of Chrome
        for extractor in self.extractors:
            for offset, metric, value in extractor.calc_metrics():
                yield offset, metric, value

    def get_aggr_metrics(self):
        # yields (metric, value) for metrics that are reported one time
        for extractor in self.extractors:
            for metric in extractor.calc_aggregates():
                yield metric

    def get_custom_tables(self):
        tables = []
        for extractor in self.extractors:
            extra_tables = extractor.get_custom_tables()
            if extra_tables:
                tables.extend(extra_tables)
        return tables

    def get_tab_label(self):
        for extractor in self.extractors:
            if isinstance(extractor, TabNameExtractor):
                return extractor.get_tab_label()


class ChromeClient(MonitoringClient):
    """
    Chrome performance client

    :type engine: bzt.Engine
    :type processors: list[PerformanceDataProcessor]
    """
    def __init__(self, parent_logger):
        super(ChromeClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.engine = None
        self.processors = []

    def add_processor(self, proc):
        self.processors.append(proc)

    def connect(self):
        pass

    def start(self):
        pass

    def process_data(self):
        for processor in self.processors:
            processor.process_file()

    def get_start_time(self):
        for processor in self.processors:
            if isinstance(processor, TraceProcessor):
                return processor.start_time

    def get_data(self):
        start_time = self.get_start_time()
        if start_time is None:
            return []
        datapoints = []
        for offset, metric, value in self.get_metrics():
            item = {
                "ts": start_time + offset,
                "source": "chrome",
                metric: value
            }
            datapoints.append(item)
        return datapoints

    def get_metrics(self):
        for processor in self.processors:
            # noinspection PyTypeChecker
            for metric in processor.get_metrics():
                yield metric

    def get_aggr_metrics(self):
        res = {}
        for processor in self.processors:
            # noinspection PyTypeChecker
            for metric, value in processor.get_aggr_metrics():
                res[metric] = value
        return res

    def get_metric_label(self, metric):
        for processor in self.processors:
            label = processor.get_metric_label(metric)
            if label:
                return label

    def get_custom_tables(self):
        tables = []
        for processor in self.processors:
            # noinspection PyTypeChecker
            for table in processor.get_custom_tables():
                tables.append(table)
        return tables

    def get_tab_label(self):
        for processor in self.processors:
            if isinstance(processor, TraceProcessor):
                return processor.get_tab_label()

    def disconnect(self):
        pass


class MetricReporter(Reporter):
    def __init__(self):
        super(MetricReporter, self).__init__()
        self.chrome_profiler = None

    def prepare(self):
        for module in self.engine.services:
            if isinstance(module, ChromeProfiler):
                self.chrome_profiler = module

    def post_process(self):
        if self.chrome_profiler is None:
            return

        metrics = self.chrome_profiler.get_aggr_metrics()
        if metrics is not None:
            tab_label = self.chrome_profiler.get_tab_label()
            self.log.info("Chrome metrics for tab '%s':", tab_label)

        for table in self.chrome_profiler.get_custom_tables():
            self.log.info(table["name"] + ":")
            rows = table["data"]
            if not rows:
                continue
            first_row = rows[0]
            self.log.info(" | ".join(title for title, _ in iteritems(first_row)))
            for row in rows:
                self.log.info(" | ".join(str(value) for _, value in iteritems(row)))
            self.log.info("")
