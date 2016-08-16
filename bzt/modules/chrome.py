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
        processors = self.parameters.get("processors", [])

        if not processors:
            processors.append({"class": "bzt.modules.chrome.TraceProcessor", "file": "trace.json"})

        self.client = ChromeClient(self.log)
        self.client.engine = self.engine

        for proc in processors:
            klass = load_class(proc.get("class", ValueError("You must specify class for performance processor")))
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
        return False  # profiling never stops

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


class DOMMetricsExtractor(MetricExtractor):
    DOM_DOCUMENTS = 'dom-documents'
    DOM_NODES = 'dom-nodes'
    EVENT_LISTENERS = 'dom-event-listeners'

    FINAL_EVENT_LISTENERS = 'dom-final-event-listeners'
    FINAL_DOM_DOCUMENTS = 'dom-final-documents'
    FINAL_DOM_NODES = 'dom-final-nodes'

    def __init__(self, processor, log):
        super(DOMMetricsExtractor, self).__init__(processor, log)
        self.dom_documents = defaultdict(OrderedDict)  # pid -> (timestamp -> documents)
        self.dom_nodes = defaultdict(OrderedDict)  # pid -> (timestamp -> nodes)
        self.event_listeners = defaultdict(OrderedDict)  # pid -> (timestamp -> listeners)

    def reset(self):
        super(DOMMetricsExtractor, self).reset()
        self.dom_documents.clear()
        self.dom_nodes.clear()
        self.event_listeners.clear()

    def categories(self):
        return super(DOMMetricsExtractor, self).categories() | {"disabled-by-default-devtools.timeline"}

    def metrics(self):
        return {self.DOM_DOCUMENTS: "DOM documents",
                self.DOM_NODES: "DOM nodes",
                self.EVENT_LISTENERS: "JS event listeners",
                self.FINAL_DOM_DOCUMENTS: "Number of DOM documents at the end",
                self.FINAL_DOM_NODES: "Number of DOM nodes at the end",
                self.FINAL_EVENT_LISTENERS: "Number of event listeners at the end"}

    def process_event(self, trace_event):
        super(DOMMetricsExtractor, self).process_event(trace_event)
        if trace_event["name"] == "UpdateCounters":
            timestamp = trace_event['ts']
            pid = trace_event["pid"]
            self.dom_documents[pid][timestamp] = trace_event["args"]["data"]["documents"]
            self.dom_nodes[pid][timestamp] = trace_event["args"]["data"]["nodes"]
            self.event_listeners[pid][timestamp] = trace_event["args"]["data"]["jsEventListeners"]

    def calc_metrics(self):
        tab_pid = self.tracing_tab_pid
        if tab_pid not in self.dom_documents or tab_pid not in self.dom_nodes:
            return

        docs_per_ts = self.aggregate_by_ts(self.dom_documents[tab_pid])
        for offset in sorted(docs_per_ts):
            docs = docs_per_ts[offset]
            yield offset, self.DOM_DOCUMENTS, int(round(docs))

        nodes_per_ts = self.aggregate_by_ts(self.dom_nodes[tab_pid])
        for offset in sorted(nodes_per_ts):
            nodes = nodes_per_ts[offset]
            yield offset, self.DOM_NODES, int(round(nodes))

        listeners_per_ts = self.aggregate_by_ts(self.event_listeners[self.tracing_tab_pid])
        for offset in sorted(listeners_per_ts):
            listeners = listeners_per_ts[offset]
            yield offset, self.EVENT_LISTENERS, int(round(listeners))

    def calc_aggregates(self):
        tab_pid = self.tracing_tab_pid
        if tab_pid not in self.dom_documents or tab_pid not in self.dom_nodes:
            return

        docs_per_ts = self.aggregate_by_ts(self.dom_documents[tab_pid])
        max_ts = max(docs_per_ts)
        yield self.FINAL_DOM_DOCUMENTS, int(round(docs_per_ts[max_ts]))

        nodes_per_ts = self.aggregate_by_ts(self.dom_nodes[tab_pid])
        max_ts = max(nodes_per_ts)
        yield self.FINAL_DOM_NODES, int(round(nodes_per_ts[max_ts]))

        listeners_per_ts = self.aggregate_by_ts(self.event_listeners[self.tracing_tab_pid])
        max_ts = max(listeners_per_ts)
        yield self.FINAL_EVENT_LISTENERS, int(round(listeners_per_ts[max_ts]))

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
        table = {"id": "Time", "name": "DOM metrics",
                 "description": "DOM performance metrics.",
                 "data": rows}
        return [table]


class NetworkMetricsExtractor(MetricExtractor):
    NETWORK_FOOTPRINT = 'network-footprint'
    NETWORK_REQUESTS = 'network-http-requests'
    NETWORK_XHR_REQUESTS = 'network-xhr-requests'
    TIME_TO_FIRST_BYTE = 'network-time-to-first-byte'
    FULL_LOAD_TIME = 'time-full-load-time'

    def __init__(self, processor, log):
        super(NetworkMetricsExtractor, self).__init__(processor, log)
        self.requests = defaultdict(dict)  # request_id -> dict(pid, send_req_time, method, url, recv_resp_time,
        #                                                       recv_data_time, mime, status_code, size, did_fail,
        #                                                       finish_time, network_time)
        self.xhr_requests = defaultdict(list)  # pid -> [(ts, url)]

    def reset(self):
        super(NetworkMetricsExtractor, self).reset()
        self.requests.clear()
        self.xhr_requests.clear()

    def categories(self):
        return super(NetworkMetricsExtractor, self).categories() | {"devtools.timeline"}

    def metrics(self):
        return {self.NETWORK_FOOTPRINT: "Network footprint",
                self.NETWORK_REQUESTS: "Number of HTTP requests",
                self.NETWORK_XHR_REQUESTS: "Number of AJAX requests",
                self.TIME_TO_FIRST_BYTE: "Time to first byte",
                self.FULL_LOAD_TIME: "Time for full page load"}

    def process_event(self, trace_event):
        super(NetworkMetricsExtractor, self).process_event(trace_event)
        if trace_event["name"] == "ResourceSendRequest":
            pid = trace_event['pid']
            timestamp = trace_event['ts']
            request_id = trace_event["args"]["data"]["requestId"]
            self.requests[request_id]["send_req_time"] = timestamp
            self.requests[request_id]["pid"] = pid
            self.requests[request_id]["method"] = trace_event["args"]["data"]["requestMethod"]
            self.requests[request_id]["url"] = trace_event["args"]["data"]["url"]
        elif trace_event["name"] == "ResourceReceiveResponse":
            timestamp = trace_event['ts']
            request_id = trace_event["args"]["data"]["requestId"]
            self.requests[request_id]["recv_resp_time"] = timestamp
            self.requests[request_id]["mime"] = trace_event["args"]["data"]["mimeType"]
            self.requests[request_id]["status_code"] = trace_event["args"]["data"]["statusCode"]
        elif trace_event["name"] == "ResourceReceivedData":
            timestamp = trace_event['ts']
            request_id = trace_event["args"]["data"]["requestId"]
            self.requests[request_id]["recv_data_time"] = timestamp
            data_len = trace_event["args"]["data"]["encodedDataLength"]
            size = self.requests[request_id].get("size", 0)
            self.requests[request_id]["size"] = size + data_len
        elif trace_event["name"] == "ResourceFinish":
            timestamp = trace_event['ts']
            request_id = trace_event["args"]["data"]["requestId"]
            self.requests[request_id]["finish_time"] = timestamp
            self.requests[request_id]["did_fail"] = trace_event["args"]["data"]["didFail"]
            if trace_event["args"]["data"].get("networkTime"):
                self.requests[request_id]["network_time"] = trace_event["args"]["data"]["networkTime"]
        elif trace_event["name"] == "XHRLoad":
            # we can also track XHRReadyStateChange event to track all state changes
            ets = trace_event['ts']
            pid = trace_event["pid"]
            url = trace_event["args"]["data"]["url"]
            self.xhr_requests[pid].append((ets, url))

    def calc_aggregates(self):
        tab_pid = self.tracing_tab_pid
        tab_requests = list(req for _, req in iteritems(self.requests) if req.get("pid") == tab_pid)
        if not tab_requests:
            return

        yield self.NETWORK_REQUESTS, len(tab_requests)

        total = 0.0
        for request in tab_requests:
            payload_size = request.get("size", 0)
            total += payload_size
        yield self.NETWORK_FOOTPRINT, to_mb(total)

        first = min(tab_requests, key=lambda r: r.get("recv_data_time", float("inf")))
        ttfb = first['recv_data_time']
        yield self.TIME_TO_FIRST_BYTE, self.convert_ts(ttfb)

        yield self.NETWORK_XHR_REQUESTS, len(self.xhr_requests.get(tab_pid, []))

        last = max(tab_requests, key=lambda r: r.get("finish_time", float("-inf")))
        last_request_time = last['finish_time']
        yield self.FULL_LOAD_TIME, self.convert_ts(last_request_time)

    def get_custom_tables(self):
        tables = []

        rows = []
        metrics = self.metrics()
        for name, value in self.calc_aggregates():
            row = OrderedDict()
            row["Label"] = metrics[name]
            if isinstance(value, float):
                value = "%.2f" % value
            row["Value"] = value
            rows.append(row)
        tables.append({
            "id": "Network", "name": "Network metrics",
            "description": "Network performance metrics.",
            "data": rows,
        })

        requests = [req for _, req in iteritems(self.requests) if req.get("pid") == self.tracing_tab_pid]
        rows = []
        for req in sorted(requests, key=lambda r: r.get("send_req_time")):
            row = OrderedDict()
            row["Start time"] = req.get("send_req_time")
            row["End time"] = req.get("finish_time")
            row["Method"] = req.get("method")
            url = req.get("url")
            if len(url) > 60:
                url = url[:57] + "..."
            row["URL"] = url
            row["HTTP Status"] = req.get("status_code", "unknown")
            row["MIME"] = req.get("mime", "unknown")
            row["Size"] = req.get("size")
            if not any(value is None for _, value in iteritems(row)):
                row["Start time"] = epoch_to_str(self.offset_to_epoch(self.convert_ts(row["Start time"])))
                row["End time"] = epoch_to_str(self.offset_to_epoch(self.convert_ts(row["End time"])))
                rows.append(row)
        tables.append({"id": "HTTP", "name": "HTTP requests",
                       "description": "HTTP requests made by page",
                       "data": rows})

        rows = []
        for req_time, url in sorted(self.xhr_requests[self.tracing_tab_pid]):
            row = OrderedDict()
            row["Start time"] = epoch_to_str(self.offset_to_epoch(self.convert_ts(req_time)))
            if len(url) > 60:
                url = url[:57] + "..."
            row["URL"] = url
            rows.append(row)
        if rows:
            tables.append({"id": "AJAX", "name": "AJAX requests",
                           "description": "AJAX requests made by page",
                           "data": rows})
        return tables


class MemoryMetricsExtractor(MetricExtractor):
    BROWSER_MEMORY = 'memory-browser'
    TAB_MEMORY = 'memory-tab'

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
                self.BROWSER_MEMORY: "Browser memory usage"}

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


class JavaScriptMetricsExtractor(MetricExtractor):
    CPU_USAGE = 'js-cpu-usage'
    JS_HEAP = 'js-heap-usage'

    AVERAGE_JS_HEAP = 'js-average-heap'
    TOTAL_GC_TIME = 'js-total-gc-time'

    def __init__(self, processor, log):
        super(JavaScriptMetricsExtractor, self).__init__(processor, log)
        self.gc_times = defaultdict(list)
        self.v8_complete_events = defaultdict(list)
        self.used_heap = defaultdict(dict)  # pid -> (timestamp -> heap_size)

    def reset(self):
        super(JavaScriptMetricsExtractor, self).reset()
        self.gc_times.clear()
        self.v8_complete_events.clear()
        self.used_heap.clear()

    def categories(self):
        return super(JavaScriptMetricsExtractor, self).categories() | {"v8", "disabled-by-default-devtools.timeline"}

    def metrics(self):
        return {
            self.JS_HEAP: "JS heap size",
            self.CPU_USAGE: "JavaScript CPU utilization",
            self.TOTAL_GC_TIME: "Time spent doing GC in JS engine",
            self.AVERAGE_JS_HEAP: "Average JS heap size",
        }

    def process_event(self, trace_event):
        super(JavaScriptMetricsExtractor, self).process_event(trace_event)

        cats = trace_event.get("cat", "").split(",")

        if "v8" in cats and trace_event.get("ph") == "X":
            pid = trace_event['pid']
            ets = trace_event['ts']
            name = trace_event['name']
            duration = trace_event.get('dur', 1)
            self.v8_complete_events[pid].append((ets, name, duration))

        if trace_event.get("name") in ["MajorGC", "MinorGC"]:
            pid = trace_event["pid"]
            if trace_event.get("ph") == "B":  # GC begin
                item = {'gc_start_time': trace_event['ts'],
                        'heap_before_gc': to_mb(trace_event['args']['usedHeapSizeBefore'])}
                self.gc_times[pid].append(item)
            elif trace_event.get("ph") == "E":  # GC end
                if self.gc_times[pid]:
                    self.gc_times[pid][-1]['gc_end_time'] = trace_event['ts']
                    self.gc_times[pid][-1]['heap_after_gc'] = to_mb(trace_event['args']['usedHeapSizeAfter'])

        if trace_event["name"] == "UpdateCounters":
            timestamp = trace_event['ts']
            pid = trace_event["pid"]
            self.used_heap[pid][timestamp] = trace_event["args"]["data"]["jsHeapSizeUsed"]

    def calc_metrics(self):
        tab_pid = self.tracing_tab_pid
        if tab_pid not in self.v8_complete_events or tab_pid not in self.used_heap:
            return

        heap_per_ts = self.aggregate_by_ts(self.used_heap[self.tracing_tab_pid])
        for offset in sorted(heap_per_ts):
            tab_heap = heap_per_ts[offset]
            yield offset, self.JS_HEAP, to_mb(tab_heap)

        v8_events = self.v8_complete_events[self.tracing_tab_pid]
        # reaggregate by 1 second
        per_ts = dict()  # ts -> (pid -> [measurement at ts])
        for ets, _, duration in v8_events:
            base_ts = int(self.convert_ts(ets))
            if base_ts not in per_ts:
                per_ts[base_ts] = []
            per_ts[base_ts].append(duration)
        # yield timeline
        for ets in sorted(per_ts):
            usage = sum(per_ts[ets])
            cpu_at_ts = float(usage) / 1000000
            yield float(ets), self.CPU_USAGE, cpu_at_ts * 100

    def calc_aggregates(self):
        tab_pid = self.tracing_tab_pid
        if tab_pid not in self.gc_times or tab_pid not in self.used_heap:
            return

        total_gc_time = 0.0
        gcs = self.gc_times[tab_pid]
        for gc_record in gcs:
            if 'gc_start_time' in gc_record and 'gc_end_time' in gc_record:
                gc_duration = float(gc_record['gc_end_time'] - gc_record['gc_start_time']) / 1000000
                total_gc_time += gc_duration
        yield self.TOTAL_GC_TIME, total_gc_time

        heap_per_ts = self.aggregate_by_ts(self.used_heap[self.tracing_tab_pid])
        yield self.AVERAGE_JS_HEAP, to_mb(average([value for _, value in iteritems(heap_per_ts)]))

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
        table = {"id": "JS", "name": "JavaScript metrics",
                 "description": "JavaScript performance metrics",
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


class LoadTimeMetricsExtractor(MetricExtractor):
    LOAD_TIME = "time-load-time"
    DOM_CONTENT_LOAD_TIME = "time-dom-content-load-time"
    FIRST_PAINT_TIME = "time-first-paint-time"

    def __init__(self, processor, log):
        super(LoadTimeMetricsExtractor, self).__init__(processor, log)
        self.commit_load_events = defaultdict(dict)  # pid -> (ts -> page id)
        self.composite_layers_events = defaultdict(list)  # pid -> [ts]
        self.draw_frame_events = defaultdict(list)  # pid -> [ts]
        self.load_events = defaultdict(dict)  # pid -> (ts -> frame id)
        self.dom_content_loaded_events = defaultdict(dict)  # pid -> (ts -> frame id)

    def reset(self):
        super(LoadTimeMetricsExtractor, self).reset()
        self.commit_load_events.clear()
        self.composite_layers_events.clear()
        self.draw_frame_events.clear()
        self.load_events.clear()
        self.dom_content_loaded_events.clear()

    def categories(self):
        cats = {"blink.user_timing",
                "devtools.timeline",
                "disabled-by-default-devtools.timeline",
                "disabled-by-default-devtools.timeline.frame"}
        return super(LoadTimeMetricsExtractor, self).categories() | cats

    def metrics(self):
        return {
            self.LOAD_TIME: "Time to page load",
            self.DOM_CONTENT_LOAD_TIME: "Time to DOMContentLoad event",
            self.FIRST_PAINT_TIME: "Time to first paint",
        }

    def process_event(self, event):
        super(LoadTimeMetricsExtractor, self).process_event(event)

        if event.get("name") == "loadEventStart":
            pid = event['pid']
            ets = event['ts']
            frame = event["args"]["frame"]
            self.load_events[pid][ets] = frame
        elif event.get("name") == "domContentLoadedEventStart":
            pid = event['pid']
            ets = event['ts']
            frame = event["args"]["frame"]
            self.dom_content_loaded_events[pid][ets] = frame
        elif event.get("name") == "CommitLoad":
            pid = event["pid"]
            ets = event["ts"]
            page_id = event["args"]["data"]["page"]
            self.commit_load_events[pid][ets] = page_id
        elif event.get("name") == "CompositeLayers":
            pid = event["pid"]
            ets = event["ts"]
            self.composite_layers_events[pid].append(ets)
        elif event.get("name") == "DrawFrame":
            pid = event["pid"]
            ets = event["ts"]
            self.draw_frame_events[pid].append(ets)

    def calc_page_load_time(self):
        if self.tracing_tab_pid in self.load_events:
            page_load_ts = min(ts
                               for ts, frame_id in iteritems(self.load_events[self.tracing_tab_pid])
                               if frame_id == self.tracing_page_id)
            return self.convert_ts(page_load_ts)

    def calc_dom_content_load_time(self):
        if self.tracing_tab_pid in self.dom_content_loaded_events:
            dom_load = max(ts
                           for ts, frame_id in iteritems(self.dom_content_loaded_events[self.tracing_tab_pid])
                           if frame_id == self.tracing_page_id)
            return self.convert_ts(dom_load)

    def calc_first_paint_time(self):
        # "consider first DrawFrame that happens after first CompositeLayers
        #  that follows the last CommitLoad event to be first paint" (Chromium sources)
        tab_pid = self.tracing_tab_pid
        if not tab_pid in self.commit_load_events:
            return None
        commit_loads = [
            offset
            for offset, page_id in iteritems(self.commit_load_events[tab_pid])
            if page_id == self.tracing_page_id
        ]
        if not commit_loads:
            return None
        last_commit_load = max(commit_loads)
        next_composite_layers = None
        if tab_pid not in self.composite_layers_events:
            return None
        for composite_ts in sorted(self.composite_layers_events[tab_pid]):
            if composite_ts >= last_commit_load:
                next_composite_layers = composite_ts
                break
        if tab_pid not in self.draw_frame_events:
            return None
        if next_composite_layers:
            next_draw_frame = None
            for draw_frame_ts in sorted(self.draw_frame_events[tab_pid]):
                if draw_frame_ts >= next_composite_layers:
                    next_draw_frame = draw_frame_ts
                    break
            if next_draw_frame:
                return self.convert_ts(next_draw_frame)

    def calc_aggregates(self):
        load_time = self.calc_page_load_time()
        if load_time:
            yield self.LOAD_TIME, load_time
        dom_content_load_time = self.calc_dom_content_load_time()
        if dom_content_load_time:
            yield self.DOM_CONTENT_LOAD_TIME, dom_content_load_time
        first_paint_time = self.calc_first_paint_time()
        if first_paint_time:
            yield self.FIRST_PAINT_TIME, first_paint_time

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
        table = {"id": "Times", "name": "Page load times",
                 "description": "Page load performance metrics",
                 "data": rows}
        return [table]


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
    EXTRACTORS = {
        'dom': DOMMetricsExtractor,
        'memory': MemoryMetricsExtractor,
        'network': NetworkMetricsExtractor,
        'js': JavaScriptMetricsExtractor,
        'timing': LoadTimeMetricsExtractor,
    }

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
        self.extractors = [
            TabNameExtractor(self, self.log),
        ]
        extractors = self.config.get("extractors", ["dom", "memory", "network", "js", "timing"])
        self.instantiate_extractors(extractors)

    def instantiate_extractors(self, extractor_names):
        for name in extractor_names:
            extractor_class = self.EXTRACTORS.get(name)
            if extractor_class is None:
                self.log.error("Invalid trace extractor name: %s", name)
                self.log.info("Avaliable extractors: %s", [name for name, _ in iteritems(self.EXTRACTORS)])
                raise ValueError("Invalid trace extractor name: %s" % name)
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
            if tab_label is not None:
                self.log.info("")
                self.log.info("Chrome metrics for tab '%s':", tab_label)

        for table in self.chrome_profiler.get_custom_tables():
            self.log.info("")
            self.log.info(table["name"] + ":")
            rows = table["data"]
            first_row = rows[0]
            self.log.info(" | ".join(title for title, _ in iteritems(first_row)))
            for row in rows:
                self.log.info(" | ".join(str(value) for _, value in iteritems(row)))
