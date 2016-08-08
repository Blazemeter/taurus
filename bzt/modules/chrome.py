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
import copy
import json
import re
from collections import defaultdict, OrderedDict
from os import path

from bzt.engine import Reporter
from bzt.modules.monitoring import Monitoring, MonitoringClient, MonitoringListener
from bzt.six import iteritems
from bzt.utils import epoch_to_str


class ChromeProfiler(Monitoring):
    def __init__(self):
        super(ChromeProfiler, self).__init__()
        self.client = None

    def prepare(self):
        trace_file = self.parameters.get('trace-file', "trace.json")
        trace_path = path.join(self.engine.artifacts_dir, trace_file)

        self.client = ChromeClient(trace_path, self.log)
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

    def get_aggr_metrics(self):
        if self.client is not None:
            return self.client.get_aggr_metrics()

    def get_requests_stats(self):
        if self.client is not None:
            return self.client.get_requests_stats()

    def get_ajax_stats(self):
        if self.client is not None:
            return self.client.get_ajax_stats()

    def get_tab_label(self):
        if self.client is not None:
            return self.client.get_tab_label()

    def get_metrics_tables_json(self):
        network_rows = []
        times_rows = []
        js_rows = []
        dom_rows = []
        http_rows = []
        ajax_rows = []

        metrics = self.get_aggr_metrics()
        for metric, value in iteritems(metrics):
            row = OrderedDict()
            row["Label"] = Metrics.metric_label(metric)
            row["Value"] = value
            if Metrics.is_network_metric(metric):
                network_rows.append(row)
            elif Metrics.is_time_metric(metric):
                times_rows.append(row)
            elif Metrics.is_dom_metric(metric):
                dom_rows.append(row)
            elif Metrics.is_js_metric(metric):
                js_rows.append(row)

        requests = self.get_requests_stats()
        if requests:
            for req in sorted(requests, key=lambda r: r.get("send_req_time")):
                # [dict(send_req_time, method, url, recv_resp_time,
                #      recv_data_time, mime, status_code, size, did_fail,
                #      finish_time, network_time, pid)]
                row = OrderedDict()
                row["Start time"] = req.get("send_req_time")
                row["End time"] = req.get("finish_time")
                row["Method"] = req.get("method")
                row["URL"] = req.get("url")
                row["HTTP Status"] = req.get("status_code", "unknown")
                row["MIME"] = req.get("mime", "unknown")
                row["Size"] = req.get("size")
                if not any(value is None for _, value in iteritems(row)):
                    row["Start time"] = epoch_to_str(row["Start time"])
                    row["End time"] = epoch_to_str(row["End time"])
                    http_rows.append(row)

        ajax = self.get_ajax_stats()
        if ajax:
            for ts, url in sorted(ajax):
                row = OrderedDict()
                row["Start time"] = epoch_to_str(ts)
                row["URL"] = url
                ajax_rows.append(row)

        tables = []
        if times_rows:
            tables.append({"id": "Time", "name": "Load Time Metrics",
                           "description": "Page load performance metrics.",
                           "data": sorted(times_rows, key=lambda x: x['Label'])})
        if network_rows:
            tables.append({"id": "Network", "name": "Network Metrics",
                           "description": "Network performance metrics",
                           "data": sorted(network_rows, key=lambda x: x['Label'])})
        if js_rows:
            tables.append({"id": "JS", "name": "JS Metrics",
                           "description": "JavaScript performance metrics",
                           "data": sorted(js_rows, key=lambda x: x['Label'])})
        if dom_rows:
            tables.append({"id": "DOM", "name": "DOM Metrics",
                           "description": "DOM performance metrics",
                           "data": sorted(dom_rows, key=lambda x: x['Label'])})
        if http_rows:
            tables.append({"id": "HTTP", "name": "HTTP Requests",
                           "description": "HTTP requests made by page",
                           "data": http_rows})
        if ajax_rows:
            tables.append({"id": "AJAX", "name": "AJAX Requests",
                           "description": "AJAX requests made by page",
                           "data": ajax_rows})

        return {
            "tables": tables
        }


class TraceJSONReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None

    def extract_events(self):
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
        self.fds = open(self.filename, 'r')
        return True

    def __del__(self):
        if self.fds is not None:
            self.fds.close()


class ChromePerfLogReader(object):
    TRACING_EVENT_RE = re.compile(r"^\[(\d+\.\d+)\]\[(\w+)\]: DEVTOOLS EVENT Tracing.dataCollected \{$")

    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.data_buffer = ""

    def extract_events(self):
        if not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        lines = self.fds.readlines()
        for line in lines:
            if self.TRACING_EVENT_RE.match(line):
                self.data_buffer = "{"
                continue

            if self.data_buffer:
                self.data_buffer += line

            try:
                trace_events = json.loads(self.data_buffer)
                for event in trace_events["value"]:
                    if isinstance(event, dict):
                        yield event
                self.data_buffer = ""
            except ValueError:
                continue

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


class Metrics:
    PAGE_LOAD_TIME = 'load-page-time'
    DOM_CONTENT_LOADED_TIME = 'dom-content-loaded-time'
    FULL_LOAD_TIME = 'load-full-time'
    FIRST_PAINT_TIME = 'first-paint-time'

    MEMORY_TAB = 'memory-tab-mb'
    MEMORY_BROWSER = 'memory-browser-mb'
    MEMORY_JS_HEAP = 'memory-js-heap'

    NETWORK_FOOTPRINT = 'network-footprint-mb'
    NETWORK_REQUESTS = 'network-http-requests'
    NETWORK_XHR_REQUESTS = 'network-xhr-requests'
    NETWORK_TTFB = 'network-time-to-first-byte'

    JS_GC_TIME = 'js-gc-time'
    JS_EVENT_LISTENERS = 'js-event-listeners'
    JS_CPU_UTILIZATION = 'js-cpu-utilization'

    DOM_NODES = 'dom-nodes'
    DOM_DOCUMENTS = 'dom-documents'

    METRIC_LABELS = {
        PAGE_LOAD_TIME: "Time to page load",
        DOM_CONTENT_LOADED_TIME: "Time to DOMContentLoad event",
        FULL_LOAD_TIME: "Time to full page load",
        FIRST_PAINT_TIME: "Time to first paint",

        MEMORY_TAB: "Memory consumption of a tab",
        MEMORY_BROWSER: "Memory consumption of a browser",
        MEMORY_JS_HEAP: "Memory allocated by JS engine",

        NETWORK_FOOTPRINT: "Network footprint of a page",
        NETWORK_REQUESTS: "Number of HTTP requests (including AJAX)",
        NETWORK_XHR_REQUESTS: "Number of AJAX requests",
        NETWORK_TTFB: "Time to first byte",

        JS_GC_TIME: "Time spent doing GC in JS engine",
        JS_CPU_UTILIZATION: "JavaScript CPU utilization",
        JS_EVENT_LISTENERS: "Number of DOM event listeners",

        DOM_NODES: "Number of DOM nodes",
        DOM_DOCUMENTS: "Number of DOM documents",
    }

    @classmethod
    def is_network_metric(cls, metric):
        return metric in (cls.NETWORK_FOOTPRINT, cls.NETWORK_REQUESTS, cls.NETWORK_XHR_REQUESTS, cls.NETWORK_TTFB)

    @classmethod
    def is_time_metric(cls, metric):
        return metric in (cls.PAGE_LOAD_TIME, cls.FULL_LOAD_TIME, cls.FIRST_PAINT_TIME, cls.DOM_CONTENT_LOADED_TIME)

    @classmethod
    def is_memory_metric(cls, metric):
        return metric in (cls.MEMORY_TAB, cls.MEMORY_BROWSER, cls.MEMORY_JS_HEAP)

    @classmethod
    def is_js_metric(cls, metric):
        return metric in (cls.JS_GC_TIME, cls.JS_EVENT_LISTENERS, cls.JS_CPU_UTILIZATION)

    @classmethod
    def is_dom_metric(cls, metric):
        return metric in (cls.DOM_DOCUMENTS, cls.DOM_NODES)

    @classmethod
    def metric_label(cls, metric):
        return cls.METRIC_LABELS.get(metric, metric)


class MetricExtractor(object):
    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.start_time = None
        self.tracing_start_ts = None
        self.tracing_duration = 0.0

        self.tracing_page_id = None
        self.tracing_tab_pid = None
        self.commit_load_events = defaultdict(dict)  # pid -> (ts -> page id)
        self.composite_layers_events = defaultdict(list)  # pid -> [ts]
        self.draw_frame_events = defaultdict(list)  # pid -> [ts]
        self.load_events = defaultdict(dict)  # pid -> (ts -> frame id)
        self.dom_content_loaded_events = defaultdict(dict)  # pid -> (ts -> frame id)

        self.process_names = {}  # pid -> str
        self.process_labels = {}  # pid -> str
        self.memory_per_process = defaultdict(OrderedDict)  # pid -> (ts -> float)
        self.resources = defaultdict(dict)  # download_id -> dict(start_time, request_id, end_time)
        self.requests = defaultdict(dict)  # request_id -> dict(pid, send_req_time, method, url, recv_resp_time,
        #                                                       recv_data_time, mime, status_code, size, did_fail,
        #                                                       finish_time, network_time)
        self.js_heap_size_used = defaultdict(OrderedDict)  # pid -> (ts -> heap_size)
        self.js_event_listeners = defaultdict(OrderedDict)  # pid -> (ts -> int)
        self.gc_times = defaultdict(list)  # pid -> [dict(gc_start_time, heap_before_gc, gc_end_time, heap_after_gc)]
        self.dom_documents = defaultdict(OrderedDict)  # pid -> (ts -> dom document count)
        self.dom_nodes = defaultdict(OrderedDict)  # pid -> (ts -> dom node count)
        self.events = defaultdict(dict)  # pid -> (ts -> event_name)
        self.xhr_requests = defaultdict(list)  # pid -> [(ts, url)]
        self.complete_events = defaultdict(list)  # pid -> [(ts, name, duration)]

    def convert_ts(self, ts):
        if self.tracing_start_ts is not None:
            offset = float(ts) / 1000000 - self.tracing_start_ts
            return max(offset, 0.0)
        else:
            return 0.0

    def feed_event(self, event):
        # see https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/edit#
        # for trace event format
        if event.get("ts") and self.tracing_start_ts is None:
            self.tracing_start_ts = float(event["ts"]) / 1000000
        if event.get("ts"):
            ts = self.convert_ts(event['ts'])
            self.tracing_duration = max(self.tracing_duration, ts)

        if not "cat" in event:
            return

        categories = event.get("cat").split(",")

        if "disabled-by-default-memory-infra" in categories:
            # NOTE: system_stats category can also be interesting
            # as it dumps stats about OS memory/swap consumption and disk activity
            self.process_memory_event(event)
        elif "__metadata" in categories:
            self.process_metadata_event(event)
        elif "blink.net" in categories:
            self.process_network_event(event)
        elif any(c in categories for c in ["devtools.timeline",
                                           "disabled-by-default-devtools.timeline",
                                           "disabled-by-default-devtools.timeline.frame"]):
            self.process_devtools_event(event)
        elif "blink.user_timing" in categories:
            self.process_user_timing_event(event)

        if "v8" in categories:
            self.process_v8_event(event)

    def process_v8_event(self, event):
        if event.get("ph") == "X":
            pid = event['pid']
            ts = self.convert_ts(event['ts'])
            name = event['name']
            duration = event.get('dur', 1)
            self.complete_events[pid].append((ts, name, duration))

    def process_user_timing_event(self, event):
        if event.get("name") == "loadEventStart":
            pid = event['pid']
            ts = self.convert_ts(event['ts'])
            frame = event["args"]["frame"]
            self.load_events[pid][ts] = frame
        elif event.get("name") == "domContentLoadedEventStart":
            pid = event['pid']
            ts = self.convert_ts(event['ts'])
            frame = event["args"]["frame"]
            self.dom_content_loaded_events[pid][ts] = frame

    def process_devtools_event(self, event):
        if event.get("name") == "ResourceSendRequest":
            pid = event['pid']
            ts = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["send_req_time"] = ts
            self.requests[request_id]["pid"] = pid
            self.requests[request_id]["method"] = event["args"]["data"]["requestMethod"]
            self.requests[request_id]["url"] = event["args"]["data"]["url"]
        elif event.get("name") == "ResourceReceiveResponse":
            ts = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["recv_resp_time"] = ts
            self.requests[request_id]["mime"] = event["args"]["data"]["mimeType"]
            self.requests[request_id]["status_code"] = event["args"]["data"]["statusCode"]
        elif event.get("name") == "ResourceReceivedData":
            ts = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["recv_data_time"] = ts
            data_len = event["args"]["data"]["encodedDataLength"]
            size = self.requests[request_id].get("size", 0)
            self.requests[request_id]["size"] = size + data_len
        elif event.get("name") == "ResourceFinish":
            ts = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["finish_time"] = ts
            self.requests[request_id]["did_fail"] = event["args"]["data"]["didFail"]
            if event["args"]["data"].get("networkTime"):
                self.requests[request_id]["network_time"] = event["args"]["data"]["networkTime"]
        elif event.get("name") == "UpdateCounters":
            ts = self.convert_ts(event['ts'])
            pid = event["pid"]
            self.js_event_listeners[pid][ts] = event["args"]["data"]["jsEventListeners"]
            self.js_heap_size_used[pid][ts] = float(event["args"]["data"]["jsHeapSizeUsed"]) / 1024 / 1024
            self.dom_documents[pid][ts] = event["args"]["data"]["documents"]
            self.dom_nodes[pid][ts] = event["args"]["data"]["nodes"]
        elif event.get("name") in ["MajorGC", "MinorGC"]:
            ts = self.convert_ts(event['ts'])
            pid = event["pid"]
            if event.get("ph") == "B":  # GC begin
                item = {'gc_start_time': event['ts'],
                        'heap_before_gc': float(event['args']['usedHeapSizeBefore']) / 1024 / 1024}
                self.gc_times[pid].append(item)
            elif event.get("ph") == "E":  # GC end
                if self.gc_times[pid]:
                    self.gc_times[pid][-1]['gc_end_time'] = event['ts']
                    self.gc_times[pid][-1]['heap_after_gc'] = float(event['args']['usedHeapSizeAfter']) / 1024 / 1024
        elif event.get("name") == "EventDispatch":
            ts = self.convert_ts(event['ts'])
            pid = event["pid"]
            event_name = event["args"]["data"]["type"]
            self.events[pid][ts] = event_name
        elif event.get("name") == "XHRLoad":
            # we can also track XHRReadyStateChange event to track all state changes
            ts = self.convert_ts(event['ts'])
            pid = event["pid"]
            url = event["args"]["data"]["url"]
            self.xhr_requests[pid].append((ts, url))
        elif event.get("name") == "TracingStartedInPage":
            pid = event["pid"]
            page_id = event["args"]["data"]["page"]
            if self.tracing_page_id is None:
                self.tracing_page_id = page_id
            if self.tracing_tab_pid is None:
                self.tracing_tab_pid = pid
        elif event.get("name") == "CommitLoad":
            pid = event["pid"]
            ts = self.convert_ts(event["ts"])
            page_id = event["args"]["data"]["page"]
            self.commit_load_events[pid][ts] = page_id
        elif event.get("name") == "CompositeLayers":
            pid = event["pid"]
            ts = self.convert_ts(event["ts"])
            self.composite_layers_events[pid].append(ts)
        elif event.get("name") == "DrawFrame":
            pid = event["pid"]
            ts = self.convert_ts(event["ts"])
            self.draw_frame_events[pid].append(ts)

    def process_network_event(self, event):
        if event.get("name") == "Resource":
            if event.get("ph") == "S":  # download starts
                ts = self.convert_ts(event['ts'])
                download_id = event["id"]
                request_id = event["args"]["data"]["requestId"]
                self.resources[download_id]["start_time"] = ts
                self.resources[download_id]["request_id"] = request_id
            elif event.get("ph") == "F":  # download finished
                ts = self.convert_ts(event['ts'])
                download_id = event["id"]
                self.resources[download_id]["end_time"] = ts

    def process_metadata_event(self, event):
        if event.get("name") == "process_name":
            pid = event["pid"]
            self.process_names[pid] = event['args']['name']
        elif event.get("name") == "process_labels":
            pid = event["pid"]
            self.process_labels[pid] = event['args']['labels']
        elif event.get("name") == "start_time":
            self.start_time = event['args']['timestamp']

    def process_memory_event(self, event):
        if event.get("name") == "periodic_interval":
            pid = event["pid"]
            ts = self.convert_ts(event['ts'])
            dumps = event['args']['dumps']
            if 'process_totals' not in dumps:
                return
            totals = dumps['process_totals']
            resident_mbytes = float(int(totals['resident_set_bytes'], 16)) / 1024 / 1024
            self.memory_per_process[pid][ts] = resident_mbytes

    @staticmethod
    def reaggregate_by_ts(per_pid_stats, aggregate_by=lambda xs: float(sum(xs)) / len(xs)):
        # TODO: sub-second granularity
        per_ts = dict()  # ts -> (pid -> [measurement at ts])
        for pid in per_pid_stats:
            for ts, value in iteritems(per_pid_stats[pid]):
                base_ts = int(ts)
                if base_ts not in per_ts:
                    per_ts[base_ts] = {}
                if pid not in per_ts[base_ts]:
                    per_ts[base_ts][pid] = []
                per_ts[base_ts][pid].append(value)
        return {
            ts: {
                pid: aggregate_by(pid_measurements)
                for pid, pid_measurements in iteritems(stats_per_ts)
            }
            for ts, stats_per_ts in iteritems(per_ts)
        }

    def calc_memory_stats(self):
        if self.tracing_tab_pid not in self.memory_per_process:
            msg = ("Can't extract memory stats for Chrome tab. "
                   "Ensure that 'disabled-by-default-memory-infra' category is enabled.")
            self.log.warning(msg)
            return
        memory_per_ts = self.reaggregate_by_ts(self.memory_per_process)  # ts -> (pid -> memory)
        for ts in sorted(memory_per_ts):
            process_mems = memory_per_ts[ts]
            tab_memory_at_ts = process_mems[self.tracing_tab_pid]
            yield ts, Metrics.MEMORY_TAB, tab_memory_at_ts
            browser_memory_at_ts = sum(per_process for _, per_process in iteritems(process_mems))
            yield ts, Metrics.MEMORY_BROWSER, browser_memory_at_ts

    def aggr_network_footprint(self):
        tab_requests = list(req for _, req in iteritems(self.requests) if req.get("pid") == self.tracing_tab_pid)
        total = 0.0
        for request in tab_requests:
            payload_size = request.get("size", 0)
            total += payload_size
        total /= 1024 * 1024
        yield Metrics.NETWORK_FOOTPRINT, total

    def aggr_time_to_first_byte(self):
        tab_requests = list(req for _, req in iteritems(self.requests) if req.get("pid") == self.tracing_tab_pid)
        if not tab_requests:
            msg = ("No requests were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        first = min(tab_requests, key=lambda r: r.get("recv_data_time", float("inf")))
        ttfb = first['recv_data_time']
        yield Metrics.NETWORK_TTFB, ttfb

    def aggr_network_requests(self):
        tab_requests = list(req for _, req in iteritems(self.requests) if req.get("pid") == self.tracing_tab_pid)
        yield Metrics.NETWORK_REQUESTS, len(tab_requests)

    def aggr_xhr_requests(self):
        tab_xhr_requests = self.xhr_requests.get(self.tracing_tab_pid, [])
        yield Metrics.NETWORK_XHR_REQUESTS, len(tab_xhr_requests)

    def aggr_page_load_time(self):
        if self.tracing_tab_pid not in self.load_events:
            msg = ("No 'load' events were recorded for Chrome tab. "
                   "Ensure that 'blink.user_timing' category is enabled.")
            self.log.warning(msg)
            return
        if not self.tracing_page_id:
            msg = ("No 'load' events were recorded for Chrome tab. "
                   "Ensure that 'blink.user_timing' category is enabled.")
            self.log.warning(msg)
            return

        page_load = min(ts
                        for ts, frame_id in iteritems(self.load_events[self.tracing_tab_pid])
                        if frame_id == self.tracing_page_id)
        yield Metrics.PAGE_LOAD_TIME, page_load

    def aggr_dom_content_loaded_time(self):
        if self.tracing_tab_pid not in self.load_events:
            msg = ("No 'DOMContentLoad' events were recorded for Chrome tab. "
                   "Ensure that 'blink.user_timing' category is enabled.")
            self.log.warning(msg)
            return
        if not self.tracing_page_id:
            msg = ("No 'load' events were recorded for Chrome tab. "
                   "Ensure that 'blink.user_timing' category is enabled.")
            self.log.warning(msg)
            return

        dom_load = max(ts
                       for ts, frame_id in iteritems(self.dom_content_loaded_events[self.tracing_tab_pid])
                       if frame_id == self.tracing_page_id)
        yield Metrics.DOM_CONTENT_LOADED_TIME, dom_load

    def aggr_full_load_time(self):
        tab_requests = list(req for _, req in iteritems(self.requests) if req.get("pid") == self.tracing_tab_pid)
        if not tab_requests:
            msg = ("No requests were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        last = max(tab_requests, key=lambda r: r.get("finish_time", float("-inf")))
        last_request_time = last['finish_time']
        yield Metrics.FULL_LOAD_TIME, last_request_time

    def aggr_first_paint_time(self):
        if not self.tracing_page_id:
            msg = ("No paint events were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return

        commit_loads = [
            offset
            for offset, page_id in iteritems(self.commit_load_events[self.tracing_tab_pid])
            if page_id == self.tracing_page_id
        ]
        last_commit_load = max(commit_loads)
        next_composite_layers = None
        for composite_ts in sorted(self.composite_layers_events[self.tracing_tab_pid]):
            if composite_ts >= last_commit_load:
                next_composite_layers = composite_ts
                break
        if next_composite_layers:
            next_draw_frame = None
            for draw_frame_ts in sorted(self.draw_frame_events[self.tracing_tab_pid]):
                if draw_frame_ts >= next_composite_layers:
                    next_draw_frame = draw_frame_ts
                    break
            if next_draw_frame:
                yield Metrics.FIRST_PAINT_TIME, next_draw_frame

    def calc_js_heap_size(self):
        if self.tracing_tab_pid not in self.js_heap_size_used:
            msg = ("No heap size data was recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        heap_per_ts = self.reaggregate_by_ts(self.js_heap_size_used)
        for ts in sorted(heap_per_ts):
            process_heap = heap_per_ts[ts]
            tab_heap_at_ts = process_heap[self.tracing_tab_pid]
            yield ts, Metrics.MEMORY_JS_HEAP, tab_heap_at_ts

    def calc_js_event_listeners(self):
        if self.tracing_tab_pid not in self.js_event_listeners:
            msg = ("No event listener stats were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        listeners_per_ts = self.reaggregate_by_ts(self.js_event_listeners)
        for ts in sorted(listeners_per_ts):
            listeners_at_ts = listeners_per_ts[ts]
            listeners = listeners_at_ts[self.tracing_tab_pid]
            yield ts, Metrics.JS_EVENT_LISTENERS, round(listeners)

    def aggr_gc_time(self):
        if self.tracing_tab_pid not in self.gc_times:
            msg = ("No GC events were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        total_gc_time = 0.0
        gcs = self.gc_times[self.tracing_tab_pid]
        for gc_record in gcs:
            if 'gc_start_time' in gc_record and 'gc_end_time' in gc_record:
                gc_duration = float(gc_record['gc_end_time'] - gc_record['gc_start_time']) / 1000000
                total_gc_time += gc_duration
        yield Metrics.JS_GC_TIME, total_gc_time

    def calc_dom_documents(self):
        if self.tracing_tab_pid not in self.dom_documents:
            msg = ("No DOM stats were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        docs_per_ts = self.reaggregate_by_ts(self.dom_documents)
        for ts in sorted(docs_per_ts):
            docs_at_ts = docs_per_ts[ts]
            docs_in_tab = docs_at_ts[self.tracing_tab_pid]
            yield ts, Metrics.DOM_DOCUMENTS, round(docs_in_tab)

    def calc_dom_nodes(self):
        if self.tracing_tab_pid not in self.dom_nodes:
            msg = ("No DOM stats were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        nodes_per_ts = self.reaggregate_by_ts(self.dom_nodes)
        for ts in sorted(nodes_per_ts):
            nodes_at_ts = nodes_per_ts[ts]
            nodes_in_tab = nodes_at_ts[self.tracing_tab_pid]
            yield ts, Metrics.DOM_NODES, round(nodes_in_tab)

    def calc_js_cpu_utilization(self):
        if self.tracing_tab_pid not in self.complete_events:
            msg = ("No JS timing events were recorded for Chrome tab. "
                   "Ensure that 'v8' category is enabled.")
            self.log.warning(msg)
            return

        v8_events = self.complete_events[self.tracing_tab_pid]

        # reaggregate by 1 second
        per_ts = dict()  # ts -> (pid -> [measurement at ts])
        for ts, _, duration in v8_events:
            base_ts = int(ts)
            if base_ts not in per_ts:
                per_ts[base_ts] = []
            per_ts[base_ts].append(duration)

        # yield timeline
        for ts in sorted(per_ts):
            cpu_at_ts = float(sum(per_ts[ts])) / 1000000
            yield ts, Metrics.JS_CPU_UTILIZATION, cpu_at_ts * 100

    def get_metrics(self):
        # yields (offset, metric, value) for all metrics that are defined in time
        # offset is number of seconds since the start of Chrome
        if self.tracing_tab_pid is None:
            self.log.warning("No tracing data recorded for Chrome tab.")
            return
        for prop in dir(self):
            if prop.startswith('calc_'):
                calc_method = getattr(self, prop)
                for metric in calc_method():
                    yield metric

    def get_aggr_metrics(self):
        # yields (metric, value) for metrics that are reported one time
        if self.tracing_tab_pid is None:
            self.log.warning("No tracing data recorded for Chrome tab.")
            return
        for prop in dir(self):
            if prop.startswith('aggr_'):
                aggr_method = getattr(self, prop)
                for metric in aggr_method():
                    yield metric

    def get_requests_stats(self):
        return [req for _, req in iteritems(self.requests) if req.get("pid") == self.tracing_tab_pid]

    def get_ajax_stats(self):
        return self.xhr_requests.get(self.tracing_tab_pid, [])

    def get_tab_label(self):
        return self.process_labels.get(self.tracing_tab_pid, self.tracing_tab_pid)


class ChromeClient(MonitoringClient):
    """
    Chrome performance client

    :type extractor: MetricExtractor
    :type reader: TraceJSONReader
    :type engine: bzt.Engine
    """
    def __init__(self, chrome_trace, parent_logger):
        super(ChromeClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.trace_file = chrome_trace
        self.trace_file_ctime = None
        self.reader = TraceJSONReader(chrome_trace, self.log)
        self.extractor = MetricExtractor(self.log)
        self.engine = None
        self.metric_cache = dict()

    def connect(self):
        pass

    def start(self):
        pass

    def get_data(self):
        if not path.exists(self.trace_file):
            return []

        ctime = path.getctime(self.trace_file)
        if self.trace_file_ctime is None:
            self.log.debug("Recording trace file ctime %s", epoch_to_str(ctime))
            self.trace_file_ctime = ctime
        elif self.trace_file_ctime != ctime and abs(self.trace_file_ctime - ctime) >= 2.0:
            self.log.debug("Trace file ctime updated, resetting reader and extractor, %s", epoch_to_str(ctime))
            self.trace_file_ctime = ctime
            self.reader = TraceJSONReader(self.trace_file, self.log)
            self.extractor = MetricExtractor(self.log)

        for event in self.reader.extract_events():
            self.extractor.feed_event(event)

        if self.extractor.start_time is None:
            return []

        start_time = self.extractor.start_time

        datapoints = []
        for offset, metric, value in self.extractor.get_metrics():
            self.metric_cache[metric] = value
            item = {
                "ts": start_time + offset,
                "source": "chrome",
                metric: value
            }
            datapoints.append(item)

        return datapoints

    def get_aggr_metrics(self):
        res = {}
        for metric, value in iteritems(self.metric_cache):
            res[metric] = value
        for metric, value in self.extractor.get_aggr_metrics():
            res[metric] = value
        return res

    def get_requests_stats(self):
        if self.extractor.start_time is None:
            return []
        start_time = self.extractor.start_time
        res = []
        for request in self.extractor.get_requests_stats():
            rcopy = copy.copy(request)
            offset_fields = ["send_req_time", "recv_resp_time", "recv_data_time", "finish_time"]
            for field in offset_fields:
                if field in rcopy:
                    rcopy[field] = start_time + rcopy[field]
            res.append(rcopy)
        return res

    def get_ajax_stats(self):
        if self.extractor.start_time is None:
            return []
        start_time = self.extractor.start_time
        return [(start_time + ts, url)
                for ts, url in self.extractor.get_ajax_stats()]

    def get_tab_label(self):
        return self.extractor.get_tab_label()

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
            for metric in sorted(metrics):
                label = Metrics.metric_label(metric)
                self.log.info("%s = %s", label, metrics[metric])

        requests = self.chrome_profiler.get_requests_stats()
        if requests:
            self.log.info("HTTP requests:")
            for req in sorted(requests, key=lambda r: r.get("send_req_time")):
                start_time = req.get("send_req_time")
                method = req.get("method")
                url = req.get("url")
                status = req.get("status_code")
                if all([start_time, method, url, status]):
                    if len(url) > 50:
                        url = url[:50] + "..."
                    self.log.info("%s: %s %s - %s", epoch_to_str(start_time), method, url, status)

        ajax_requests = self.chrome_profiler.get_ajax_stats()
        if ajax_requests:
            self.log.info("AJAX requests:")
            for ts, url in sorted(ajax_requests):
                if len(url) > 60:
                    url = url[:60] + "..."
                self.log.info("%s: %s", epoch_to_str(ts), url)
