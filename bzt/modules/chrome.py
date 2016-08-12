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
import bisect
import copy
import json
import re
from collections import defaultdict, OrderedDict, namedtuple
from os import path

from bzt.engine import Reporter
from bzt.modules.monitoring import Monitoring, MonitoringClient
from bzt.six import iteritems
from bzt.utils import epoch_to_str


class ChromeProfiler(Monitoring):
    def __init__(self):
        super(ChromeProfiler, self).__init__()
        self.client = None

    def prepare(self):
        trace_file = self.parameters.get('trace-file', "trace.json")
        cpuprofile_file = self.parameters.get('cpuprofile', None)
        trace_path = path.join(self.engine.artifacts_dir, trace_file)
        if cpuprofile_file:
            cpuprofile_file = path.join(self.engine.artifacts_dir, cpuprofile_file)

        self.client = ChromeClient(trace_path, cpuprofile_file, self.log)
        self.client.engine = self.engine
        self.client.connect()

    def startup(self):
        self.client.start()

    def check(self):
        results = self.client.get_data()
        if results:
            for listener in self.listeners:
                listener.monitoring_data(results)
        return False  # profiling never stops

    def shutdown(self):
        results = self.client.get_data()
        if results:
            for listener in self.listeners:
                listener.monitoring_data(results)
        self.client.disconnect()

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

    def get_js_function_call_stats(self):
        if self.client is not None:
            return self.client.get_js_function_call_stats()

    def get_metrics_tables_json(self):
        network_rows = []
        times_rows = []
        js_rows = []
        dom_rows = []
        http_rows = []
        ajax_rows = []
        js_trace_rows = []

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
            for req_time, url in sorted(ajax):
                row = OrderedDict()
                row["Start time"] = epoch_to_str(req_time)
                row["URL"] = url
                ajax_rows.append(row)

        js_stats = self.get_js_function_call_stats()
        if js_stats:
            call_stats = js_stats
            stats = [(func, calls) for func, calls in iteritems(call_stats)]
            counter = 0
            for func, stat in sorted(stats, key=lambda fs: fs[1]["total_time"], reverse=True):
                row = OrderedDict()
                row["Function"] = func.name
                row["Source"] = func.url
                row["Calls"] = stat["ncalls"]
                row["Call percentage"] = stat["perc_calls"]
                row["Total time"] = stat["total_time"]
                row["Self time"] = stat["self_time"]
                row["Self percentage"] = stat["perc_self"]
                js_trace_rows.append(row)
                counter += 1
                if counter >= 100:
                    break

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

        if js_trace_rows:
            tables.append({"id": "JSTrace", "name": "JavaScript Function Calls",
                           "description": "JavaScript function call stats",
                           "data": js_trace_rows})

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


class Metrics(object):
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

        MEMORY_TAB: "Tab memory consumption",
        MEMORY_BROWSER: "Browser memory consumption",
        MEMORY_JS_HEAP: "JS heap size",

        NETWORK_FOOTPRINT: "Network footprint",
        NETWORK_REQUESTS: "Number of HTTP requests",
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
        self.v8_complete_events = defaultdict(list)  # pid -> [(ts, name, duration)]

    def convert_ts(self, timestamp):
        if self.tracing_start_ts is not None:
            offset = float(timestamp) / 1000000 - self.tracing_start_ts
            return max(offset, 0.0)
        else:
            return 0.0

    def feed_event(self, event):
        # see https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/edit#
        # for trace event format
        if event.get("ts") and self.tracing_start_ts is None:
            self.tracing_start_ts = float(event["ts"]) / 1000000
        if event.get("ts"):
            ets = self.convert_ts(event['ts'])
            self.tracing_duration = max(self.tracing_duration, ets)

        if not "cat" in event:
            return

        categories = event.get("cat").split(",")

        if "disabled-by-default-memory-infra" in categories:
            # NOTE: system_stats category can also be interesting
            # as it dumps stats about OS memory/swap consumption and disk activity
            self.process_memory_event(event)
        if "__metadata" in categories:
            self.process_metadata_event(event)
        if "blink.net" in categories:
            self.process_network_event(event)
        if any(c in categories for c in ["devtools.timeline",
                                         "disabled-by-default-devtools.timeline",
                                         "disabled-by-default-devtools.timeline.frame"]):
            self.process_devtools_event(event)
        if "blink.user_timing" in categories:
            self.process_user_timing_event(event)
        if any(c in categories for c in ["v8", "disabled-by-default-v8.cpu_profile"]):
            self.process_v8_event(event)

    def process_v8_event(self, event):
        # record all complete events
        if event.get("ph") == "X":
            pid = event['pid']
            ets = self.convert_ts(event['ts'])
            name = event['name']
            duration = event.get('dur', 1)
            self.v8_complete_events[pid].append((ets, name, duration))

    def process_user_timing_event(self, event):
        if event.get("name") == "loadEventStart":
            pid = event['pid']
            ets = self.convert_ts(event['ts'])
            frame = event["args"]["frame"]
            self.load_events[pid][ets] = frame
        elif event.get("name") == "domContentLoadedEventStart":
            pid = event['pid']
            ets = self.convert_ts(event['ts'])
            frame = event["args"]["frame"]
            self.dom_content_loaded_events[pid][ets] = frame

    def process_devtools_event(self, event):
        if event.get("name") == "ResourceSendRequest":
            pid = event['pid']
            ets = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["send_req_time"] = ets
            self.requests[request_id]["pid"] = pid
            self.requests[request_id]["method"] = event["args"]["data"]["requestMethod"]
            self.requests[request_id]["url"] = event["args"]["data"]["url"]
        elif event.get("name") == "ResourceReceiveResponse":
            ets = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["recv_resp_time"] = ets
            self.requests[request_id]["mime"] = event["args"]["data"]["mimeType"]
            self.requests[request_id]["status_code"] = event["args"]["data"]["statusCode"]
        elif event.get("name") == "ResourceReceivedData":
            ets = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["recv_data_time"] = ets
            data_len = event["args"]["data"]["encodedDataLength"]
            size = self.requests[request_id].get("size", 0)
            self.requests[request_id]["size"] = size + data_len
        elif event.get("name") == "ResourceFinish":
            ets = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["finish_time"] = ets
            self.requests[request_id]["did_fail"] = event["args"]["data"]["didFail"]
            if event["args"]["data"].get("networkTime"):
                self.requests[request_id]["network_time"] = event["args"]["data"]["networkTime"]
        elif event.get("name") == "UpdateCounters":
            ets = self.convert_ts(event['ts'])
            pid = event["pid"]
            self.js_event_listeners[pid][ets] = event["args"]["data"]["jsEventListeners"]
            self.js_heap_size_used[pid][ets] = float(event["args"]["data"]["jsHeapSizeUsed"]) / 1024 / 1024
            self.dom_documents[pid][ets] = event["args"]["data"]["documents"]
            self.dom_nodes[pid][ets] = event["args"]["data"]["nodes"]
        elif event.get("name") in ["MajorGC", "MinorGC"]:
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
            ets = self.convert_ts(event['ts'])
            pid = event["pid"]
            event_name = event["args"]["data"]["type"]
            self.events[pid][ets] = event_name
        elif event.get("name") == "XHRLoad":
            # we can also track XHRReadyStateChange event to track all state changes
            ets = self.convert_ts(event['ts'])
            pid = event["pid"]
            url = event["args"]["data"]["url"]
            self.xhr_requests[pid].append((ets, url))
        elif event.get("name") == "TracingStartedInPage":
            pid = event["pid"]
            page_id = event["args"]["data"]["page"]
            if self.tracing_page_id is None:
                self.tracing_page_id = page_id
            if self.tracing_tab_pid is None:
                self.tracing_tab_pid = pid
        elif event.get("name") == "CommitLoad":
            pid = event["pid"]
            ets = self.convert_ts(event["ts"])
            page_id = event["args"]["data"]["page"]
            self.commit_load_events[pid][ets] = page_id
        elif event.get("name") == "CompositeLayers":
            pid = event["pid"]
            ets = self.convert_ts(event["ts"])
            self.composite_layers_events[pid].append(ets)
        elif event.get("name") == "DrawFrame":
            pid = event["pid"]
            ets = self.convert_ts(event["ts"])
            self.draw_frame_events[pid].append(ets)

    def process_network_event(self, event):
        if event.get("name") == "Resource":
            if event.get("ph") == "S":  # download starts
                ets = self.convert_ts(event['ts'])
                download_id = event["id"]
                request_id = event["args"]["data"]["requestId"]
                self.resources[download_id]["start_time"] = ets
                self.resources[download_id]["request_id"] = request_id
            elif event.get("ph") == "F":  # download finished
                ets = self.convert_ts(event['ts'])
                download_id = event["id"]
                self.resources[download_id]["end_time"] = ets

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
            ets = self.convert_ts(event['ts'])
            dumps = event['args']['dumps']
            if 'process_totals' not in dumps:
                return
            totals = dumps['process_totals']
            resident_mbytes = float(int(totals['resident_set_bytes'], 16)) / 1024 / 1024
            self.memory_per_process[pid][ets] = resident_mbytes

    @staticmethod
    def reaggregate_by_ts(per_pid_stats, aggregate_by=lambda xs: float(sum(xs)) / len(xs)):
        # TODO: sub-second granularity
        per_ts = dict()  # ts -> (pid -> [measurement at ts])
        for pid in per_pid_stats:
            for offset, value in iteritems(per_pid_stats[pid]):
                base_ts = int(offset)
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
        for offset in sorted(memory_per_ts):
            process_mems = memory_per_ts[offset]
            tab_memory_at_ts = process_mems[self.tracing_tab_pid]
            yield offset, Metrics.MEMORY_TAB, tab_memory_at_ts
            browser_memory_at_ts = sum(per_process for _, per_process in iteritems(process_mems))
            yield offset, Metrics.MEMORY_BROWSER, browser_memory_at_ts

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
        for offset in sorted(heap_per_ts):
            process_heap = heap_per_ts[offset]
            tab_heap_at_ts = process_heap[self.tracing_tab_pid]
            yield offset, Metrics.MEMORY_JS_HEAP, tab_heap_at_ts

    def calc_js_event_listeners(self):
        if self.tracing_tab_pid not in self.js_event_listeners:
            msg = ("No event listener stats were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        listeners_per_ts = self.reaggregate_by_ts(self.js_event_listeners)
        for offset in sorted(listeners_per_ts):
            listeners_at_ts = listeners_per_ts[offset]
            listeners = listeners_at_ts[self.tracing_tab_pid]
            yield offset, Metrics.JS_EVENT_LISTENERS, round(listeners)

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
        for offset in sorted(docs_per_ts):
            docs_at_ts = docs_per_ts[offset]
            docs_in_tab = docs_at_ts[self.tracing_tab_pid]
            yield offset, Metrics.DOM_DOCUMENTS, round(docs_in_tab)

    def calc_dom_nodes(self):
        if self.tracing_tab_pid not in self.dom_nodes:
            msg = ("No DOM stats were recorded for Chrome tab. "
                   "Ensure that 'devtools.timeline' category is enabled.")
            self.log.warning(msg)
            return
        nodes_per_ts = self.reaggregate_by_ts(self.dom_nodes)
        for offset in sorted(nodes_per_ts):
            nodes_at_ts = nodes_per_ts[offset]
            nodes_in_tab = nodes_at_ts[self.tracing_tab_pid]
            yield offset, Metrics.DOM_NODES, round(nodes_in_tab)

    def calc_js_cpu_utilization(self):
        if self.tracing_tab_pid not in self.v8_complete_events:
            msg = ("No JS timing events were recorded for Chrome tab. "
                   "Ensure that 'v8' category is enabled.")
            self.log.warning(msg)
            return

        v8_events = self.v8_complete_events[self.tracing_tab_pid]

        # reaggregate by 1 second
        per_ts = dict()  # ts -> (pid -> [measurement at ts])
        for ets, _, duration in v8_events:
            base_ts = int(ets)
            if base_ts not in per_ts:
                per_ts[base_ts] = []
            per_ts[base_ts].append(duration)

        # yield timeline
        for ets in sorted(per_ts):
            cpu_at_ts = float(sum(per_ts[ets])) / 1000000
            yield ets, Metrics.JS_CPU_UTILIZATION, cpu_at_ts * 100

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


class CPUProfileReader(object):
    "Reader for .cpuprofile files produced by Chrome"
    def __init__(self, profile_file, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.profile_file = profile_file
        self.profile_start_time = 0
        self.profile_end_time = 0
        self.total_hit_count = 0
        self.timestamps = []
        self.samples = []
        self.head = None
        self._id_to_node = {}
        self.max_depth = 0

    def _transform_profile_tree(self, root):
        def compute_hit_count_for_subtree(tree):
            acc = tree['hitCount']
            for child in tree["children"]:
                acc += compute_hit_count_for_subtree(child)
            return acc

        def is_native_node(node):
            return node.get("url") and node.get("url").startswith("native ")

        self.total_hit_count = compute_hit_count_for_subtree(root)
        sample_time = float(self.profile_end_time - self.profile_start_time) / self.total_hit_count

        id_map = {root['id']: root['id']}
        result_root = copy.deepcopy(root)
        result_root["self"] = result_root["hitCount"] * sample_time
        parent_node_stack = [result_root for _ in root['children']]
        source_node_stack = root['children']
        while source_node_stack:
            parent_node = parent_node_stack.pop()
            source_node = source_node_stack.pop()
            target_node = copy.deepcopy(source_node)
            target_node["self"] = target_node["hitCount"] * sample_time
            if is_native_node(source_node):
                parent_node['children'].append(target_node)
                parent_node = target_node
            else:
                parent_node["self"] += target_node["self"]
            id_map[source_node['id']] = target_node["id"]
            parent_node_stack.extend([parent_node for _ in source_node['children']])
            source_node_stack.extend(source_node['children'])
        self.samples = [id_map[sample] for sample in self.samples]
        return result_root

    def _sort_samples(self):
        timestamps = self.timestamps
        if not timestamps:
            return
        samples = self.samples
        indices = [index for index, _ in enumerate(timestamps)]
        indices.sort(key=lambda ind: timestamps[ind])
        timestamps_count = len(timestamps)
        for i in range(timestamps_count):
            index = indices[i]
            if index == i:
                continue
            saved_timestamp = timestamps[i]
            saved_sample = samples[i]
            current_index = i
            while index != i:
                samples[current_index] = samples[index]
                timestamps[current_index] = timestamps[index]
                current_index = index
                index = indices[index]
                indices[current_index] = current_index
            samples[current_index] = saved_sample
            timestamps[current_index] = saved_timestamp

    def _normalize_timestamps(self):
        timestamps = self.timestamps
        timestamps = [ts / 1000 for ts in timestamps]
        last_ts = timestamps[len(timestamps) - 1]
        avg_sample = (last_ts - timestamps[0]) / (len(timestamps) - 1)
        self.timestamps.append(last_ts + avg_sample)
        self.profile_start_time = timestamps[0]
        last_ts = timestamps[len(timestamps) - 1]
        self.profile_end_time = last_ts

    def _build_id_to_node_map(self):
        id_to_node = {}
        stack = [self.head]
        while stack:
            node = stack.pop()
            id_to_node[node["id"]] = node
            stack.extend(node["children"])
        return id_to_node

    @staticmethod
    def _lower_bound(limit, values):
        "returns minimal value `x` from `values` that is greater than `limit`"
        index = bisect.bisect_left(values, limit)
        if index > 0:
            return index - 1
        else:
            return index

    def _for_each_frame(self, open_frame_cb, close_frame_cb, start_time=0, stop_time=float("inf")):
        if not self.head or not self.samples:
            return
        samples = self.samples
        timestamps = self.timestamps
        id_to_node = self._id_to_node
        samples_count = len(self.samples)
        start_index = self._lower_bound(start_time, timestamps)
        stack_top = 0
        stack_nodes = []
        prev_id = self.head["id"]

        stack_start_times = [0.0] * (self.max_depth + 2)
        stack_children_duration = [0.0] * (self.max_depth + 2)

        for sample_index in range(start_index, samples_count, 1):
            sample_time = timestamps[sample_index]
            if sample_time >= stop_time:
                break
            sample_id = samples[sample_index]
            if sample_id == prev_id:
                continue
            node = id_to_node[sample_id]
            prev_node = id_to_node[prev_id]

            while node["depth"] > prev_node["depth"]:
                stack_nodes.append(node)
                node = node["parent"]

            while prev_node != node:
                start = stack_start_times[stack_top]
                duration = sample_time - start
                stack_children_duration[stack_top - 1] += duration
                close_frame_cb(prev_node["depth"], prev_node, start, duration,
                               duration - stack_children_duration[stack_top])
                stack_top -= 1
                if node["depth"] == prev_node["depth"]:
                    stack_nodes.append(node)
                    node = node["parent"]
                prev_node = prev_node["parent"]

            while stack_nodes:
                node = stack_nodes.pop()
                open_frame_cb(node["depth"], node, sample_time)
                stack_top += 1
                stack_start_times[stack_top] = sample_time
                stack_children_duration[stack_top] = 0.0

            prev_id = sample_id

    def _calculate_totals(self, root):
        acc = root.get("self", 0)
        for child in root["children"]:
            acc += self._calculate_totals(child)
        root["total"] = acc
        return root["total"]

    def _assign_depths_and_parents(self):
        root = self.head
        root["depth"] = -1
        root["parent"] = None
        self.max_depth = 0
        nodes_to_traverse = [root]
        while nodes_to_traverse:
            parent = nodes_to_traverse.pop()
            depth = parent["depth"] + 1
            if depth > self.max_depth:
                self.max_depth = depth
            children = parent["children"]
            for child in children:
                child["depth"] = depth
                child["parent"] = parent
                if child["children"]:
                    nodes_to_traverse.append(child)

    def _calculate_timeline(self):
        timeline = []
        stack = []
        # we can track max call depth here, it might be a useful metrics

        def on_open_frame(depth, node, sample_time):
            stack.append(len(timeline))
            timeline.append(None)

        def on_close_frame(depth, node, start_time, total_time, self_time):
            index = stack.pop()
            timeline[index] = dict(depth=depth, node=node, start_time=start_time, total_time=total_time, self_time=self_time)

        self._for_each_frame(on_open_frame, on_close_frame)

        return timeline

    def build_call_number_stats(self):
        special_funs = ["", "(idle)", "(program)", "(garbage collector)", "(root)"]
        func_entry = namedtuple("func_entry", "name, url")

        fun_stats = {}  # func_entry -> dict(ncalls, perc_time, ...)
        stack = [self.head]
        while stack:
            node = stack.pop()
            fun_key = func_entry(node['functionName'], node['url'])
            hit_count = node['hitCount'] or 1
            if fun_key not in fun_stats:
                fun_stats[fun_key] = {"ncalls": 0}
            fun_stats[fun_key]["ncalls"] += hit_count
            stack.extend(node['children'])

        stats = {}
        for func, stat in iteritems(fun_stats):
            # skip special functions
            if func.name in special_funs:
                continue
            stat["perc_calls"] = "%.2f%%" % (float(stat["ncalls"]) / self.total_hit_count * 100)
            stats[func] = stat

        return stats

    def build_call_time_stats(self):
        special_funs = ["", "(idle)", "(program)", "(garbage collector)", "(root)"]
        func_entry = namedtuple("func_entry", "name, url")
        timeline = self._calculate_timeline()

        stats = {}

        for item in timeline:
            if item is None:
                continue
            node = item["node"]
            func = func_entry(node["functionName"], node["url"])
            if func.name is special_funs:
                continue
            self_time = item["self_time"]
            total_time = item["total_time"]
            if func not in stats:
                stats[func] = {"self_time": 0.0, "total_time": 0.0}
            stats[func]["self_time"] += self_time
            stats[func]["total_time"] += total_time

        return stats

    def process_cpuprofile(self):
        with open(self.profile_file) as fds:
            profile = json.load(fds)
        head = profile["head"]
        self.profile_start_time = profile["startTime"]
        self.profile_end_time = profile["endTime"]
        self.timestamps = profile["timestamps"]
        self.samples = profile["samples"]
        self.head = self._transform_profile_tree(head)
        self._calculate_totals(self.head)
        self._assign_depths_and_parents()
        if self.samples:
            self._id_to_node = self._build_id_to_node_map()
            self._sort_samples()
            self._normalize_timestamps()

    def extract_js_call_stats(self):
        if not path.exists(self.profile_file):
            self.log.warning("CPU profile file doesn't exist, can't extract call stats")
            return None
        self.process_cpuprofile()
        call_number_stats = self.build_call_number_stats()
        call_time_stats = self.build_call_time_stats()
        stats = {}
        for func in call_number_stats:
            if func not in call_time_stats:
                continue
            num_stat = call_number_stats[func]
            time_stat = call_time_stats[func]
            self_time = time_stat["self_time"] / 1000000
            total_time = time_stat["total_time"] / 1000000
            perc_self = "%.2f%%" % (self_time / total_time * 100)
            stats[func] = {"ncalls": num_stat["ncalls"], "perc_calls": num_stat["perc_calls"],
                           "self_time": self_time, "total_time": total_time, "perc_self": perc_self}

        return stats


class ChromeClient(MonitoringClient):
    """
    Chrome performance client

    :type extractor: MetricExtractor
    :type reader: TraceJSONReader
    :type cpuprofile_reader: CPUProfileReader
    :type engine: bzt.Engine
    """
    def __init__(self, trace_file, cpuprofile_file, parent_logger):
        super(ChromeClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.trace_file = trace_file
        self.cpuprofile_file = cpuprofile_file
        self.trace_file_ctime = None
        self.reader = TraceJSONReader(trace_file, self.log)
        self.extractor = MetricExtractor(self.log)
        if cpuprofile_file:
            self.cpuprofile_reader = CPUProfileReader(cpuprofile_file, self.log)
        else:
            self.cpuprofile_reader = None
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

    def get_js_function_call_stats(self):
        if self.cpuprofile_reader is not None:
            return self.cpuprofile_reader.extract_js_call_stats()

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
                for metric in sorted(metrics):
                    label = Metrics.metric_label(metric)
                    self.log.info("%s = %s", label, metrics[metric])

        requests = self.chrome_profiler.get_requests_stats()
        if requests:
            self.log.info("")
            self.log.info("HTTP requests:")
            for index, req in enumerate(sorted(requests, key=lambda r: r.get("send_req_time")), 1):
                start_time = req.get("send_req_time")
                method = req.get("method")
                url = req.get("url")
                status = req.get("status_code")
                if all([start_time, method, url, status]):
                    if len(url) > 50:
                        url = url[:50] + "..."
                    self.log.info("%s. %s: %s %s - %s", index, epoch_to_str(start_time), method, url, status)

        ajax_requests = self.chrome_profiler.get_ajax_stats()
        if ajax_requests:
            self.log.info("")
            self.log.info("AJAX requests:")
            for index, req in enumerate(sorted(ajax_requests), 1):
                timestamp, url = req
                if len(url) > 60:
                    url = url[:60] + "..."
                self.log.info("%s. %s: %s", index, epoch_to_str(timestamp), url)

        js_stats = self.chrome_profiler.get_js_function_call_stats()
        if js_stats:
            call_stats = js_stats
            self.log.info("")
            self.log.info("Longest JS function calls (first 20):")
            stats = [(func, calls) for func, calls in iteritems(call_stats)]
            for index, funcstat in enumerate(sorted(stats, key=lambda fs: fs[1]["total_time"], reverse=True), 1):
                func, stat = funcstat
                tmpl = "%s. %s (%s) : ncalls=%s, perc_calls=%s, total_time=%s, self_time=%s, perc_self=%s"
                url = func.url
                if len(url) > 60:
                    url = url[:50] + "..." + url[-7:]
                self.log.info(tmpl, index, func.name, url, stat["ncalls"], stat["perc_calls"],
                              stat["total_time"], stat["self_time"], stat["perc_self"])
                if index >= 20:
                    break
