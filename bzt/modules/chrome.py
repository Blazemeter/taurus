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
import itertools
import json
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
        self.client = ChromeClient(log_path, self.log)
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


class TraceReader(object):
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


class MetricExtractor(object):
    METRIC_PAGE_LOAD_TIME = 'load-page-time'
    METRIC_FULL_LOAD_TIME = 'load-full-time'

    METRIC_MEMORY_TAB = 'memory-tab-mb'
    METRIC_MEMORY_BROWSER = 'memory-browser-mb'

    METRIC_NETWORK_FOOTPRINT = 'network-footprint-mb'
    METRIC_NETWORK_REQUESTS = 'network-http-requests'
    METRIC_NETWORK_TTFB = 'network-time-to-first-byte'

    METRIC_JS_GC_TIME = 'js-gc-time'
    METRIC_JS_HEAP_SIZE = 'js-heap-size'
    METRIC_JS_EVENT_LISTENERS = 'js-event-listeners'

    METRIC_DOM_NODES = 'dom-nodes'
    METRIC_DOM_DOCUMENTS = 'dom-documents'

    def __init__(self):
        self.tracing_start_ts = None
        self.tracing_duration = 0.0

        self.process_names = {}  # pid -> str
        self.process_labels = {}  # pid -> str
        self.memory_per_process = defaultdict(OrderedDict)  # pid -> (ts -> float)
        self.resources = defaultdict(dict)  # download_id -> dict(start_time, request_id, end_time)
        self.requests = defaultdict(dict)  # request_id -> dict(send_req_time, recv_resp_time, recv_data_time, mime,
        #                                                       status_code, size, did_fail, finish_time, network_time)
        self.page_load_times = {}
        self.js_heap_size_used = defaultdict(OrderedDict)  # pid -> (ts -> heap_size)
        self.js_event_listeners = defaultdict(OrderedDict)  # pid -> (ts -> int)
        self.gc_times = defaultdict(dict)  # pid -> dict(gc_start_time, heap_before_gc, gc_end_time, heap_after_gc)
        self.dom_documents = defaultdict(OrderedDict)  # pid -> (ts -> dom document count)
        self.dom_nodes = defaultdict(OrderedDict)  # pid -> (ts -> dom node count)

    def convert_ts(self, ts):
        if self.tracing_start_ts is not None:
            offset = float(ts) / 1000000 - self.tracing_start_ts
            rounded = round(offset, 1)
            return max(rounded, 0.0)
        else:
            return 0.0

    def feed_event(self, event):
        if event.get("timestamp") and self.tracing_start_ts is None:
            self.tracing_start_ts = event["timestamp"]
        if event.get("ts"):
            ts = self.convert_ts(event['ts'])
            self.tracing_duration = max(self.tracing_duration, ts)

        if not "cat" in event:
            return

        categories = event.get("cat").split(",")

        if "disabled-by-default-memory-infra" in categories:
            self.process_memory_event(event)
        elif "__metadata" in categories:
            self.process_metadata_event(event)
        elif "blink.net" in categories:
            self.process_network_event(event)
        elif any(c in categories for c in ["devtools.timeline",
                                           "disabled-by-default-devtools.timeline"]):
            self.process_devtools_event(event)
        elif "blink.user_timing" in categories:
            self.process_user_timing_event(event)
        # NOTE: system_stats category can also be interesting

    def process_user_timing_event(self, event):
        if event.get("name") == "loadEventStart":
            ts = self.convert_ts(event['ts'])
            self.page_load_times[event['pid']] = ts

    def process_devtools_event(self, event):
        if event.get("name") == "ResourceSendRequest":
            ts = self.convert_ts(event['ts'])
            request_id = event["args"]["data"]["requestId"]
            self.requests[request_id]["send_req_time"] = ts
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
                self.gc_times[pid]['gc_start_time'] = ts
                self.gc_times[pid]['heap_before_gc'] = float(event['args']['usedHeapSizeBefore']) / 1024 / 1024
            elif event.get("ph") == "E":  # GC end
                self.gc_times[pid]['gc_end_time'] = ts
                self.gc_times[pid]['heap_after_gc'] = float(event['args']['usedHeapSizeAfter']) / 1024 / 1024

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
        pid = event["pid"]
        if event.get("name") == "process_name":
            self.process_names[pid] = event['args']['name']
        elif event.get("name") == "process_labels":
            self.process_labels[pid] = event['args']['labels']

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

    def calc_memory_metrics(self):
        """
        Calculate memory metrics:
        - METRIC_MEMORY_TAB - memory consumption of main Chrome tab
        - METRIC_MEMORY_BROWSER - memory consumption of all Chrome processes
        :return:
        """
        # aggregate memory stats by timestamp (integer)
        # TODO: sub-second granularity
        memory_per_ts = dict()  # ts -> (pid -> [memory measurement])
        for pid in self.memory_per_process:
            for ts, value in iteritems(self.memory_per_process[pid]):
                base_ts = int(ts)
                if base_ts not in memory_per_ts:
                    memory_per_ts[base_ts] = {}
                if pid not in memory_per_ts[base_ts]:
                    memory_per_ts[base_ts][pid] = []
                memory_per_ts[base_ts][pid].append(value)

        average = lambda xs: float(sum(xs)) / len(xs)
        tab_process_pid = next(iter(self.process_labels))
        for ts in sorted(memory_per_ts):
            process_mems = memory_per_ts[ts]
            # report tab memory
            tab_memory_at_ts = average(process_mems[tab_process_pid])
            yield ts, self.METRIC_MEMORY_TAB, tab_memory_at_ts
            # report browser memory
            browser_memory_at_ts = sum(average(per_process) for _, per_process in iteritems(process_mems))
            yield ts, self.METRIC_MEMORY_BROWSER, browser_memory_at_ts

    def calc_network_metrics(self):
        """
        Calculate network metrics:
        - METRIC_NETWORK_FOOTPRINT - total download size per Chrome session
        - METRIC_NETWORK_TTFB - time when first HTTP response data arrived
        - METRIC_NETWORK_REQUESTS - number of HTTP requests made
        :return:
        """
        if self.requests:
            requests = list(req for _, req in iteritems(self.requests))
            # calculate network footprint
            total = 0.0
            for request in requests:
                payload_size = request.get("size", 0)
                total += payload_size
            total /= 1024 * 1024
            yield self.tracing_duration, self.METRIC_NETWORK_FOOTPRINT, total

            # calculate time to first byte
            first = min(requests, key=lambda r: r.get("recv_data_time", float("inf")))
            ttfb = first['recv_data_time']
            yield self.tracing_duration, self.METRIC_NETWORK_TTFB, ttfb

            # calculate requests count
            yield self.tracing_duration, self.METRIC_NETWORK_REQUESTS, len(requests)

    def calc_loading_metrics(self):
        """
        Calculate loading metrics:
        - METRIC_PAGE_LOAD_TIME - time to JS 'load' event
        - load-full-time - time to full load (when all HTTP activity is silent for 2s) (TODO)
        """
        tab_process_pid = next(iter(self.process_labels))
        yield self.tracing_duration, self.METRIC_PAGE_LOAD_TIME, self.page_load_times[tab_process_pid]

    def calc_js_metrics(self):
        """
        Calculate JS-related metrics:
        - METRIC_JS_GC_TIME - time spent doing GC
        - METRIC_JS_EVENT_LISTENERS - number of active event listeners
        - METRIC_JS_HEAP_SIZE - V8 heap size over time (TODO)
        :return:
        """
        for pid in sorted(self.js_heap_size_used):
            if pid in self.process_labels:
                for ts, value in iteritems(self.js_heap_size_used[pid]):
                    yield ts, self.METRIC_JS_HEAP_SIZE, value
        for pid in sorted(self.js_event_listeners):
            if pid in self.process_labels:
                for ts, value in iteritems(self.js_event_listeners[pid]):
                    yield ts, self.METRIC_JS_EVENT_LISTENERS, value
        if self.gc_times:
            total_gc_time = 0.0
            for pid, gc_record in iteritems(self.gc_times):
                if pid in self.process_labels:
                    if 'gc_start_time' in gc_record and 'gc_end_time' in gc_record:
                        gc_duration = gc_record['gc_end_time'] - gc_record['gc_start_time']
                        total_gc_time += gc_duration
            yield self.tracing_duration, self.METRIC_JS_GC_TIME, total_gc_time

    def calc_dom_metrics(self):
        """
        Calculate DOM-related metrics:
        - METRIC_DOM_DOCUMENTS - count of DOM nodes
        - METRIC_DOM_NODES - count of DOM documents
        :return:
        """
        for pid in self.dom_documents:
            if pid in self.process_labels:
                for ts, value in iteritems(self.dom_documents[pid]):
                    yield ts, self.METRIC_DOM_DOCUMENTS, value
        for pid in self.dom_nodes:
            if pid in self.process_labels:
                for ts, value in iteritems(self.dom_nodes[pid]):
                    yield ts, self.METRIC_DOM_NODES, value

    def get_metrics(self):
        # yields (offset, metric, value)
        # offset is number of seconds since the start of Chrome
        for metric in itertools.chain(
            self.calc_memory_metrics(),
            self.calc_network_metrics(),
            self.calc_loading_metrics(),
            self.calc_dom_metrics(),
            self.calc_js_metrics(),
        ):
            yield metric


class ChromeClient(MonitoringClient):
    def __init__(self, log_file, parent_logger):
        super(ChromeClient, self).__init__()
        self.log_file = log_file
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.reader = TraceReader(log_file, self.log)
        self.extractor = MetricExtractor()
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
        start_time = path.getctime(self.log_file)

        res = []
        for offset, metric, value in self.extractor.get_metrics():
            item = {
                "ts": start_time + offset,
                "source": "chrome",
                metric: value
            }
            res.append(item)
        self.log.info("tracing duration: %s", self.extractor.tracing_duration)
        return res

    def disconnect(self):
        pass
