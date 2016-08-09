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
import csv
import json
import logging
import re
import subprocess
from collections import defaultdict, OrderedDict
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


class SplayTreeNode(object):
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.left = None
        self.right = None


class SplayTree(object):
    def __init__(self):
        self.root = None

    def is_empty(self):
        return not self.root

    def insert(self, key, value):
        if self.is_empty():
            self.root = SplayTreeNode(key, value)
            return
        self.splay(key)
        if self.root.key == key:
            return
        node = SplayTreeNode(key, value)
        if key > self.root.key:
            node.left = self.root
            node.right = self.root.right
            self.root.right = None
        else:
            node.right = self.root
            node.left = self.root.left
            self.root.left = None
        self.root = node

    def remove(self, key):
        if self.is_empty():
            return
        self.splay(key)
        if self.root.key != key:
            return
        removed = self.root
        if not self.root.left:
            self.root = self.root.right
        else:
            right = self.root.right
            self.root = self.root.left
            self.splay(key)
            self.root.right = right
        return removed

    def find(self, key):
        if self.is_empty():
            return None
        self.splay(key)
        if self.root.key == key:
            return self.root
        return None

    def find_max(self):
        if self.is_empty():
            return None
        current = self.root
        while current.right is not None:
            current = current.right
        return current

    def find_min(self):
        if self.is_empty():
            return None
        current = self.root
        while current.left is not None:
            current = current.left
        return current

    def find_greatests_less_than(self, key):
        if self.is_empty():
            return None
        self.splay(key)
        if self.root.key <= key:
            return self.root
        else:
            tmp = self.root
            self.root = self.root.left
            result = self.find_max()
            self.root = tmp
            return result

    def export_value_list(self):
        result = []
        nodes_to_visit = [self.root]
        while len(nodes_to_visit) > 0:
            node = nodes_to_visit.pop()
            if not node:
                continue
            result.append(node.value)
            nodes_to_visit.append(node.left)
            nodes_to_visit.append(node.right)
        return result

    def splay(self, key):
        if self.is_empty():
            return
        dummy = left = right = SplayTreeNode(None, None)
        current = self.root
        while True:
            if key < current.key:
                if not current.left:
                    break
                if key < current.left.key:
                    tmp = current.left
                    current.left = tmp.right
                    tmp.right = current
                    current = tmp
                    if not current.left:
                        break
                right.left = current
                right = current
                current = current.left
            elif key > current.key:
                if not current.right:
                    break
                if key > current.right.key:
                    tmp = current.right
                    current.right = tmp.left
                    tmp.left = current
                    current = tmp
                    if not current.right:
                        break
                left.right = current
                left = current
                current = current.right
            else:
                break
        left.right = current.left
        right.left = current.right
        current.left = dummy.right
        current.right = dummy.left
        self.root = current


class V8CodeMap(object):
    PAGE_ALIGNMENT = 12
    PAGE_SIZE = 1 << 12

    def __init__(self):
        self.statics = SplayTree()
        self.dynamics = SplayTree()
        self.libraries = SplayTree()
        self.pages = {}

    def add_code(self, start, entry):
        self.delete_all_covered_nodes(self.dynamics, start, start + entry.size)
        self.dynamics.insert(start, entry)

    def add_static_code(self, start, entry):
        self.statics.insert(start, entry)

    def add_library(self, start, entry):
        self.mark_pages(start, start + entry.size)
        self.libraries.insert(start, entry)

    @staticmethod
    def rshift(val, n):
        return (val % 0x100000000) >> n

    def mark_pages(self, start, end):
        addr = start
        while addr <= end:
            page = self.rshift(addr, self.PAGE_ALIGNMENT)
            self.pages[page] = 1
            addr += self.PAGE_SIZE

    def delete_all_covered_nodes(self, tree, start, end):
        to_delete = []
        addr = end - 1
        while addr >= start:
            node = tree.find_greatests_less_than(addr)
            if not node:
                break
            start2= node.key
            end2 = start2 + node.value.size
            if start2 < end and start < end2:
                to_delete.append(start2)
            addr = start2 - 1
        for addr in to_delete:
            tree.remove(addr)

    def find_dynamic_entry_by_start_addr(self, addr):
        node = self.dynamics.find(addr)
        if node:
            return node.value

    def find_in_tree(self, tree, addr):
        node = tree.find_greatests_less_than(addr)
        if node and self.is_address_belongs_to(addr, node):
            return node.value
        else:
            return None

    @staticmethod
    def is_address_belongs_to(addr, node):
        return node.key <= addr < (node.key + node.value.size)

    def find_entry(self, addr):
        page_addr = self.rshift(addr, self.PAGE_ALIGNMENT)
        if page_addr in self.pages:
            return self.find_in_tree(self.statics, addr) or self.find_in_tree(self.libraries, addr)

        min_kv = self.dynamics.find_min()
        max_kv = self.dynamics.find_max()
        if max_kv is not None and min_kv.key <= addr < (max_kv.key + max_kv.value.size):
            dyna_entry = self.find_in_tree(self.dynamics, addr)
            # TODO: handle cases when entry name was updated
            return dyna_entry
        return None


class CodeEntry(object):
    def __init__(self, size, name=None):
        self.size = size
        self.name = name or ''
        self.tick_count = 0
        self.stacks = {}

    def tick(self, stack):
        self.tick_count += 1
        if stack:
            stack.insert(0, self.get_name())
            stack_key = tuple(stack)
            self.stacks[stack_key] = self.stacks.setdefault(stack_key, 0) + 1

    def get_name(self):
        return self.name

    def __repr__(self):
        return "CodeEntry(size=%s, name=%s)" % (self.size, self.name)


class DynamicCodeEntry(CodeEntry):
    def __init__(self, size, type, name):
        super(DynamicCodeEntry, self).__init__(size, name)
        self.type = type

    def get_name(self):
        return self.type + ": " + self.name

    def __repr__(self):
        return "DynamicCodeEntry(type=%s, size=%s, name=%s)" % (self.type, self.size, self.name)


class DynamicFuncCodeEntry(CodeEntry):
    def __init__(self, size, type, func, opt_state):
        super(DynamicFuncCodeEntry, self).__init__(size)
        self.type = type
        self.func = func
        self.opt_state = opt_state

    def get_name(self):
        name = self.func.get_name()
        return self.type + ": " + self.opt_state + name

    def __repr__(self):
        tmpl = "DynamicFuncCodeEntry(type=%s, func=%s, opt_state=%s, size=%s)"
        return tmpl % (self.type, self.func, self.opt_state, self.size)


class FunctionEntry(CodeEntry):
    def __init__(self, name):
        super(FunctionEntry, self).__init__(0, name)

    def get_name(self):
        name = self.name
        if not name:
            name = '<anonymous>'
        elif name[0] == ' ':
            name = '<anonymous>' + name
        return name

    def __repr__(self):
        return "FunctionEntry(name=%s)" % self.name


class V8Profile(object):
    def __init__(self):
        self.codemap = V8CodeMap()
        self.stacks = []

    def add_code(self, type, name, start, size):
        entry = DynamicCodeEntry(size, type, name)
        self.codemap.add_code(start, entry)
        return entry

    def add_static_code(self, name, start_addr, end_addr):
        entry = CodeEntry(end_addr - start_addr, name)
        self.codemap.add_static_code(start_addr, entry)
        return entry

    def add_library(self, name, start_addr, end_addr):
        entry = CodeEntry(end_addr - start_addr, name)
        self.codemap.add_library(start_addr, entry)
        return entry

    def add_func_code(self, type, name, start, size, func_addr, opt_state):
        func = self.codemap.find_dynamic_entry_by_start_addr(func_addr)
        if not func:
            func = FunctionEntry(name)
            self.codemap.add_code(func_addr, func)
        elif func.name != name:
            func.name = name

        entry = self.codemap.find_dynamic_entry_by_start_addr(start)
        if entry:
            # code was recompiled, update optimization state
            if entry.size == size and entry.func == func:
                entry.opt_state = opt_state
        else:
            entry = DynamicFuncCodeEntry(size, type, func, opt_state)
            self.codemap.add_code(start, entry)

        return entry

    def find_entry(self, addr):
        return self.codemap.find_entry(addr)

    def resolve_and_filter_funcs(self, stack):
        result = []
        for addr in stack:
            entry = self.codemap.find_entry(addr)
            if entry:
                name = entry.get_name()
                result.append(name)
            else:
                # TODO: handle unknown code in stack
                pass
        return result

    def record_tick(self, stack):
        resolved_stack = self.resolve_and_filter_funcs(stack)
        self.stacks.append(resolved_stack)
        # self.bottom_up_tree.add_path(resolved_stack)
        # self.top_down_tree.add_path(reversed(resolved_stack))

    # TODO: move code. delete code, etc


class V8LogReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        # aggregates
        self.profile = V8Profile()
        self.ticks = 0
        self.current_time = 0
        self.rests = []

    def __open_fds(self):
        if self.fds is None:
            self.fds = open(self.filename)

    def parse(self):
        self.__open_fds()
        reader = csv.reader(self.fds)
        for row in reader:
            row_name = row[0]
            if row_name == 'code-creation':
                self.process_code_creation(row)
            elif row_name == 'tick':
                self.process_tick(row)
            elif row_name == 'current-time':
                self.process_current_time(row)
            elif row_name == 'shared-library':
                self.process_shared_library(row)
            else:
                pass

    def process_code_creation(self, args):
        type = args[1]
        kind = int(args[2], 16)
        start_addr = int(args[3], 16)
        size = int(args[4], 16)
        name = args[5]
        maybe_func = args[6:]
        if maybe_func:
            func_addr = int(maybe_func[0], 16)
            opt_state = maybe_func[1]  # "" - compiled, "~" - optimizable, "*" - optimized
            self.profile.add_func_code(type, name, start_addr, size, func_addr, opt_state)
        else:
            self.profile.add_code(type, name, start_addr, size)

    def process_tick(self, args):
        pc = int(args[1], 16)
        ns_offset = int(args[2])
        is_external_callback = int(args[3])
        tos_or_external_callback = int(args[4], 16)
        vm_state = int(args[5], 16)
        stack = args[6:]

        self.ticks += 1

        if is_external_callback:
            pc = tos_or_external_callback
            tos_or_external_callback = 0
        elif tos_or_external_callback:
            func_entry = self.profile.find_entry(tos_or_external_callback)
            if not func_entry or not isinstance(func_entry, DynamicFuncCodeEntry):
                tos_or_external_callback = 0

        processed_stack = self.process_stack(pc, tos_or_external_callback, stack)

        entry = self.profile.find_entry(pc)
        if entry:
            entry.tick(processed_stack)

        # logging.debug("%s -> %s", stack, processed_stack)
        self.profile.record_tick(processed_stack)

    def extract_shared_library_functions(self, filename, start, end):
        command = 'nm -C -n "%s"; nm -C -n -D "%s"' % (filename, filename)
        process = subprocess.Popen(command, shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT)
        pipe = process.stdout

        prev_entry = None

        try:
            for line in pipe:
                row = re.match('^([0-9a-fA-F]{8}) . (.*)$', line)
                if row:
                    addr = int(row.group(1), 16)
                    if addr < start and addr < end - start:
                        addr += start
                    name = row.group(2)
                    yield addr, name
                    self.cpp_entries.insert(addr, tickprocessor.CodeEntry(addr, row.group(2)))
        finally:
            pipe.close()

    def process_shared_library(self, args):
        name = args[1]
        start_addr = int(args[2], 16)
        end_addr = int(args[3], 16)
        entry = self.profile.add_library(name, start_addr, end_addr)
        # TODO: add static code cpp entries
        for fun_name, fun_start, fun_end in self.extract_shared_library_functions(name, start_addr, end_addr):
            self.profile.add_static_code(fun_name, fun_start, fun_end)

    @staticmethod
    def process_stack(pc, func, stack):
        full_stack = [pc, func] if func else [pc]
        prev_frame = pc
        for frame in stack:
            first_char = frame[0]
            if first_char in "+-":
                prev_frame += int(frame, 16)
                full_stack.append(prev_frame)
            elif first_char != "o":
                full_stack.append(int(frame, 16))
        return full_stack

    def process_current_time(self, args):
        self.current_time = int(args[1])


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
        self.v8_complete_events = defaultdict(list)  # pid -> [(ts, name, duration)]
        self.js_function_calls = defaultdict(list)  # pid -> [(ts, function name, duration)]
        self.v8_cpu_profile = defaultdict(list)  # pid -> [(ts, event name, args)]

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
            ts = self.convert_ts(event['ts'])
            name = event['name']
            duration = event.get('dur', 1)
            self.v8_complete_events[pid].append((ts, name, duration))

        if event.get("name") == "FunctionCall":
            pid = event['pid']
            ts = self.convert_ts(event['ts'])
            function_name = event['args']['data']['functionName'] + ":" + event['args']['data']['scriptName']
            duration = event.get('dur', event.get('tdur', 1))
            self.js_function_calls[pid].append((ts, function_name, duration))
        elif event.get("name") in ("JitCodeAdded", "JitCodeMoved", "V8Sample"):
            pid = event['pid']
            ts = self.convert_ts(event['ts'])
            name = event["name"]
            args = event["args"]
            self.v8_cpu_profile[pid].append((ts, name, args))

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
        if self.tracing_tab_pid not in self.v8_complete_events:
            msg = ("No JS timing events were recorded for Chrome tab. "
                   "Ensure that 'v8' category is enabled.")
            self.log.warning(msg)
            return

        v8_events = self.v8_complete_events[self.tracing_tab_pid]

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

    def get_js_function_call_stats(self):
        fun_stats = dict()
        names = set()
        for _, name, duration in self.js_function_calls[self.tracing_tab_pid]:
            if name not in fun_stats:
                fun_stats[name] = {"calls": 0, "duration": 0.0}
            fun_stats[name]["calls"] += 1
            fun_stats[name]["duration"] += duration
            names.add(name)
        for name in names:
            fun_stats[name]["duration"] /= 1000000
        return fun_stats


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

    def get_js_function_call_stats(self):
        return self.extractor.get_js_function_call_stats()

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

        fun_stats = self.chrome_profiler.get_js_function_call_stats()
        if fun_stats:
            self.log.info("Top JS functions:")
            lstats = [(name, stat) for name, stat in fun_stats.items()]
            by_duration = sorted(lstats, key=lambda namestat: namestat[1]["duration"], reverse=True)
            for name, stat in by_duration[:10]:
                if len(name) > 60:
                    name = name[:60] + "..."
                self.log.info("%s: calls=%s, duration=%s", name, stat["calls"], stat["duration"])
