"""
Copyright 2017 BlazeMeter Inc.

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
import logging
import traceback
import mimetypes
import re

from bzt import TaurusConfigError, TaurusInternalException
from bzt.utils import ensure_is_dict, dehumanize_time, BetterDict, parse_think_time

VARIABLE_PATTERN = re.compile("\${.+\}")


def has_variable_pattern(val):
    if isinstance(val, str):
        return bool(VARIABLE_PATTERN.search(val))
    else:
        return False


class Request(object):
    NAME = "request"

    def __init__(self, config, scenario=None):
        self.label = None
        self.config = config
        self.scenario = scenario

    def priority_option(self, name, default=None):
        val = self.config.get(name, None)
        if val is None:
            val = self.scenario.get(name, None)
        if val is None and default is not None:
            val = default
        return val


class HTTPRequest(Request):
    NAME = "request"

    def __init__(self, config, scenario, engine, pure_body_file=False):
        self.engine = engine
        self.log = self.engine.log.getChild(self.__class__.__name__)
        super(HTTPRequest, self).__init__(config, scenario)
        msg = "Option 'url' is mandatory for request but not found in %s" % config
        self.url = self.config.get("url", TaurusConfigError(msg))
        self.label = str(self.config.get("label", self.url))
        self.method = self.config.get("method", "GET")
        if not has_variable_pattern(self.method):
            self.method = self.method.upper()

        # TODO: add method to join dicts/lists from scenario/request level?
        self.headers = self.config.get("headers", {})

        self.keepalive = self.config.get('keepalive', None)
        self.timeout = self.config.get('timeout', None)
        self.follow_redirects = self.config.get('follow-redirects', None)
        self.body = self._get_body(pure_body_file=pure_body_file)

    def get_think_time(self, full=False):
        think_time = self.priority_option('think-time')
        if think_time:
            return parse_think_time(think_time=think_time, full=full)

    def get_header(self, name):
        def dic_lower(dic):
            return {str(k).lower(): str(dic[k]).lower() for k in dic}

        scenario_headers = dic_lower(self.scenario.get_headers())
        request_headers = dic_lower(self.headers)
        headers = BetterDict.from_dict(scenario_headers)
        headers.merge(request_headers)
        return headers.get(name.lower(), None)

    def _get_body(self, pure_body_file=False):
        # todo: if self.method not in ("PUT", "POST")?
        body = self.config.get('body', None)
        body_file = self.config.get('body-file')
        if body_file:
            if body:
                self.log.warning('body and body-file fields are found, only first will take effect')
            else:
                if not pure_body_file:
                    body_file_path = self.engine.find_file(body_file)
                    with open(body_file_path) as fhd:
                        body = fhd.read()

        return body


class HierarchicHTTPRequest(HTTPRequest):
    def __init__(self, config, scenario, engine):
        super(HierarchicHTTPRequest, self).__init__(config, scenario, engine, pure_body_file=True)
        self.upload_files = self.config.get("upload-files", [])

        if self.method == "PUT" and len(self.upload_files) > 1:
            self.upload_files = self.upload_files[:1]

        for file_dict in self.upload_files:
            param = file_dict.get("param", None)

            if self.method == "PUT":
                file_dict["param"] = ""
            if self.method == "POST" and not param:
                raise TaurusConfigError("Items from upload-files must specify parameter name")

            path_exc = TaurusConfigError("Items from upload-files must specify path to file")
            path = str(file_dict.get("path", path_exc))
            if not has_variable_pattern(path):  # exclude variables
                path = self.engine.find_file(path)  # prepare full path for jmx
            else:
                msg = "Path '%s' contains variable and can't be expanded. Don't use relative paths in 'upload-files'!"
                self.log.warning(msg % path)

            file_dict["path"] = path

            mime = mimetypes.guess_type(file_dict["path"])[0] or "application/octet-stream"
            file_dict.get("mime-type", mime, force_set=True)
        self.content_encoding = self.config.get('content-encoding', None)


class IfBlock(Request):
    NAME = "if"

    def __init__(self, condition, then_clause, else_clause, config):
        super(IfBlock, self).__init__(config)
        self.condition = condition
        self.then_clause = then_clause
        self.else_clause = else_clause

    def __repr__(self):
        then_clause = [repr(req) for req in self.then_clause]
        else_clause = [repr(req) for req in self.else_clause]
        return "IfBlock(condition=%s, then=%s, else=%s)" % (self.condition, then_clause, else_clause)


class OnceBlock(Request):
    NAME = "once"

    def __init__(self, requests, config):
        super(OnceBlock, self).__init__(config)
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        return "OnceBlock(requests=%s)" % requests


class LoopBlock(Request):
    NAME = "loop"

    def __init__(self, loops, requests, config):
        super(LoopBlock, self).__init__(config)
        self.loops = loops
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        return "LoopBlock(loops=%s, requests=%s)" % (self.loops, requests)


class WhileBlock(Request):
    NAME = "while"

    def __init__(self, condition, requests, config):
        super(WhileBlock, self).__init__(config)
        self.condition = condition
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        return "WhileBlock(condition=%s, requests=%s)" % (self.condition, requests)


class ForEachBlock(Request):
    NAME = "foreach"

    def __init__(self, input_var, loop_var, requests, config):
        super(ForEachBlock, self).__init__(config)
        self.input_var = input_var
        self.loop_var = loop_var
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        fmt = "ForEachBlock(input=%s, loop_var=%s, requests=%s)"
        return fmt % (self.input_var, self.loop_var, requests)


class TransactionBlock(Request):
    NAME = "transaction"

    def __init__(self, name, requests, include_timers, config, scenario):
        super(TransactionBlock, self).__init__(config, scenario)
        self.label = name
        self.requests = requests
        self.include_timers = include_timers

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        fmt = "TransactionBlock(name=%s, requests=%s, include-timers=%r)"
        return fmt % (self.label, requests, self.include_timers)


class IncludeScenarioBlock(Request):
    NAME = "include-scenario"

    def __init__(self, scenario_name, config):
        super(IncludeScenarioBlock, self).__init__(config)
        self.scenario_name = scenario_name

    def __repr__(self):
        return "IncludeScenarioBlock(scenario_name=%r)" % self.scenario_name


class RequestParser(object):
    def __init__(self, scenario, engine):
        self.engine = engine
        self.scenario = scenario

    def _parse_requests(self, raw_requests, require_url=True):
        requests = []
        for key in range(len(raw_requests)):  # pylint: disable=consider-using-enumerate
            req = ensure_is_dict(raw_requests, key, "url")
            if not require_url and "url" not in req:
                req["url"] = None
            try:
                requests.append(self._parse_request(req))
            except BaseException as exc:
                logging.debug("%s\n%s" % (exc, traceback.format_exc()))
                raise TaurusConfigError("Wrong request:\n %s" % req)
        return requests

    def _parse_request(self, req):
        return HTTPRequest(req, self.scenario, self.engine)

    def _expand_transactions(self, requests):
        res = []
        for req in requests:
            if TransactionBlock.NAME in req:
                res.extend(self._expand_transactions(req.get("do")))
            else:
                res.append(req)

        return res

    def extract_requests(self, require_url=True):
        requests = self.scenario.get("requests", [])
        requests = self._expand_transactions(requests)
        return self._parse_requests(requests, require_url=require_url)


class HierarchicRequestParser(RequestParser):
    def extract_requests(self, require_url=True):
        requests = self.scenario.get("requests", [])
        return self._parse_requests(requests, require_url=require_url)

    def _parse_request(self, req):
        if 'if' in req:
            condition = req.get("if")

            # TODO: apply some checks to `condition`?
            then_clause = req.get("then", TaurusConfigError("'then' clause is mandatory for 'if' blocks"))
            then_requests = self._parse_requests(then_clause)
            else_clause = req.get("else", [])
            else_requests = self._parse_requests(else_clause)
            return IfBlock(condition, then_requests, else_requests, req)
        elif 'once' in req:
            do_block = req.get("once", TaurusConfigError("operation list is mandatory for 'once' blocks"))
            do_requests = self._parse_requests(do_block)
            return OnceBlock(do_requests, req)
        elif 'loop' in req:
            loops = req.get("loop")
            do_block = req.get("do", TaurusConfigError("'do' option is mandatory for 'loop' blocks"))
            do_requests = self._parse_requests(do_block)
            return LoopBlock(loops, do_requests, req)
        elif 'while' in req:
            condition = req.get("while")
            do_block = req.get("do", TaurusConfigError("'do' option is mandatory for 'while' blocks"))
            do_requests = self._parse_requests(do_block)
            return WhileBlock(condition, do_requests, req)
        elif 'foreach' in req:
            iteration_str = req.get("foreach")
            match = re.match(r'(.+) in (.+)', iteration_str)
            if not match:
                msg = "'foreach' value should be in format '<elementName> in <collection>' but '%s' found"
                raise TaurusConfigError(msg % iteration_str)
            loop_var, input_var = match.groups()
            do_block = req.get("do", TaurusConfigError("'do' field is mandatory for 'foreach' blocks"))
            do_requests = self._parse_requests(do_block)
            return ForEachBlock(input_var, loop_var, do_requests, req)
        elif TransactionBlock.NAME in req:
            name = req.get(TransactionBlock.NAME)
            do_block = req.get('do', TaurusConfigError("'do' field is mandatory for transaction blocks"))
            do_requests = self._parse_requests(do_block)
            include_timers = req.get('include-timers')
            return TransactionBlock(name, do_requests, include_timers, req, self.scenario)
        elif 'include-scenario' in req:
            name = req.get('include-scenario')
            return IncludeScenarioBlock(name, req)
        elif 'action' in req:
            action = req.get('action')
            if action not in ('pause', 'stop', 'stop-now', 'continue'):
                raise TaurusConfigError("Action should be either 'pause', 'stop', 'stop-now' or 'continue'")
            target = req.get('target', 'current-thread')
            if target not in ('current-thread', 'all-threads'):
                msg = "Target for action should be either 'current-thread' or 'all-threads' but '%s' found"
                raise TaurusConfigError(msg % target)
            duration = req.get('pause-duration', None)
            if duration is not None:
                duration = dehumanize_time(duration)
            return ActionBlock(action, target, duration, req)
        elif 'set-variables' in req:
            mapping = req.get('set-variables')
            return SetVariables(mapping, req)
        else:
            return HierarchicHTTPRequest(req, self.scenario, self.engine)


class ActionBlock(Request):
    def __init__(self, action, target, duration, config):
        super(ActionBlock, self).__init__(config)
        self.action = action
        self.target = target
        self.duration = duration


class SetVariables(Request):
    def __init__(self, mapping, config):
        super(SetVariables, self).__init__(config)
        self.mapping = mapping


class RequestVisitor(object):
    def __init__(self):
        self.path = []

    def clear_path_cache(self):
        self.path = []

    def record_path(self, path):
        self.path.append(path)

    def visit(self, node):
        class_name = node.__class__.__name__.lower()
        visitor = getattr(self, 'visit_' + class_name, None)
        if visitor is not None:
            return visitor(node)
        raise TaurusInternalException("Visitor for class %s not found" % class_name)


class ResourceFilesCollector(RequestVisitor):
    def __init__(self, executor):
        """
        :param executor: JMeterExecutor
        """
        super(ResourceFilesCollector, self).__init__()
        self.executor = executor

    def visit_hierarchichttprequest(self, request):
        files = []

        body_file = request.config.get('body-file')
        if body_file:
            files.append(body_file)

        uploads = request.config.get('upload-files', [])
        files.extend([x['path'] for x in uploads if not has_variable_pattern(x['path'])])

        if 'jsr223' in request.config:
            jsrs = request.config.get('jsr223')
            if isinstance(jsrs, dict):
                jsrs = [jsrs]
            for jsr in jsrs:
                if 'script-file' in jsr:
                    files.append(jsr.get('script-file'))
        return files

    def visit_onceblock(self, block):
        return self.visit_loopblock(block)

    def visit_ifblock(self, block):
        files = []
        for request in block.then_clause:
            files.extend(self.visit(request))
        for request in block.else_clause:
            files.extend(self.visit(request))
        return files

    def visit_loopblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_whileblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_foreachblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_transactionblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_includescenarioblock(self, block):
        scenario_name = block.scenario_name
        if scenario_name in self.path:
            msg = "Mutual recursion detected in include-scenario blocks (scenario %s)"
            raise TaurusConfigError(msg % scenario_name)
        self.record_path(scenario_name)
        scenario = self.executor.get_scenario(name=block.scenario_name)
        return self.executor.res_files_from_scenario(scenario)

    def visit_actionblock(self, _):
        return []

    def visit_setvariables(self, _):
        return []
