"""
Module holds JMX handlers implementations

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
import os
import re
from distutils.version import LooseVersion
from lxml import etree

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Scenario
from bzt.jmx import JMX
from bzt.jmx.base import cond_int
from bzt.jmx.threadgroups import ThreadGroup, ConcurrencyThreadGroup, ThreadGroupHandler
from bzt.requests_model import RequestVisitor, has_variable_pattern, HierarchicRequestParser
from bzt.utils import iteritems, numeric_types
from bzt.utils import BetterDict, dehumanize_time, ensure_is_dict, load_class, guess_delimiter


class RequestCompiler(RequestVisitor):
    def __init__(self, jmx_builder):
        super(RequestCompiler, self).__init__()
        self.jmx_builder = jmx_builder

    def visit_hierarchichttprequest(self, request):
        return self.jmx_builder.compile_request(request)

    def visit_ifblock(self, block):
        return self.jmx_builder.compile_if_block(block)

    def visit_onceblock(self, block):
        return self.jmx_builder.compile_once_block(block)

    def visit_loopblock(self, block):
        return self.jmx_builder.compile_loop_block(block)

    def visit_whileblock(self, block):
        return self.jmx_builder.compile_while_block(block)

    def visit_foreachblock(self, block):
        return self.jmx_builder.compile_foreach_block(block)

    def visit_transactionblock(self, block):
        return self.jmx_builder.compile_transaction_block(block)

    def visit_includescenarioblock(self, block):
        scenario_name = block.scenario_name
        if scenario_name in self.path:
            msg = "Mutual recursion detected in include-scenario blocks (scenario %s)"
            raise TaurusConfigError(msg % scenario_name)
        self.record_path(scenario_name)
        return self.jmx_builder.compile_include_scenario_block(block)

    def visit_actionblock(self, block):
        return self.jmx_builder.compile_action_block(block)

    def visit_setvariables(self, block):
        return self.jmx_builder.compile_set_variables_block(block)


class LoadSettingsProcessor(object):
    TG = ThreadGroup.__name__
    CTG = ConcurrencyThreadGroup.__name__

    def __init__(self, executor):
        self.log = executor.log.getChild(self.__class__.__name__)
        self.load = executor.get_specific_load()
        self.raw_load = executor.get_raw_load()
        self.log.debug("Load: %s", self.load)
        self.tg = self._detect_thread_group(executor)
        self.tg_handler = ThreadGroupHandler(self.log)

    def _detect_thread_group(self, executor):
        """
        Detect preferred thread group
        :param executor:
        :return:
        """
        tg = self.TG
        if not executor.settings.get('force-ctg', True):
            return tg

        msg = 'Thread group detection: %s, regular ThreadGroup will be used'

        if not self.load.duration:
            self.log.debug(msg, 'duration not found')
        elif self.load.iterations:
            self.log.debug(msg, 'iterations are found')
        elif not executor.tool:
            msg = 'You must set executor tool (%s) for choosing of ConcurrencyThreadGroup'
            raise TaurusInternalException(msg % executor.tool_name)
        elif not executor.tool.ctg_plugin_installed():
            self.log.warning(msg % 'plugin for ConcurrentThreadGroup not found')
        else:
            tg = self.CTG

        return tg

    def modify(self, jmx):
        if not (self.raw_load.iterations or self.raw_load.concurrency or self.load.duration):
            self.log.debug('No iterations/concurrency/duration found, thread group modification is skipped')
            return

        # IMPORTANT: fix groups order as changing of element type changes order of getting of groups
        groups = list(self.tg_handler.groups(jmx))

        # user concurrency is jmeter variable, write it to tg as is
        if isinstance(self.load.concurrency, str):
            target_list = [(group, self.load.concurrency) for group in groups]
        else:  # concurrency is numeric or empty
            raw = self.load.concurrency is None  # keep existed concurrency if self.load.concurrency is omitted

            concurrency_list = []
            for group in groups:
                concurrency = group.get_concurrency(raw=raw)
                if concurrency is None:
                    concurrency = 1
                concurrency_list.append(concurrency)

            if not raw:  # divide numeric concurrency
                self._divide_concurrency(concurrency_list)

            target_list = zip(groups, concurrency_list)

        for group, concurrency in target_list:
            self.tg_handler.convert(source=group, target_gtype=self.tg, load=self.load, concurrency=concurrency)

        if self.load.throughput:
            self._add_shaper(jmx)

        if self.tg == self.TG and self.load.steps:
            self.log.warning("Stepping ramp-up isn't supported for regular ThreadGroup")

    def _divide_concurrency(self, concurrency_list):
        """
        calculate target concurrency for every thread group
        """
        total_old_concurrency = sum(concurrency_list)

        for idx, concurrency in enumerate(concurrency_list):
            if total_old_concurrency and concurrency_list[idx] != 0:
                part_of_load = 1.0 * self.load.concurrency * concurrency / total_old_concurrency
                concurrency_list[idx] = int(round(part_of_load))
                if concurrency_list[idx] == 0:
                    concurrency_list[idx] = 1
            else:
                concurrency_list[idx] = 0

        total_new_concurrency = sum(concurrency_list)
        leftover = self.load.concurrency - total_new_concurrency
        if leftover < 0:
            msg = "Had to add %s more threads to maintain thread group proportion"
            self.log.warning(msg, -leftover)
        elif leftover > 0:
            msg = "%s threads left undistributed due to thread group proportion"
            self.log.warning(msg, leftover)

    def _add_shaper(self, jmx):
        """
        Add shaper
        :param jmx: JMX
        :return:
        """
        if not self.load.duration:
            self.log.warning("You must set 'ramp-up' and/or 'hold-for' when using 'throughput' option")
            return

        etree_shaper = jmx.get_rps_shaper()
        if self.load.ramp_up:
            if isinstance(self.load.throughput, numeric_types) and self.load.duration:
                start_rps = self.load.throughput / float(self.load.duration)
                start_rps = max(start_rps, 0.001)  # avoid zeroing
                start_rps = min(start_rps, 1.0)  # avoid starting too fast
            else:
                start_rps = 1

            if not self.load.steps:
                jmx.add_rps_shaper_schedule(etree_shaper, start_rps, self.load.throughput, self.load.ramp_up)
            else:
                step_h = self.load.throughput / self.load.steps
                step_w = float(self.load.ramp_up) / self.load.steps
                accum_time = 0
                for step in range(1, self.load.steps + 1):
                    jmx.add_rps_shaper_schedule(etree_shaper, step_h * step, step_h * step,
                                                step_w * step - accum_time)
                    accum_time += cond_int(step_w * step - accum_time)

        if self.load.hold:
            jmx.add_rps_shaper_schedule(etree_shaper, self.load.throughput, self.load.throughput, self.load.hold)

        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree_shaper)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))


class ProtocolHandler(object):

    def __init__(self, sys_props):
        super(ProtocolHandler, self).__init__()
        self.system_props = sys_props

    def get_toplevel_elements(self, scenario):
        return []

    def get_sampler_pair(self, request):
        return None, None

    @staticmethod
    def safe_time(any_time):
        try:
            smart_time = int(1000 * dehumanize_time(any_time))
        except TaurusInternalException:
            smart_time = any_time

        return smart_time


class JMeterScenarioBuilder(JMX):
    """
    Helper to build JMeter test plan from Scenario

    :type protocol_handlers: dict[str,ProtocolHandler]
    """

    def __init__(self, executor, original=None):
        """
        :type executor: ScenarioExecutor
        :type original: JMX
        """
        super(JMeterScenarioBuilder, self).__init__(original)
        self.executor = executor
        self.scenario = executor.get_scenario()
        self.engine = executor.engine
        self.system_props = BetterDict()
        self.request_compiler = None
        self.default_protocol = self.executor.settings.get('default-protocol', 'http')
        self.protocol_handlers = {}
        for protocol, cls_name in iteritems(self.executor.settings.get("protocol-handlers")):
            cls_obj = load_class(cls_name)
            instance = cls_obj(self.system_props)
            self.protocol_handlers[protocol] = instance
        self.FIELD_KEYSTORE_CONFIG = 'keystore-config'

    @staticmethod
    def _get_timer(req):
        think_time = req.get_think_time(full=True)
        if not think_time:
            return []

        if not isinstance(think_time, list):  # constant
            return JMX.get_constant_timer(delay=ProtocolHandler.safe_time(think_time))

        mean = ProtocolHandler.safe_time(think_time[1])
        dev = ProtocolHandler.safe_time(think_time[2])

        if think_time[0] == "uniform":
            return JMX.get_uniform_timer(maximum=dev * 2, offset=mean - dev)
        elif think_time[0] == "gaussian":
            return JMX.get_gaussian_timer(dev=dev, offset=mean)
        elif think_time[0] == "poisson":
            return JMX.get_poisson_timer(lam=mean - dev, delay=dev)
        else:
            raise TaurusConfigError("Wrong timer type: %s" % think_time[0])

    def __add_extractors(self, children, req):
        self.__add_boundary_ext(children, req)
        self.__add_regexp_ext(children, req)
        self.__add_json_ext(children, req)
        self.__add_jquery_ext(children, req)
        self.__add_xpath_ext(children, req)

    def __add_boundary_ext(self, children, req):
        extractors = req.config.get("extract-boundary")
        for varname, cfg in iteritems(extractors):
            subj = cfg.get('subject', 'body')
            left = cfg.get('left', TaurusConfigError("Left boundary is missing for boundary extractor %s" % varname))
            right = cfg.get('right', TaurusConfigError("Right boundary is missing for boundary extractor %s" % varname))
            match_no = cfg.get('match-no', 1)
            defvalue = cfg.get('default', 'NOT_FOUND')
            scope = cfg.get("scope", None)
            from_var = cfg.get("from-variable", None)
            extractor = JMX._get_boundary_extractor(varname, subj, left, right, match_no, defvalue, scope, from_var)
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_regexp_ext(self, children, req):
        extractors = req.config.get("extract-regexp")
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            scope = cfg.get("scope", None)
            from_var = cfg.get("from-variable", None)

            extractor = JMX._get_extractor(varname, cfg.get('subject', 'body'), cfg['regexp'], cfg.get('template', 1),
                                           cfg.get('match-no', 1), cfg.get('default', 'NOT_FOUND'), scope, from_var)
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_json_ext(self, children, req):
        jextractors = req.config.get("extract-jsonpath")
        for varname in jextractors:
            cfg = ensure_is_dict(jextractors, varname, "jsonpath")
            if LooseVersion(str(self.executor.settings.get("version"))) < LooseVersion("3.0"):
                extractor = JMX._get_json_extractor(varname,
                                                    cfg["jsonpath"],
                                                    cfg.get("default", "NOT_FOUND"),
                                                    cfg.get("from-variable", None))
            else:
                extractor = JMX._get_internal_json_extractor(varname,
                                                             cfg["jsonpath"],
                                                             cfg.get("default", "NOT_FOUND"),
                                                             cfg.get("scope", None),
                                                             cfg.get("from-variable", None),
                                                             cfg.get("match-no", "0"),
                                                             cfg.get("concat", False))

            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_jquery_ext(self, children, req):
        css_jquery_extors = req.config.get("extract-css-jquery")
        for varname in css_jquery_extors:
            cfg = ensure_is_dict(css_jquery_extors, varname, "expression")
            extractor = self._get_jquerycss_extractor(varname,
                                                      cfg['expression'],
                                                      cfg.get('attribute', ""),
                                                      cfg.get('match-no', 0),
                                                      cfg.get('default', 'NOT_FOUND'),
                                                      cfg.get("scope", None),
                                                      cfg.get("from-variable", None))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_xpath_ext(self, children, req):
        xpath_extractors = req.config.get("extract-xpath")
        for varname in xpath_extractors:
            cfg = ensure_is_dict(xpath_extractors, varname, "xpath")
            children.append(JMX._get_xpath_extractor(varname,
                                                     cfg['xpath'],
                                                     cfg.get('default', 'NOT_FOUND'),
                                                     cfg.get('validate-xml', False),
                                                     cfg.get('ignore-whitespace', True),
                                                     cfg.get("match-no", "-1"),
                                                     cfg.get('use-namespaces', False),
                                                     cfg.get('use-tolerant-parser', False),
                                                     cfg.get("scope", None),
                                                     cfg.get("from-variable", None)))
            children.append(etree.Element("hashTree"))

    @staticmethod
    def __add_assertions(children, req):
        assertions = req.config.get("assert", [])
        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]
            children.append(JMX._get_resp_assertion(assertion.get("subject", Scenario.FIELD_BODY),
                                                    assertion['contains'],
                                                    assertion.get('regexp', True),
                                                    assertion.get('not', False),
                                                    assertion.get('assume-success', False)))
            children.append(etree.Element("hashTree"))

        jpath_assertions = req.config.get("assert-jsonpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "jsonpath")

            exc = TaurusConfigError('JSON Path not found in assertion: %s' % assertion)
            component = JMX._get_json_path_assertion(assertion.get('jsonpath', exc),
                                                     assertion.get('expected-value', ''),
                                                     assertion.get('validate', False),
                                                     assertion.get('expect-null', False),
                                                     assertion.get('invert', False),
                                                     assertion.get('regexp', True))
            children.append(component)
            children.append(etree.Element("hashTree"))

        xpath_assertions = req.config.get("assert-xpath", [])
        for idx, assertion in enumerate(xpath_assertions):
            assertion = ensure_is_dict(xpath_assertions, idx, "xpath")

            exc = TaurusConfigError('XPath not found in assertion: %s' % assertion)
            component = JMX._get_xpath_assertion(assertion.get('xpath', exc),
                                                 assertion.get('validate-xml', False),
                                                 assertion.get('ignore-whitespace', True),
                                                 assertion.get('use-tolerant-parser', False),
                                                 assertion.get('invert', False))
            children.append(component)
            children.append(etree.Element("hashTree"))

    @staticmethod
    def __add_jsr_elements(children, req, get_from_config=True):
        """
        :type children: etree.Element
        :type req: Request
        """
        jsrs = []
        if get_from_config:
            jsrs = req.config.get("jsr223", [])
        else:
            jsrs = req.get("jsr223", [])

        if not isinstance(jsrs, list):
            jsrs = [jsrs]
        for idx, _ in enumerate(jsrs):
            jsr = ensure_is_dict(jsrs, idx, sub_key='script-text')
            lang = jsr.get("language", "groovy")
            script_file = jsr.get("script-file", None)
            script_text = jsr.get("script-text", None)
            if not script_file and not script_text:
                raise TaurusConfigError("jsr223 element must specify one of 'script-file' or 'script-text'")
            parameters = jsr.get("parameters", "")
            execute = jsr.get("execute", "after")

            cache_key = str(jsr.get("compile-cache", True)).lower()

            children.append(JMX._get_jsr223_element(lang, script_file, parameters, execute, script_text, cache_key))
            children.append(etree.Element("hashTree"))

    def __gen_requests(self, scenario):
        is_protocol_rte = scenario.data.get('protocol', None) == "rte"
        requests = scenario.get_requests(parser=HierarchicRequestParser, require_url=not(is_protocol_rte))

        elements = []
        for compiled in self.compile_requests(requests):
            elements.extend(compiled)
        return elements

    def compile_scenario(self, scenario):
        elements = []
        for _, protocol in iteritems(self.protocol_handlers):
            elements.extend(protocol.get_toplevel_elements(scenario))
        elements.extend(self.__gen_authorization(scenario))
        elements.extend(self.__gen_keystore_config(scenario))
        elements.extend(self.__gen_data_sources(scenario))
        elements.extend(self.__gen_requests(scenario))
        self.__add_jsr_elements(elements, scenario, False)
        return elements

    def compile_request(self, request):
        """

        :type request: HierarchicHTTPRequest
        :return:
        """
        sampler = children = None
        protocol_name = request.priority_option('protocol', default=self.default_protocol)
        if protocol_name in self.protocol_handlers:
            protocol = self.protocol_handlers[protocol_name]
            sampler, children = protocol.get_sampler_pair(request)

        if sampler is None:
            self.log.warning("Problematic request: %s", request.config)
            raise TaurusInternalException("Unable to handle request, please review missing options")

        children.extend(self._get_timer(request))

        self.__add_assertions(children, request)

        timeout = ProtocolHandler.safe_time(request.priority_option('timeout'))
        if timeout is not None:
            children.append(JMX._get_dur_assertion(timeout))
            children.append(etree.Element("hashTree"))

        self.__add_extractors(children, request)

        self.__add_jsr_elements(children, request)

        return [sampler, children]

    def compile_if_block(self, block):
        elements = []

        # TODO: pass jmeter IfController options
        if_controller = JMX._get_if_controller(block.condition)
        then_children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.then_clause):
            for element in compiled:
                then_children.append(element)
        elements.extend([if_controller, then_children])

        if block.else_clause:
            inverted_condition = "!(" + block.condition + ")"
            else_controller = JMX._get_if_controller(inverted_condition)
            else_children = etree.Element("hashTree")
            for compiled in self.compile_requests(block.else_clause):
                for element in compiled:
                    else_children.append(element)
            elements.extend([else_controller, else_children])

        return elements

    def compile_once_block(self, block):
        elements = []

        once_controller = JMX._get_once_controller()
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([once_controller, children])

        return elements

    def compile_loop_block(self, block):
        elements = []

        loop_controller = JMX._get_loop_controller(block.loops)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([loop_controller, children])

        return elements

    def compile_while_block(self, block):
        elements = []

        controller = JMX._get_while_controller(block.condition)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([controller, children])

        return elements

    def compile_foreach_block(self, block):
        """
        :type block: ForEachBlock
        """

        elements = []

        controller = JMX._get_foreach_controller(block.input_var, block.loop_var)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([controller, children])

        return elements

    def compile_transaction_block(self, block):
        elements = []
        controller = JMX._get_transaction_controller(block.label,
                                                     block.priority_option('force-parent-sample', False),
                                                     block.include_timers)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([controller, children])
        return elements

    def compile_include_scenario_block(self, block):
        elements = []
        controller = JMX._get_simple_controller(block.scenario_name)
        children = etree.Element("hashTree")
        scenario = self.executor.get_scenario(name=block.scenario_name)
        for element in self.compile_scenario(scenario):
            children.append(element)
        elements.extend([controller, children])
        return elements

    def compile_action_block(self, block):
        """
        :type block: ActionBlock
        :return:
        """
        actions = {
            'stop': 0,
            'pause': 1,
            'stop-now': 2,
            'continue': 3,
        }
        targets = {'current-thread': 0, 'all-threads': 2}
        action = actions[block.action]
        target = targets[block.target]
        duration = 0
        if block.duration is not None:
            duration = int(block.duration * 1000)
        test_action = JMX._get_action_block(action, target, duration)
        children = etree.Element("hashTree")
        self.__add_jsr_elements(children, block)
        return [test_action, children]

    @staticmethod
    def compile_set_variables_block(block):
        set_var_action = JMX.get_set_var_action(block.mapping)
        hashtree = etree.Element("hashTree")
        return [set_var_action, hashtree]

    def compile_requests(self, requests):
        if self.request_compiler is None:
            self.request_compiler = RequestCompiler(self)
        compiled = []
        for request in requests:
            compiled.append(self.request_compiler.visit(request))
            self.request_compiler.clear_path_cache()
        return compiled

    def __generate(self):
        """
        Generate the test plan
        """

        thread_group = JMX.get_thread_group(testname=self.executor.label)
        thread_group_ht = etree.Element("hashTree", type="tg")

        # NOTE: set realistic dns-cache and JVM prop by default?
        self.request_compiler = RequestCompiler(self)
        for element in self.compile_scenario(self.scenario):
            thread_group_ht.append(element)

        results_tree = self._get_results_tree()
        results_tree_ht = etree.Element("hashTree")

        self.append(self.TEST_PLAN_SEL, thread_group)
        self.append(self.TEST_PLAN_SEL, thread_group_ht)
        self.append(self.TEST_PLAN_SEL, results_tree)
        self.append(self.TEST_PLAN_SEL, results_tree_ht)

    def save(self, filename):
        """
        Generate test plan and save

        :type filename: str
        """
        # NOTE: bad design, as repetitive save will duplicate stuff
        self.__generate()
        super(JMeterScenarioBuilder, self).save(filename)

    @staticmethod
    def __gen_authorization(scenario):
        """
        Generates HTTP Authorization Manager

        """
        elements = []
        authorizations = scenario.get("authorization")
        if authorizations:
            clear_flag = False

            if isinstance(authorizations, dict):
                if "clear" in authorizations or "list" in authorizations:  # full form
                    clear_flag = authorizations.get("clear", False)
                    authorizations = authorizations.get("list", [])
                else:
                    authorizations = [authorizations]  # short form

            if not isinstance(authorizations, list):
                raise TaurusConfigError("Wrong authorization format: %s" % authorizations)

            auth_manager = JMX.get_auth_manager(authorizations, clear_flag)
            elements.append(auth_manager)
            elements.append(etree.Element("hashTree"))

        return elements

    def __gen_data_sources(self, scenario):
        elements = []
        for source in scenario.get_data_sources():
            source_path = source["path"]
            delimiter = source.get("delimiter")

            if has_variable_pattern(source_path):
                msg = "Path to CSV contains JMeter variable/function, can't check for file existence: %s"
                self.log.warning(msg, source_path)
                if not delimiter:
                    delimiter = ','
                    self.log.warning("Can't detect CSV dialect, default delimiter will be '%s'", delimiter)
            else:
                source_path = self.executor.engine.find_file(source_path)
                if not os.path.isfile(source_path):
                    raise TaurusConfigError("data-sources path not found: %s" % source_path)
                if not delimiter:
                    delimiter = guess_delimiter(source_path)

            if source.get("random-order"):
                config = JMX._get_csv_config_random(source_path, delimiter, source.get("loop", True),
                                                    source.get("variable-names", ""))
            else:
                config = JMX._get_csv_config(source_path, delimiter, source.get("loop", True),
                                             source.get("variable-names", ""),  source.get("quoted", False))
            elements.append(config)
            elements.append(etree.Element("hashTree"))
        return elements

    def __gen_keystore_config(self, scenario):
        elements = []
        keystore_config = scenario.get(self.FIELD_KEYSTORE_CONFIG)
        if keystore_config:
            variable_name = keystore_config["variable-name"]
            start_index = keystore_config["start-index"]
            end_index = keystore_config["end-index"]
            preload = keystore_config["preload"]

            config = JMX.get_keystore_config_elements(variable_name, start_index, end_index, preload)
            elements.append(config)
            elements.append(etree.Element("hashTree"))
        return elements
