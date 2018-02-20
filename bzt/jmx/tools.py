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
import json
import os
import traceback
from distutils.version import LooseVersion

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Scenario
from bzt.jmx import JMX
from bzt.requests_model import RequestVisitor, has_variable_pattern
from bzt.six import etree, iteritems, numeric_types
from bzt.utils import BetterDict, dehumanize_time, ensure_is_dict, get_host_ips, get_full_path, guess_csv_dialect


class RequestCompiler(RequestVisitor):
    def __init__(self, jmx_builder):
        super(RequestCompiler, self).__init__()
        self.jmx_builder = jmx_builder

    def visit_hierarchichttprequest(self, request):
        return self.jmx_builder.compile_http_request(request)

    def visit_ifblock(self, block):
        return self.jmx_builder.compile_if_block(block)

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


class AbstractThreadGroup(object):
    XPATH = None
    RAMP_UP_SEL = None
    CONCURRENCY_SEL = None

    def __init__(self, element, logger):
        self.element = element
        self.gtype = self.__class__.__name__
        self.log = logger.getChild(self.gtype)

    def get_testname(self):
        return self.element.get('testname')

    def set_concurrency(self, concurrency=None):
        self.log.warning('Setting of concurrency for %s not implemented', self.gtype)

    def set_ramp_up(self, ramp_up=None):
        self.log.warning('Setting of ramp-up for %s not implemented', self.gtype)

    def get_duration(self):
        """
        task duration or None if getting isn't possible (skipped, timeless, jmeter variables, etc.)
        """
        self.log.warning('Getting of duration for %s not implemented', self.gtype)

    def get_rate(self, pure=False):
        self.log.warning('Getting of rate for %s not implemented', self.gtype)

    def get_iterations(self):
        """
        iterations number or None if getting isn't possible (skipped, unsupported, jmeter variables, etc.)
        Note: ConcurrencyThreadGroup and ArrivalsThreadGroup aren't stopped by iterations limit
        """
        self.log.warning('Getting of iterations for %s not implemented', self.gtype)

    def get_ramp_up(self, pure=False):
        if not self.RAMP_UP_SEL:
            self.log.warning('Getting of ramp-up for %s not implemented', self.gtype)
            return 1

        return self._get_val(self.RAMP_UP_SEL, name='ramp-up', default=0, pure=pure)

    def get_concurrency(self, pure=False):
        if not self.CONCURRENCY_SEL:
            self.log.warning('Getting of concurrency for %s not implemented', self.gtype)
            return 1

        return self._get_val(self.CONCURRENCY_SEL, name='concurrency', default=1, pure=pure)

    def _get_val(self, selector, name='', default=None, convertor=int, pure=False):
        element = self.element.find(selector)
        if element is None:
            string_val = None
        else:
            string_val = element.text

        if pure:
            return string_val

        try:
            return convertor(string_val)
        except (ValueError, TypeError):
            if default:
                msg = "Parsing {param} '{val}' in group '{gtype}' failed, choose {default}"
                self.log.warning(msg.format(param=name, val=string_val, gtype=self.gtype, default=default))
                return default

    def get_on_error(self):
        action = self.element.find(".//stringProp[@name='ThreadGroup.on_sample_error']")
        if action is not None:
            return action.text


class ThreadGroup(AbstractThreadGroup):
    XPATH = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'
    CONCURRENCY_SEL = ".//*[@name='ThreadGroup.num_threads']"

    def get_duration(self):
        sched_sel = ".//*[@name='ThreadGroup.scheduler']"
        scheduler = self._get_val(sched_sel, "scheduler", pure=True)

        if scheduler == 'true':
            duration_sel = ".//*[@name='ThreadGroup.duration']"
            return self._get_val(duration_sel, "duration")
        elif scheduler == 'false':
            ramp_sel = ".//*[@name='ThreadGroup.ramp_time']"
            return self._get_val(ramp_sel, "ramp-up")
        else:
            msg = 'Getting of ramp-up for %s is impossible due to scheduler: %s'
            self.log.warning(msg, (self.gtype, scheduler))

    def get_iterations(self):
        loop_control_sel = ".//*[@name='LoopController.continue_forever']"
        loop_controller = self._get_val(loop_control_sel, name="loop controller", pure=True)
        if loop_controller == "false":
            loop_sel = ".//*[@name='LoopController.loops']"
            return self._get_val(loop_sel, name="loops")
        else:
            msg = 'Getting of ramp-up for %s is impossible due to loop_controller: %s'
            self.log.warning(msg, (self.gtype, loop_controller))


class SteppingThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.SteppingThreadGroup'
    CONCURRENCY_SEL = ".//*[@name='ThreadGroup.num_threads']"


class UltimateThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.UltimateThreadGroup'


# parent of ConcurrencyThreadGroup and ArrivalThreadGroup
class AbstractDynamicThreadGroup(AbstractThreadGroup):
    RAMP_UP_SEL = ".//*[@name='RampUp']"

    def _get_time_unit(self):
        unit_sel = ".//*[@name='Unit']"
        return self._get_val(unit_sel, name="unit", pure=True)

    def set_ramp_up(self, ramp_up=None):
        ramp_up_element = self.element.find(self.RAMP_UP_SEL)
        ramp_up_element.text = str(ramp_up)

    def get_duration(self):
        hold_sel = ".//*[@name='Hold']"

        hold = self._get_val(hold_sel, name="hold")
        ramp_up = self.get_ramp_up()

        # 'empty' means 0 sec, let's detect that
        p_hold = self._get_val(hold_sel, name="hold", pure=True)
        p_ramp_up = self.get_ramp_up(pure=True)
        if hold is None and not p_hold:
            hold = 0
        if ramp_up is None and not p_ramp_up:
            ramp_up = 0

        if hold is not None and ramp_up is not None:
            result = hold + ramp_up
            if self._get_time_unit() == 'M':
                result *= 60

            return result

    def get_iterations(self):
        iter_sel = ".//*[@name='Iterations']"
        return self._get_val(iter_sel, name="iterations")


class ConcurrencyThreadGroup(AbstractDynamicThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>com\.blazemeter\.jmeter\.threads\.concurrency\.ConcurrencyThreadGroup'
    CONCURRENCY_SEL = ".//*[@name='TargetLevel']"

    def set_concurrency(self, concurrency=None):
        concurrency_prop = self.element.find(self.CONCURRENCY_SEL)
        concurrency_prop.text = str(concurrency)


class ArrivalsThreadGroup(AbstractDynamicThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>com\.blazemeter\.jmeter\.threads\.arrivals\.ArrivalsThreadGroup'
    RATE_SEL = ".//*[@name='TargetLevel']"

    def get_rate(self, pure=False):
        return self._get_val(self.RATE_SEL, name='rate', default=1, pure=pure)

    def set_rate(self, rate=None):
        rate_prop = self.element.find(self.RATE_SEL)
        rate_prop.text = str(rate)


class ThreadGroupHandler(object):
    CLASSES = [ThreadGroup, SteppingThreadGroup, UltimateThreadGroup, ConcurrencyThreadGroup, ArrivalsThreadGroup]

    def __init__(self, logger):
        self.log = logger.getChild(self.__class__.__name__)

    def groups(self, jmx):
        """
        Get wrappers for thread groups that are enabled
        """
        for _class in self.CLASSES:
            for group in jmx.get(_class.XPATH):
                if group.get("enabled") != "false":
                    yield _class(group, self.log)

    def convert(self, group, target, load, concurrency):
        """
        Convert a thread group to ThreadGroup/ConcurrencyThreadGroup for applying of load
        """
        msg = "Converting %s (%s) to %s and apply load parameters"
        self.log.debug(msg, group.gtype, group.get_testname(), target)
        on_error = group.get_on_error()

        if target == ThreadGroup.__name__:
            new_group_element = JMX.get_thread_group(
                concurrency=concurrency,
                rampup=load.ramp_up,
                hold=load.hold,
                iterations=load.iterations,
                testname=group.get_testname(),
                on_error=on_error)
        elif target == ConcurrencyThreadGroup.__name__:
            new_group_element = JMX.get_concurrency_thread_group(
                concurrency=concurrency,
                rampup=load.ramp_up,
                hold=load.hold,
                steps=load.steps,
                testname=group.get_testname(),
                on_error=on_error)
        else:
            self.log.warning('Unsupported preferred thread group: %s', target)
            return

        group.element.getparent().replace(group.element, new_group_element)


class LoadSettingsProcessor(object):
    TG = ThreadGroup.__name__
    CTG = ConcurrencyThreadGroup.__name__

    def __init__(self, executor):
        self.log = executor.log.getChild(self.__class__.__name__)
        self.load = executor.get_specific_load()
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
        if not (self.load.iterations or self.load.concurrency or self.load.duration):
            self.log.debug('No iterations/concurrency/duration found, thread group modification is skipped')
            return

        # IMPORTANT: fix groups order as changing of element type changes order of getting of groups
        groups = list(self.tg_handler.groups(jmx))

        if self.load.concurrency and not isinstance(self.load.concurrency, numeric_types):  # property found
            for group in groups:
                self.tg_handler.convert(group=group, target=self.tg, load=self.load, concurrency=self.load.concurrency)
        else:
            target_list = zip(groups, self._get_concurrencies(groups))

            for group, concurrency in target_list:
                self.tg_handler.convert(group=group, target=self.tg, load=self.load, concurrency=concurrency)

        if self.load.throughput:
            self._add_shaper(jmx)

        if self.load.steps and self.tg == self.TG:
            self.log.warning("Stepping ramp-up isn't supported for regular ThreadGroup")

    def _get_concurrencies(self, groups):
        """
        Collect concurrency values and
        calculate target concurrency for every thread group
        """
        concurrency_list = []
        for group in groups:
            concurrency_list.append(group.get_concurrency())

        if concurrency_list and self.load.concurrency:
            total_old_concurrency = sum(concurrency_list)  # t_o_c != 0 because of logic of group.get_concurrency()

            for idx, concurrency in enumerate(concurrency_list):
                part_of_load = 1.0 * self.load.concurrency * concurrency / total_old_concurrency
                if part_of_load < 1:
                    concurrency_list[idx] = 1
                else:
                    concurrency_list[idx] = int(round(part_of_load))

            total_new_concurrency = sum(concurrency_list)
            leftover = self.load.concurrency - total_new_concurrency
            if leftover < 0:
                msg = "Had to add %s more threads to maintain thread group proportion"
                self.log.warning(msg, -leftover)
            elif leftover > 0:
                msg = "%s threads left undistributed due to thread group proportion"
                self.log.warning(msg, leftover)
        return concurrency_list

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
            jmx.add_rps_shaper_schedule(etree_shaper, 1, self.load.throughput, self.load.ramp_up)

        if self.load.hold:
            jmx.add_rps_shaper_schedule(etree_shaper, self.load.throughput, self.load.throughput, self.load.hold)

        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree_shaper)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))


class JMeterScenarioBuilder(JMX):
    """
    Helper to build JMeter test plan from Scenario

    :param executor: ScenarioExecutor
    :param original: inherited from JMX
    """

    def __init__(self, executor, original=None):
        super(JMeterScenarioBuilder, self).__init__(original)
        self.executor = executor
        self.scenario = executor.get_scenario()
        self.engine = executor.engine
        self.system_props = BetterDict()
        self.request_compiler = None

    def __gen_managers(self, scenario):
        elements = []
        if scenario.get("store-cache", True):
            elements.append(self._get_cache_mgr())
            elements.append(etree.Element("hashTree"))
        if scenario.get("store-cookie", True):
            elements.append(self._get_cookie_mgr(scenario))
            elements.append(etree.Element("hashTree"))
        if scenario.get("use-dns-cache-mgr", True):
            elements.append(self.get_dns_cache_mgr())
            elements.append(etree.Element("hashTree"))
            self.system_props.merge({"system-properties": {"sun.net.inetaddr.ttl": 0}})
        return elements

    @staticmethod
    def smart_time(any_time):  # FIXME: bad name for the function, does not reflect what it does
        try:
            smart_time = int(1000 * dehumanize_time(any_time))
        except TaurusInternalException:
            smart_time = any_time

        return smart_time

    def __gen_defaults(self, scenario):
        default_address = scenario.get("default-address", None)
        retrieve_resources = scenario.get("retrieve-resources", True)
        resources_regex = scenario.get("retrieve-resources-regex", None)
        concurrent_pool_size = scenario.get("concurrent-pool-size", 4)

        content_encoding = scenario.get("content-encoding", None)

        timeout = scenario.get("timeout", None)
        timeout = self.smart_time(timeout)
        elements = [self._get_http_defaults(default_address, timeout, retrieve_resources,
                                            concurrent_pool_size, content_encoding, resources_regex),
                    etree.Element("hashTree")]
        return elements

    def __add_think_time(self, children, req):
        think_time = req.priority_option('think-time')
        if think_time is not None:
            children.append(JMX._get_constant_timer(self.smart_time(think_time)))
            children.append(etree.Element("hashTree"))

    def __add_extractors(self, children, req):
        self.__add_regexp_ext(children, req)
        self.__add_json_ext(children, req)
        self.__add_jquery_ext(children, req)
        self.__add_xpath_ext(children, req)

    def __add_regexp_ext(self, children, req):
        extractors = req.config.get("extract-regexp", BetterDict())
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            extractor = JMX._get_extractor(varname, cfg.get('subject', 'body'), cfg['regexp'], cfg.get('template', 1),
                                           cfg.get('match-no', 1), cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_json_ext(self, children, req):
        jextractors = req.config.get("extract-jsonpath", BetterDict())
        for varname in jextractors:
            cfg = ensure_is_dict(jextractors, varname, "jsonpath")
            if LooseVersion(str(self.executor.settings["version"])) < LooseVersion("3.0"):
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
                                                             cfg.get("match-no", "-1"),
                                                             cfg.get("concat", False))

            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_jquery_ext(self, children, req):
        css_jquery_extors = req.config.get("extract-css-jquery", BetterDict())
        for varname in css_jquery_extors:
            cfg = ensure_is_dict(css_jquery_extors, varname, "expression")
            extractor = self._get_jquerycss_extractor(varname, cfg['expression'], cfg.get('attribute', ""),
                                                      cfg.get('match-no', 0), cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_xpath_ext(self, children, req):
        xpath_extractors = req.config.get("extract-xpath", BetterDict())
        for varname in xpath_extractors:
            cfg = ensure_is_dict(xpath_extractors, varname, "xpath")
            children.append(JMX._get_xpath_extractor(varname,
                                                     cfg['xpath'],
                                                     cfg.get('default', 'NOT_FOUND'),
                                                     cfg.get('validate-xml', False),
                                                     cfg.get('ignore-whitespace', True),
                                                     cfg.get('use-tolerant-parser', False)))
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
    def __add_jsr_elements(children, req):
        """
        :type children: etree.Element
        :type req: Request
        """
        jsrs = req.config.get("jsr223", [])
        if not isinstance(jsrs, list):
            jsrs = [jsrs]
        for idx, _ in enumerate(jsrs):
            jsr = ensure_is_dict(jsrs, idx, default_key='script-text')
            lang = jsr.get("language", "groovy")
            script_file = jsr.get("script-file", None)
            script_text = jsr.get("script-text", None)
            if not script_file and not script_text:
                raise TaurusConfigError("jsr223 element must specify one of 'script-file' or 'script-text'")
            parameters = jsr.get("parameters", "")
            execute = jsr.get("execute", "after")
            children.append(JMX._get_jsr223_element(lang, script_file, parameters, execute, script_text))
            children.append(etree.Element("hashTree"))

    def _get_merged_ci_headers(self, req, header):
        def dic_lower(dic):
            return {k.lower(): dic[k].lower() for k in dic}

        ci_scenario_headers = dic_lower(self.scenario.get_headers())
        ci_request_headers = dic_lower(req.headers)
        headers = BetterDict()
        headers.merge(ci_scenario_headers)
        headers.merge(ci_request_headers)
        if header.lower() in headers:
            return headers[header]
        else:
            return None

    def __gen_requests(self, scenario):
        requests = scenario.get_requests()
        elements = []
        for compiled in self.compile_requests(requests):
            elements.extend(compiled)
        return elements

    def compile_scenario(self, scenario):
        elements = []
        elements.extend(self.__gen_managers(scenario))
        elements.extend(self.__gen_defaults(scenario))
        elements.extend(self.__gen_datasources(scenario))
        elements.extend(self.__gen_requests(scenario))
        return elements

    def compile_http_request(self, request):
        """

        :type request: HierarchicHTTPRequest
        :return:
        """
        timeout = request.priority_option('timeout')
        if timeout is not None:
            timeout = self.smart_time(timeout)

        content_type = self._get_merged_ci_headers(request, 'content-type')
        if content_type == 'application/json' and isinstance(request.body, (dict, list)):
            body = json.dumps(request.body)
        else:
            body = request.body

        use_random_host_ip = request.priority_option('random-source-ip', default=False)
        host_ips = get_host_ips(filter_loopbacks=True) if use_random_host_ip else []
        http = JMX._get_http_request(request.url, request.label, request.method, timeout, body,
                                     request.priority_option('keepalive', default=True),
                                     request.upload_files, request.content_encoding,
                                     request.priority_option('follow-redirects', default=True),
                                     use_random_host_ip, host_ips)

        children = etree.Element("hashTree")

        if request.headers:
            children.append(JMX._get_header_mgr(request.headers))
            children.append(etree.Element("hashTree"))

        self.__add_think_time(children, request)

        self.__add_assertions(children, request)

        if timeout is not None:
            children.append(JMX._get_dur_assertion(timeout))
            children.append(etree.Element("hashTree"))

        self.__add_extractors(children, request)

        self.__add_jsr_elements(children, request)

        return [http, children]

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
        controller = JMX._get_transaction_controller(block.name, block.priority_option('force-parent-sample', True))
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

    def compile_set_variables_block(self, block):
        # pause current thread for 0s
        test_action = JMX._get_action_block(action_index=1, target_index=0, duration_ms=0)
        children = etree.Element("hashTree")
        fmt = "vars.put('%s', %r);"
        block.config["jsr223"] = [{
            "language": "groovy",
            "execute": "before",
            "script-text": "\n".join(fmt % (var, expr) for var, expr in iteritems(block.mapping))
        }]
        self.__add_jsr_elements(children, block)
        return [test_action, children]

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

    def __gen_datasources(self, scenario):
        sources = scenario.get("data-sources", [])
        if not sources:
            return []
        if not isinstance(sources, list):
            raise TaurusConfigError("data-sources '%s' is not a list" % sources)
        elements = []
        for idx, source in enumerate(sources):
            source = ensure_is_dict(sources, idx, "path")
            source_path = source["path"]

            delimiter = source.get('delimiter', None)

            if has_variable_pattern(source_path):
                msg = "Path to CSV contains JMeter variable/function, can't check for file existence: %s"
                self.log.warning(msg, source_path)
                if not delimiter:
                    delimiter = ','
                    self.log.warning("Can't detect CSV dialect, default delimiter will be '%s'", delimiter)
            else:
                modified_path = self.executor.engine.find_file(source_path)
                if not os.path.isfile(modified_path):
                    raise TaurusConfigError("data-sources path not found: %s" % modified_path)
                if not delimiter:
                    delimiter = self.__guess_delimiter(modified_path)
                source_path = get_full_path(modified_path)

            config = JMX._get_csv_config(source_path, delimiter, source.get("quoted", False), source.get("loop", True),
                                         source.get("variable-names", ""))
            elements.append(config)
            elements.append(etree.Element("hashTree"))
        return elements

    def __guess_delimiter(self, path):
        with open(path) as fhd:
            header = fhd.read(4096)  # 4KB is enough for header
            try:
                delimiter = guess_csv_dialect(header).delimiter
            except BaseException as exc:
                self.log.debug(traceback.format_exc())
                self.log.warning('CSV dialect detection failed (%s), default delimiter selected (",")', exc)
                delimiter = ","  # default value

        return delimiter
