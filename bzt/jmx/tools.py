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
import traceback

from bzt import TaurusInternalException, TaurusConfigError
from bzt.jmx import JMX
from bzt.jmx.threadgroups import ThreadGroup, ConcurrencyThreadGroup, ThreadGroupHandler
from bzt.requests_model import has_variable_pattern, Request
from bzt.six import etree, numeric_types
from bzt.utils import BetterDict, dehumanize_time, ensure_is_dict, get_full_path, guess_csv_dialect, load_class


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


class ProtocolHandler(object):
    def __init__(self, scenario_builder, sys_props):
        """

        :type scenario_builder: JMeterScenarioBuilder
        """
        super(ProtocolHandler, self).__init__()
        self.scenario_builder = scenario_builder
        self.system_props = sys_props

    def get_toplevel_elements(self, scenario):
        pass

    def get_sampler_elements(self, scenario, request):
        pass

    def get_processor_elements(self, scenario, request):
        pass

    @staticmethod
    def safe_time(any_time):
        try:
            smart_time = int(1000 * dehumanize_time(any_time))
        except TaurusInternalException:
            smart_time = any_time

        return smart_time

    @staticmethod
    def _add_jsr_elements(children, req):
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


class JMeterScenarioBuilder(JMX):
    """
    Helper to build JMeter test plan from Scenario

    :type protocol_handlers: list[ProtocolHandler]
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
        self.protocol_handlers = []
        for cls_name in self.executor.settings.get("protocol-handlers", []):
            cls_obj = load_class(cls_name)
            instance = cls_obj(self, self.system_props)
            self.protocol_handlers.append(instance)

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

    def __gen_requests(self, scenario):
        requests = scenario.get_requests_new()
        return self.compile_requests(requests)

    def compile_scenario(self, scenario):
        elements = []
        for protocol in self.protocol_handlers:
            toplevels = protocol.get_toplevel_elements(scenario)
            if toplevels:
                elements.extend(toplevels)
        elements.extend(self.__gen_datasources(scenario))
        elements.extend(self.__gen_requests(scenario))
        self.log.info("Compiled scenario to %s", elements)
        return elements

    def compile_subrequests(self, subrequests):
        requests = []
        for key in range(len(subrequests)):  # pylint: disable=consider-using-enumerate
            req = ensure_is_dict(subrequests, key, "url")
            request_obj = Request(req, scenario=self.scenario)
            requests.append(request_obj)
        return self.compile_requests(requests)

    def compile_request(self, request):
        """

        :type request: HierarchicHTTPRequest
        :return:
        """
        elements = []
        for protocol in self.protocol_handlers:
            elems = protocol.get_sampler_elements(self.scenario, request)
            if elems:
                elements.extend(elems)
                break

        processors = []
        for protocol in self.protocol_handlers:
            procs = protocol.get_processor_elements(self.scenario, request)
            if procs:
                processors.extend(procs)
        elements.extend(processors)

        if not elements:
            self.log.warning("Problematic request: %s", request.config)
            raise TaurusInternalException("Unable to handle request, please review missing options")

        self.log.info("Compiled request %s into %s", request, elements)

        return elements

    def compile_requests(self, requests):
        compiled = []
        for request in requests:
            compiled.extend(self.compile_request(request))
        self.log.info("Compiled all requests into %s", compiled)
        return compiled

    def __generate(self):
        """
        Generate the test plan
        """

        thread_group = JMX.get_thread_group(testname=self.executor.label)
        thread_group_ht = etree.Element("hashTree", type="tg")

        # NOTE: set realistic dns-cache and JVM prop by default?
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
        sources = scenario.get("data-sources")
        if not sources:
            return []
        if not isinstance(sources, list):
            raise TaurusConfigError("data-sources '%s' is not a list" % sources)
        elements = []
        for idx, source in enumerate(sources):
            source = ensure_is_dict(sources, idx, "path")
            source_path = source["path"]

            delimiter = source.get("delimiter")

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
