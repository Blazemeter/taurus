"""
Module holds base stuff regarding JMX format

Copyright 2015 BlazeMeter Inc.

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
import os
import traceback

from cssselect import GenericTranslator
from lxml import etree
from urllib import parse

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Scenario
from bzt.utils import BetterDict, iteritems, numeric_types
from bzt.requests_model import has_variable_pattern

LOG = logging.getLogger("")


def try_convert(val, func=int, default=None):
    if val is None:
        res = val
    elif has_variable_pattern(val):  # it's property...
        if default is not None:
            val = get_prop_default(val) or default
            res = func(val)
        else:
            res = val
    else:
        res = func(val)

    return res


def get_prop_default(val):
    comma_ind = val.find(",")
    comma_found = comma_ind > -1
    is_property = val.startswith("${__property(") or val.startswith("${__P(")
    if has_variable_pattern(val) and is_property and comma_found:
        return val[comma_ind + 1: -2]
    else:
        return None


def cond_int(val):
    if isinstance(val, float):
        return int(val)

    return val


def cond_float(val, rounding=None):
    if isinstance(val, numeric_types):
        return round(float(val), rounding) if rounding is not None else float(val)

    return val


class JMX(object):
    """
    A class to manipulate and generate JMX test plans for JMeter

    :param original: path to existing JMX to load. If it is None, then creates
    empty test plan
    """
    TEST_PLAN_SEL = "jmeterTestPlan>hashTree>hashTree"
    THR_GROUP_SEL = TEST_PLAN_SEL + ">hashTree[type=tg]"
    THR_TIMER = "kg.apc.jmeter.timers.VariableThroughputTimer"
    SET_VAR_ACTION = "kg.apc.jmeter.control.sampler.SetVariablesAction"

    def __init__(self, original=None, test_plan_name="BZT Generated Test Plan"):
        self.log = logging.getLogger(self.__class__.__name__)
        if original:
            self.load(original)
        else:
            root = etree.Element("jmeterTestPlan")
            self.tree = etree.ElementTree(root)

            test_plan = etree.Element("TestPlan", guiclass="TestPlanGui",
                                      testname=test_plan_name,
                                      testclass="TestPlan", enabled="true")

            htree = etree.Element("hashTree")
            htree.append(test_plan)
            htree.append(etree.Element("hashTree"))
            self.append("jmeterTestPlan", htree)

            element_prop = self._get_arguments_panel("TestPlan.user_defined_variables")
            self.append("jmeterTestPlan>hashTree>TestPlan", element_prop)

    def load(self, original):
        """
        Load existing JMX file

        :param original: JMX file path
        :raise TaurusInternalException: in case of XML parsing error
        """
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(original)
        except BaseException as exc:
            msg = "XML parsing failed for file %s: %s"
            raise TaurusInternalException(msg % (original, exc))

    def get(self, selector):
        """
        Returns tree elements by CSS selector

        :type selector: str
        :return:
        """
        expression = GenericTranslator().css_to_xpath(selector)
        nodes = self.tree.xpath(expression)
        return nodes

    def append(self, selector, node):
        """
        Add node to container specified by selector. If multiple nodes will
        match the selector, first of them will be used as container.

        :param selector: CSS selector for container
        :param node: Element instance to add
        :raise TaurusInternalException: if container was not found
        """
        container = self.get(selector)
        if not len(container):
            msg = "Failed to find TestPlan node in file: %s"
            raise TaurusInternalException(msg % selector)

        container[0].append(node)

    def save(self, filename):
        """
        Save JMX into file

        :param filename:
        """
        self.log.debug("Saving JMX to: %s", filename)
        with open(filename, "wb") as fhd:
            self.tree.write(fhd, pretty_print=True, encoding="UTF-8", xml_declaration=True)

    @staticmethod
    def _flag(flag_name, bool_value):
        """
        Generates element for JMX flag node

        :param flag_name:
        :param bool_value:
        :return:
        """
        elm = etree.Element(flag_name)
        elm.text = "true" if bool_value else "false"
        return elm

    @staticmethod
    def __jtl_writer(filename, label, flags):
        """
        Generates JTL writer

        :param filename:
        :return:
        """
        jtl = etree.Element("stringProp", {"name": "filename"})
        jtl.text = filename

        name = etree.Element("name")
        name.text = "saveConfig"
        value = etree.Element("value")
        value.set("class", "SampleSaveConfiguration")

        for key, val in iteritems(flags):
            value.append(JMX._flag(key, val))
        obj_prop = etree.Element("objProp")
        obj_prop.append(name)
        obj_prop.append(value)

        listener = etree.Element("ResultCollector",
                                 testname=label,
                                 testclass="ResultCollector",
                                 guiclass="SimpleDataWriter")
        listener.append(jtl)
        listener.append(obj_prop)
        return listener

    @staticmethod
    def new_kpi_listener(filename, flag_overrides=None):
        """
        Generates listener for writing basic KPI data in CSV format

        :param filename:
        :return:
        """

        defaults = {
            "xml": False,
            "fieldNames": True,
            "time": True,
            "timestamp": True,
            "latency": True,
            "connectTime": True,
            "success": True,
            "label": True,
            "code": True,
            "message": True,
            "threadName": True,
            "dataType": False,
            "encoding": False,
            "assertions": False,
            "subresults": False,
            "responseData": False,
            "samplerData": False,
            "responseHeaders": False,
            "requestHeaders": False,
            "responseDataOnError": False,
            "saveAssertionResultsFailureMessage": False,
            "bytes": True,
            "hostname": True,
            "threadCounts": True,
            "url": False
        }

        flags = BetterDict.from_dict(defaults)
        if flag_overrides:
            flags.merge(flag_overrides)

        return JMX.__jtl_writer(filename, "KPI Writer", flags)

    @staticmethod
    def new_xml_listener(filename, is_full, user_flags):
        """

        :param is_full: bool
        :param filename: str
        :param user_flags: BetterDict
        :return:
        """
        default_flags = {
            "xml": True,
            "fieldNames": True,
            "time": True,
            "timestamp": True,
            "latency": True,
            "success": True,
            "label": True,
            "code": True,
            "message": True,
            "threadName": True,
            "dataType": True,
            "encoding": True,
            "assertions": True,
            "subresults": True,
            "responseData": False,
            "samplerData": False,
            "responseHeaders": True,
            "requestHeaders": True,
            "responseDataOnError": True,
            "saveAssertionResultsFailureMessage": True,
            "bytes": True,
            "threadCounts": True,
            "url": True
        }
        flags = BetterDict.from_dict(default_flags)
        flags.merge(user_flags)

        if is_full:
            writer = JMX.__jtl_writer(filename, "Trace Writer", flags)
        else:
            writer = JMX.__jtl_writer(filename, "Errors Writer", flags)
            writer.append(JMX._bool_prop("ResultCollector.error_logging", True))

        return writer

    @staticmethod
    def _get_arguments_panel(name):
        """
        Generates ArgumentsPanel node

        :param name:
        :return:
        """
        return etree.Element("elementProp", name=name, elementType="Arguments",
                             guiclass="ArgumentsPanel", testclass="Arguments")

    @staticmethod
    def get_auth_manager(authorizations, clear_flag):
        mgr = etree.Element("AuthManager", guiclass="AuthPanel", testclass="AuthManager",
                            testname="HTTP Authorization Manager")

        if clear_flag:
            mgr.append(JMX._bool_prop("AuthManager.clearEachIteration", True))

        auth_coll = JMX._collection_prop("AuthManager.auth_list")
        mgr.append(auth_coll)

        for authorization in authorizations:
            auth_element = JMX._element_prop(name="", element_type="Authorization")

            conf_url = authorization.get("url", "")
            conf_name = authorization.get("name", "")
            conf_pass = authorization.get("password", "")
            conf_domain = authorization.get("domain", "")
            conf_realm = authorization.get("realm", "")
            conf_mech = authorization.get("mechanism", "").upper()

            if not (conf_name and conf_pass and (conf_url or conf_domain)):
                LOG.warning("Wrong authorization: %s" % authorization)
                continue

            auth_element.append(JMX._string_prop("Authorization.url", conf_url))
            auth_element.append(JMX._string_prop("Authorization.username", conf_name))
            auth_element.append(JMX._string_prop("Authorization.password", conf_pass))
            auth_element.append(JMX._string_prop("Authorization.domain", conf_domain))
            auth_element.append(JMX._string_prop("Authorization.realm", conf_realm))

            if conf_mech == "KERBEROS":  # optional prop
                auth_element.append(JMX._string_prop("Authorization.mechanism", "KERBEROS"))

            auth_coll.append(auth_element)

        return mgr

    @staticmethod
    def _get_http_request(url, label, method, timeout, body, keepalive, files=(), encoding=None, follow_redirects=True,
                          use_random_host_ip=False, host_ips=()):
        """
        Generates HTTP request
        :type method: str
        :type label: str
        :type url: str
        :rtype: lxml.etree.Element
        """
        proxy = etree.Element("HTTPSamplerProxy", guiclass="HttpTestSampleGui", testclass="HTTPSamplerProxy")
        proxy.set("testname", label)

        args = JMX._get_arguments_panel("HTTPsampler.Arguments")

        if isinstance(body, str):
            JMX.__add_body_from_string(args, body, proxy)
        elif isinstance(body, dict):
            JMX.__add_body_from_script(args, body, proxy)
        elif body:
            msg = "Cannot handle 'body' option of type %s: %s"
            raise TaurusInternalException(msg % (type(body), body))

        parsed_url = parse.urlparse(url)
        JMX.__add_hostnameport_2sampler(parsed_url, proxy, url)

        path = parsed_url.path
        if parsed_url.params:
            path += ";" + parsed_url.params
        if parsed_url.query:
            path += "?" + parsed_url.query

        proxy.append(JMX._string_prop("HTTPSampler.path", path))
        proxy.append(JMX._string_prop("HTTPSampler.method", method))
        proxy.append(JMX._bool_prop("HTTPSampler.use_keepalive", keepalive))
        proxy.append(JMX._bool_prop("HTTPSampler.follow_redirects", follow_redirects))
        proxy.append(JMX._bool_prop("HTTPSampler.auto_redirects", False))

        if timeout is not None:
            proxy.append(JMX._string_prop("HTTPSampler.connect_timeout", timeout))
            proxy.append(JMX._string_prop("HTTPSampler.response_timeout", timeout))

        if encoding is not None:
            proxy.append(JMX._string_prop("HTTPSampler.contentEncoding", encoding))

        proxy.extend(JMX.get_files_elements(files))

        if use_random_host_ip and host_ips:
            if len(host_ips) > 1:
                expr = "${__chooseRandom(%s,randomAddr)}" % ",".join(host_ips)
            else:
                expr = host_ips[0]
            proxy.append(JMX._string_prop("HTTPSampler.ipSource", expr))

        return proxy

    @staticmethod
    def get_files_elements(files):
        elements = []
        if files:
            files_prop = JMX._element_prop("HTTPsampler.Files", "HTTPFileArgs")
            elements.append(files_prop)

            files_coll = JMX._collection_prop("HTTPFileArgs.files")
            for file_dict in files:
                file_elem = JMX._element_prop(file_dict.get("path", ""), "HTTPFileArg")
                file_elem.append(JMX._string_prop("File.path", file_dict.get("path", "")))
                file_elem.append(JMX._string_prop("File.paramname", file_dict.get("param", "")))
                file_elem.append(JMX._string_prop("File.mimetype", file_dict.get("mime-type", "")))
                files_coll.append(file_elem)
            files_prop.append(files_coll)

        return elements

    @staticmethod
    def get_keystore_config_elements(variable_name, start_index, end_index, preload):
        elements = []
        if variable_name:
            elements = etree.Element("KeystoreConfig", guiclass="TestBeanGUI", testclass="KeystoreConfig",
                                     testname="Taurus-Keystore-Configuration")
            elements.append(JMX._string_prop("clientCertAliasVarName", variable_name))
            elements.append(JMX._string_prop("startIndex", start_index))
            elements.append(JMX._string_prop("endIndex", end_index))
            elements.append(JMX._string_prop("preload", preload))

        return elements

    @staticmethod
    def __add_body_from_string(args, body, proxy):
        proxy.append(JMX._bool_prop("HTTPSampler.postBodyRaw", True))
        coll_prop = JMX._collection_prop("Arguments.arguments")
        header = JMX._element_prop("elementProp", "HTTPArgument")
        try:
            header.append(JMX._string_prop("Argument.value", body))
        except ValueError:
            LOG.warning("Failed to set body: %s", traceback.format_exc())
            header.append(JMX._string_prop("Argument.value", "BINARY-STUB"))
        coll_prop.append(header)
        args.append(coll_prop)
        proxy.append(args)

    @staticmethod
    def __add_body_from_script(args, body, proxy):
        http_args_coll_prop = JMX._collection_prop("Arguments.arguments")
        for arg_name, arg_value in body.items():
            try:
                http_element_prop = JMX._element_prop(arg_name, "HTTPArgument")
            except ValueError:
                LOG.warning("Failed to get element property: %s", traceback.format_exc())
                http_element_prop = JMX._element_prop('BINARY-STUB', "HTTPArgument")

            try:
                http_element_prop.append(JMX._string_prop("Argument.name", arg_name))
            except ValueError:
                LOG.warning("Failed to set arg name: %s", traceback.format_exc())
                http_element_prop.append(JMX._string_prop("Argument.name", "BINARY-STUB"))

            try:
                http_element_prop.append(
                    JMX._string_prop("Argument.value", arg_value if arg_value is not None else ''))
            except ValueError:
                LOG.warning("Failed to set arg name: %s", traceback.format_exc())
                http_element_prop.append(JMX._string_prop("Argument.value", "BINARY-STUB"))

            http_element_prop.append(JMX._bool_prop("HTTPArgument.always_encode", True))
            use_equals = arg_value is not None
            http_element_prop.append(JMX._bool_prop("HTTPArgument.use_equals", arg_value is not None))
            http_element_prop.append(JMX._string_prop("Argument.metadata", '=' if use_equals else ''))
            http_args_coll_prop.append(http_element_prop)
        args.append(http_args_coll_prop)
        proxy.append(args)

    @staticmethod
    def __add_hostnameport_2sampler(parsed_url, proxy, url):
        if parsed_url.scheme:
            proxy.append(JMX._string_prop("HTTPSampler.protocol", parsed_url.scheme))
        if parsed_url.netloc:
            netloc_parts = parsed_url.netloc.split(':')
            if netloc_parts[0]:
                proxy.append(JMX._string_prop("HTTPSampler.domain", netloc_parts[0]))

            if len(netloc_parts) > 1 and netloc_parts[1]:
                proxy.append(JMX._string_prop("HTTPSampler.port", netloc_parts[1]))
            else:
                try:
                    if parsed_url.port:
                        proxy.append(JMX._string_prop("HTTPSampler.port", parsed_url.port))
                    else:
                        proxy.append(JMX._string_prop("HTTPSampler.port", ""))
                except ValueError:
                    LOG.debug("Non-parsable port: %s", url)
                    proxy.append(JMX._string_prop("HTTPSampler.port", ""))

    @staticmethod
    def _element_prop(name, element_type):
        """
        Generates element property node

        :param name:
        :param element_type:
        :return:
        """
        res = etree.Element("elementProp", name=name, elementType=element_type)
        return res

    @staticmethod
    def _collection_prop(name):
        """
        Adds Collection prop
        :param name:
        :return:
        """
        res = etree.Element("collectionProp", name=name)
        return res

    @staticmethod
    def _string_prop(name, value):
        """
        Generates string property node

        :param name:
        :param value:
        :return:
        """
        res = etree.Element("stringProp", name=name)
        res.text = str(value)
        return res

    @staticmethod
    def _long_prop(name, value):
        """
        Generates long property node

        :param name:
        :param value:
        :return:
        """
        res = etree.Element("longProp", name=name)
        res.text = str(value)
        return res

    @staticmethod
    def _bool_prop(name, value):
        """
        Generates boolean property

        :param name:
        :param value:
        :return:
        """
        res = etree.Element("boolProp", name=name)
        res.text = 'true' if value else 'false'
        return res

    @staticmethod
    def int_prop(name, value):
        """
        JMX int property
        :param name:
        :param value:
        :return:
        """
        res = etree.Element("intProp", name=name)
        res.text = str(value)
        return res

    @staticmethod
    def get_thread_group(concurrency=None, rampup=0, hold=0, iterations=None,
                         testname="ThreadGroup", on_error="continue", thread_delay=False, scheduler_delay=None):
        """
        Generates ThreadGroup

        Expected values (by JMeter):
            ThreadGroup.num_threads (concurrency): int
            ThreadGroup.ramp_time (rampup): int
            ThreadGroup.scheduler (need to hold): boolean
            ThreadGroup.duration (rampup + hold): int
            LoopController.loops (iterations): int
            ThreadGroup.delayedStart: boolean
        :return: etree element, ThreadGroup
        """
        rampup = cond_int(rampup or 0)
        hold = cond_int(hold or 0)

        if concurrency is None:
            concurrency = 1

        if isinstance(concurrency, numeric_types) and concurrency <= 0:
            enabled = "false"
        else:
            enabled = "true"

        if not hold:
            duration = rampup
        elif not rampup:
            duration = hold
        elif isinstance(rampup, numeric_types) and isinstance(hold, numeric_types):
            duration = hold + rampup
        else:
            duration = "${__intSum(%s,%s)}" % (rampup, hold)

        trg = etree.Element("ThreadGroup", guiclass="ThreadGroupGui",
                            testclass="ThreadGroup", testname=testname, enabled=enabled)

        if not iterations:
            if duration:
                iterations = -1
            else:
                iterations = 1

        scheduler = False
        if hold or (rampup and (iterations == -1)):
            scheduler = True

        if on_error is not None:
            trg.append(JMX._string_prop("ThreadGroup.on_sample_error", on_error))
        loop = etree.Element("elementProp",
                             name="ThreadGroup.main_controller",
                             elementType="LoopController",
                             guiclass="LoopControlPanel",
                             testclass="LoopController")

        # 'true' causes endless execution of TG in non-gui mode
        loop.append(JMX._bool_prop("LoopController.continue_forever", False))
        loop.append(JMX._string_prop("LoopController.loops", iterations))
        trg.append(loop)

        trg.append(JMX._string_prop("ThreadGroup.num_threads", concurrency))
        trg.append(JMX._string_prop("ThreadGroup.ramp_time", rampup))
        trg.append(JMX._string_prop("ThreadGroup.start_time", ""))
        trg.append(JMX._string_prop("ThreadGroup.end_time", ""))
        trg.append(JMX._bool_prop("ThreadGroup.scheduler", scheduler))
        trg.append(JMX._string_prop("ThreadGroup.duration", duration))

        if scheduler_delay:
            trg.append(JMX._string_prop("ThreadGroup.delay", scheduler_delay))

        if thread_delay:
            trg.append(JMX._bool_prop("ThreadGroup.delayedStart", thread_delay))

        return trg

    def get_rps_shaper(self):
        """

        :return: etree.Element
        """

        throughput_timer_element = etree.Element(self.THR_TIMER,
                                                 guiclass=self.THR_TIMER + "Gui",
                                                 testclass=self.THR_TIMER,
                                                 testname="Throughput_Limiter",
                                                 enabled="true")
        shaper_load_prof = self._collection_prop("load_profile")
        throughput_timer_element.append(shaper_load_prof)

        return throughput_timer_element

    def add_rps_shaper_schedule(self, shaper_etree, start_rps, end_rps, duration):
        """
        Adds schedule to rps shaper

        Expected values (by JMeter):
            <first> ('start_rps'): float
            <second> ('end_rps'): float
            <third> ('duration'): int
        """
        shaper_collection = shaper_etree.find(".//collectionProp[@name='load_profile']")
        coll_prop = self._collection_prop("")
        start_rps_prop = self._string_prop("", cond_float(start_rps, 3))
        end_rps_prop = self._string_prop("", cond_float(end_rps, 3))
        duration_prop = self._string_prop("", cond_int(duration))
        coll_prop.append(start_rps_prop)
        coll_prop.append(end_rps_prop)
        coll_prop.append(duration_prop)
        shaper_collection.append(coll_prop)

    @staticmethod
    def get_set_var_action(udv_dict, testname="Variables from Taurus"):
        """
        :type testname: str
        :type udv_dict: dict[str,str]
        :rtype: etree.Element
        """

        udv_element = etree.Element(JMX.SET_VAR_ACTION, guiclass=JMX.SET_VAR_ACTION + "Gui",
                                    testclass=JMX.SET_VAR_ACTION, testname=testname)
        arg_element = etree.Element("elementProp", name="SetVariablesAction", guiclass="ArgumentsPanel",
                                    testclass="Arguments", testname="User Defined Variables", elementType="Arguments")
        udv_element.append(arg_element)
        udv_collection_prop = JMX._collection_prop("Arguments.arguments")
        arg_element.append(udv_collection_prop)

        for var_name in sorted(udv_dict.keys(), key=str):
            udv_element_prop = JMX._element_prop(name=str(var_name), element_type="Argument")
            udv_collection_prop.append(udv_element_prop)

            udv_arg_name_prop = JMX._string_prop("Argument.name", var_name)
            udv_arg_value_prop = JMX._string_prop("Argument.value", udv_dict[var_name])
            udv_arg_meta_prop = JMX._string_prop("Argument.metadata", "=")
            udv_element_prop.append(udv_arg_name_prop)
            udv_element_prop.append(udv_arg_value_prop)
            udv_element_prop.append(udv_arg_meta_prop)

        return udv_element

    @staticmethod
    def add_user_def_vars_elements(udv_dict, testname="Variables from Taurus"):
        """
        :type testname: str
        :type udv_dict: dict[str,str]
        :rtype: etree.Element
        """

        udv_element = etree.Element("Arguments", guiclass="ArgumentsPanel", testclass="Arguments",
                                    testname=testname)
        udv_collection_prop = JMX._collection_prop("Arguments.arguments")

        for var_name in sorted(udv_dict.keys(), key=str):
            udv_element_prop = JMX._element_prop(str(var_name), "Argument")
            udv_arg_name_prop = JMX._string_prop("Argument.name", var_name)
            udv_arg_value_prop = JMX._string_prop("Argument.value", udv_dict[var_name])
            udv_arg_desc_prop = JMX._string_prop("Argument.desc", "")
            udv_arg_meta_prop = JMX._string_prop("Argument.metadata", "=")
            udv_element_prop.append(udv_arg_name_prop)
            udv_element_prop.append(udv_arg_value_prop)
            udv_element_prop.append(udv_arg_desc_prop)
            udv_element_prop.append(udv_arg_meta_prop)
            udv_collection_prop.append(udv_element_prop)

        udv_element.append(udv_collection_prop)
        return udv_element

    @staticmethod
    def get_concurrency_thread_group(concurrency=None, rampup=0, hold=0, steps=None, on_error="continue",
                                     testname="ConcurrencyThreadGroup", iterations=""):
        """
        Generates ConcurrencyThreadGroup

        Expected values (by JMeter):
            Targetlevel (concurrency): int
            RampUp (rampup): float
            Steps (steps): boolean
            Hold (hold): float

        :return: etree element, Concurrency Thread Group
        """
        if not rampup:
            rampup = 0

        if concurrency is None:
            concurrency = 1

        if isinstance(concurrency, numeric_types) and concurrency <= 0:
            enabled = "false"
        else:
            enabled = "true"

        if steps is None:  # zero means infinity of steps
            steps = 0

        name = 'com.blazemeter.jmeter.threads.concurrency.ConcurrencyThreadGroup'
        concurrency_thread_group = etree.Element(
            name, guiclass=name + "Gui", testclass=name, testname=testname, enabled=enabled)
        virtual_user_controller = etree.Element(
            "elementProp",
            name="ThreadGroup.main_controller",
            elementType="com.blazemeter.jmeter.control.VirtualUserController")
        concurrency_thread_group.append(virtual_user_controller)
        concurrency_thread_group.append(JMX._string_prop("ThreadGroup.on_sample_error", on_error))
        concurrency_thread_group.append(JMX._string_prop("TargetLevel", str(concurrency)))
        concurrency_thread_group.append(JMX._string_prop("RampUp", str(cond_int(rampup))))
        concurrency_thread_group.append(JMX._string_prop("Steps", steps))
        concurrency_thread_group.append(JMX._string_prop("Hold", str(cond_int(hold))))
        concurrency_thread_group.append(JMX._string_prop("LogFilename", ""))
        concurrency_thread_group.append(JMX._string_prop("Iterations", iterations or ""))
        concurrency_thread_group.append(JMX._string_prop("Unit", "S"))

        return concurrency_thread_group

    @staticmethod
    def get_dns_cache_mgr():
        """
        Adds dns cache element with defaults parameters

        :return:
        """
        dns_element = etree.Element("DNSCacheManager", guiclass="DNSCachePanel", testclass="DNSCacheManager",
                                    testname="DNS Cache Manager")
        dns_element.append(JMX._collection_prop("DNSCacheManager.servers"))
        dns_element.append(JMX._bool_prop("DNSCacheManager.clearEachIteration", False))
        dns_element.append(JMX._bool_prop("DNSCacheManager.isCustomResolver", False))
        return dns_element

    @staticmethod
    def _get_header_mgr(hdict):
        """

        :type hdict: dict[str,str]
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("HeaderManager", guiclass="HeaderPanel", testclass="HeaderManager", testname="Headers")

        coll_prop = etree.Element("collectionProp", name="HeaderManager.headers")
        for hname, hval in iteritems(hdict):
            header = etree.Element("elementProp", name="", elementType="Header")
            header.append(JMX._string_prop("Header.name", hname))
            header.append(JMX._string_prop("Header.value", hval))
            coll_prop.append(header)
        mgr.append(coll_prop)
        return mgr

    @staticmethod
    def _get_cache_mgr():
        """
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("CacheManager", guiclass="CacheManagerGui", testclass="CacheManager", testname="Cache")
        mgr.append(JMX._bool_prop("clearEachIteration", True))
        mgr.append(JMX._bool_prop("useExpires", True))
        return mgr

    @staticmethod
    def _get_cookie_mgr(scenario=None):
        """
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("CookieManager", guiclass="CookiePanel", testclass="CookieManager", testname="Cookies")
        mgr.append(JMX._bool_prop("CookieManager.clearEachIteration", False))
        mgr.append(JMX._string_prop("CookieManager.implementation",
                                    "org.apache.jmeter.protocol.http.control.HC4CookieHandler"))

        if scenario:
            cookies = scenario.get(Scenario.COOKIES)
            if cookies:
                cookies_coll = JMX._collection_prop("CookieManager.cookies")
                mgr.append(cookies_coll)
                for cookie in cookies:
                    if not isinstance(cookie, dict):
                        raise TaurusConfigError("Cookie must be dictionary: %s" % cookie)
                    c_name = cookie.get("name", TaurusConfigError("Name of cookie isn't found: %s" % cookie))
                    c_value = cookie.get("value", TaurusConfigError("Value of cookie isn't found: %s" % cookie))
                    c_domain = cookie.get("domain", TaurusConfigError("Domain of cookie isn't found: %s" % cookie))
                    c_path = cookie.get("path", "")
                    c_secure = cookie.get("secure", False)

                    # follow params are hardcoded in JMeter
                    c_expires = 0
                    c_path_specified = True
                    c_domain_specified = True

                    c_elem = etree.Element("elementProp", name=c_name, elementType="Cookie", testname=c_name)
                    c_elem.append(JMX._string_prop("Cookie.value", c_value))
                    c_elem.append(JMX._string_prop("Cookie.domain", c_domain))
                    c_elem.append(JMX._string_prop("Cookie.path", c_path))
                    c_elem.append(JMX._bool_prop("Cookie.secure", c_secure))
                    c_elem.append(JMX._long_prop("Cookie.expires", c_expires))
                    c_elem.append(JMX._bool_prop("Cookie.path_specified", c_path_specified))
                    c_elem.append(JMX._bool_prop("Cookie.domain_specified", c_domain_specified))

                    cookies_coll.append(c_elem)

        return mgr

    @staticmethod
    def _get_http_defaults(default_address=None, timeout=None, retrieve_resources=None, concurrent_pool_size=4,
                           content_encoding=None, resources_regex=None):
        """
        :rtype: lxml.etree.Element
        """
        cfg = etree.Element("ConfigTestElement", guiclass="HttpDefaultsGui",
                            testclass="ConfigTestElement", testname="Defaults")

        if retrieve_resources:
            cfg.append(JMX._bool_prop("HTTPSampler.image_parser", True))
            cfg.append(JMX._bool_prop("HTTPSampler.concurrentDwn", True))
            if concurrent_pool_size:
                cfg.append(JMX._string_prop("HTTPSampler.concurrentPool", concurrent_pool_size))

        params = etree.Element("elementProp",
                               name="HTTPsampler.Arguments",
                               elementType="Arguments",
                               guiclass="HTTPArgumentsPanel",
                               testclass="Arguments", testname="user_defined")
        cfg.append(params)
        if default_address:
            parsed_url = parse.urlsplit(default_address)
            if parsed_url.scheme:
                cfg.append(JMX._string_prop("HTTPSampler.protocol", parsed_url.scheme))

            if parsed_url.netloc:
                netloc = parsed_url.netloc
                if ':' in netloc:
                    index = netloc.rfind(':')
                    cfg.append(JMX._string_prop("HTTPSampler.port", netloc[index + 1:]))
                    netloc = netloc[:index]

                cfg.append(JMX._string_prop("HTTPSampler.domain", netloc))

        if timeout:
            cfg.append(JMX._string_prop("HTTPSampler.connect_timeout", timeout))
            cfg.append(JMX._string_prop("HTTPSampler.response_timeout", timeout))

        if content_encoding:
            cfg.append(JMX._string_prop("HTTPSampler.contentEncoding", content_encoding))

        if resources_regex:
            cfg.append(JMX._string_prop("HTTPSampler.embedded_url_re", resources_regex))

        return cfg

    @staticmethod
    def _get_dur_assertion(timeout):
        """

        :type timeout: int
        :return:
        """
        element = etree.Element("DurationAssertion", guiclass="DurationAssertionGui",
                                testclass="DurationAssertion", testname="Timeout Check")
        element.append(JMX._string_prop("DurationAssertion.duration", timeout))
        return element

    @staticmethod
    def get_constant_timer(delay):
        timer_type = "ConstantTimer"
        element = etree.Element(timer_type, guiclass="%sGui" % timer_type, testclass=timer_type, testname="Think-Time")
        element.append(JMX._string_prop("%s.delay" % timer_type, delay))
        return [element, etree.Element("hashTree")]

    @staticmethod
    def get_uniform_timer(maximum, offset):
        timer_type = "UniformRandomTimer"
        element = etree.Element(timer_type, guiclass="%sGui" % timer_type, testclass=timer_type, testname="Think-Time")
        element.append(JMX._string_prop("ConstantTimer.delay", offset))
        element.append(JMX._string_prop("RandomTimer.range", maximum))
        return [element, etree.Element("hashTree")]

    @staticmethod
    def get_gaussian_timer(dev, offset):
        timer_type = "GaussianRandomTimer"
        element = etree.Element(timer_type, guiclass="%sGui" % timer_type, testclass=timer_type, testname="Think-Time")
        element.append(JMX._string_prop("ConstantTimer.delay", offset))
        element.append(JMX._string_prop("RandomTimer.range", dev))
        return [element, etree.Element("hashTree")]

    @staticmethod
    def get_poisson_timer(lam, delay):
        timer_type = "PoissonRandomTimer"
        element = etree.Element(timer_type, guiclass="%sGui" % timer_type, testclass=timer_type, testname="Think-Time")
        element.append(JMX._string_prop("ConstantTimer.delay", delay))
        element.append(JMX._string_prop("RandomTimer.range", lam))
        return [element, etree.Element("hashTree")]

    @staticmethod
    def _get_extractor(varname, headers, regexp, template, match_no, default='NOT_FOUND', scope='', from_var=''):
        """
        :type varname: str
        :type regexp: str
        :type template: str|int
        :type match_no: int
        :type default: str
        :type scope: str
        :type from_var: str
        :rtype: lxml.etree.Element
        """
        if isinstance(template, int):
            template = '$%s$' % template

        if headers.lower() == 'headers':
            headers = 'true'
        elif headers.lower() == 'http-code':
            headers = 'code'
        elif headers.lower() == 'url':
            headers = 'URL'
        else:
            headers = 'body'

        element = etree.Element("RegexExtractor", guiclass="RegexExtractorGui",
                                testclass="RegexExtractor", testname="Get %s" % varname, enabled="true")
        element.append(JMX._string_prop("RegexExtractor.useHeaders", headers))
        element.append(JMX._string_prop("RegexExtractor.refname", varname))
        element.append(JMX._string_prop("RegexExtractor.regex", regexp))
        element.append(JMX._string_prop("RegexExtractor.template", template))
        element.append(JMX._string_prop("RegexExtractor.default", default))
        element.append(JMX._string_prop("RegexExtractor.match_number", match_no))
        element.extend(JMX.get_scope_props(scope, from_var))
        return element

    @staticmethod
    def _get_boundary_extractor(varname, subject, left, right, match_no, defvalue='NOT_FOUND', scope='', from_var=''):
        """
        :type varname: str
        :type regexp: str
        :type template: str|int
        :type match_no: int
        :type default: str
        :type scope: str
        :type from_var: str
        :rtype: lxml.etree.Element
        """

        subjects = {
            'body': 'false',
            'body-unescaped': 'unescaped',
            'body-as-document': 'as_document',
            'response-headers': 'true',
            'request-headers': 'request_headers',
            'url': 'URL',
            'code': 'code',
            'message': 'message',
        }

        subject = subjects.get(subject)
        element = etree.Element("BoundaryExtractor", guiclass="BoundaryExtractorGui",
                                testclass="BoundaryExtractor", testname="Get %s" % varname, enabled="true")
        element.append(JMX._string_prop("BoundaryExtractor.useHeaders", subject))
        element.append(JMX._string_prop("BoundaryExtractor.refname", varname))
        element.append(JMX._string_prop("BoundaryExtractor.lboundary", left))  # TODO: html-escape boundaries?
        element.append(JMX._string_prop("BoundaryExtractor.rboundary", right))
        element.append(JMX._string_prop("RegexExtractor.default", defvalue))
        element.append(JMX._string_prop("RegexExtractor.match_number", match_no))
        element.extend(JMX.get_scope_props(scope, from_var))
        return element

    @staticmethod
    def _get_jquerycss_extractor(varname, selector, attribute, match_no, default="NOT_FOUND", scope='', from_var=''):
        """
        :type varname: str
        :type regexp: str
        :type match_no: int
        :type default: str
        :type scope: str
        :type from_var: str
        :rtype: lxml.etree.Element
        """

        element = etree.Element("HtmlExtractor", guiclass="HtmlExtractorGui", testclass="HtmlExtractor",
                                testname="Get %s" % varname)
        element.append(JMX._string_prop("HtmlExtractor.refname", varname))
        element.append(JMX._string_prop("HtmlExtractor.expr", selector))
        element.append(JMX._string_prop("HtmlExtractor.attribute", attribute))
        element.append(JMX._string_prop("HtmlExtractor.match_number", match_no))
        element.append(JMX._string_prop("HtmlExtractor.default", default))
        element.extend(JMX.get_scope_props(scope, from_var))
        return element

    @staticmethod
    def _get_json_extractor(varname, jsonpath, default='NOT_FOUND', from_variable=None):
        """
        :type varname: str
        :type default: str
        :rtype: lxml.etree.Element
        """
        package = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor"
        element = etree.Element("%s.JSONPathExtractor" % package,
                                guiclass="%s.gui.JSONPathExtractorGui" % package,
                                testclass="%s.JSONPathExtractor" % package,
                                testname="Get %s" % varname)
        element.append(JMX._string_prop("VAR", varname))
        element.append(JMX._string_prop("JSONPATH", jsonpath))
        element.append(JMX._string_prop("DEFAULT", default))
        if from_variable:
            element.append(JMX._string_prop("VARIABLE", from_variable))
            element.append(JMX._string_prop("SUBJECT", "VAR"))
        return element

    @staticmethod
    def get_scope_props(scope, from_variable):
        props = []
        if scope:
            props.append(JMX._string_prop("Sample.scope", scope))
            if scope == "variable":
                props.append(JMX._string_prop("Scope.variable", from_variable))
        return props

    @staticmethod
    def _get_internal_json_extractor(varname, jsonpath, default, scope, from_variable, match_no, concat):
        """
        :type varname: str
        :type default: str
        :rtype: lxml.etree.Element
        """
        package = "JSONPostProcessor"
        element = etree.Element(package,
                                guiclass="%sGui" % package,
                                testclass="%s" % package,
                                testname="Get %s" % varname)
        element.append(JMX._string_prop("JSONPostProcessor.referenceNames", varname))
        element.append(JMX._string_prop("JSONPostProcessor.jsonPathExprs", jsonpath))
        element.append(JMX._string_prop("JSONPostProcessor.match_numbers", match_no))

        if default:
            element.append(JMX._string_prop("JSONPostProcessor.defaultValues", default))

        element.extend(JMX.get_scope_props(scope, from_variable))

        if concat:
            element.append(JMX._bool_prop("JSONPostProcessor.compute_concat", True))

        return element

    @staticmethod
    def _get_json_path_assertion(jsonpath, expected_value, json_validation, expect_null, invert, regexp=True):
        """
        :type jsonpath: str
        :type expected_value: str
        :type json_validation: bool
        :type expect_null: bool
        :type invert: bool
        :type regexp: bool
        :return: lxml.etree.Element
        """
        package = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion"
        element = etree.Element("%s.JSONPathAssertion" % package,
                                guiclass="%s.gui.JSONPathAssertionGui" % package,
                                testclass="%s.JSONPathAssertion" % package,
                                testname="JSon path assertion")
        element.append(JMX._string_prop("JSON_PATH", jsonpath))
        element.append(JMX._string_prop("EXPECTED_VALUE", expected_value))
        element.append(JMX._bool_prop("JSONVALIDATION", json_validation))
        element.append(JMX._bool_prop("EXPECT_NULL", expect_null))
        element.append(JMX._bool_prop("INVERT", invert))
        element.append(JMX._bool_prop("ISREGEX", regexp))

        return element

    @staticmethod
    def _get_xpath_extractor(varname, xpath, default, validate_xml, ignore_whitespace, match_no, use_namespaces,
                             use_tolerant_parser, scope, from_var):
        """
        :type varname: str
        :type xpath: str
        :type default: str
        :type validate_xml: bool
        :type ignore_whitespace: bool
        :type use_tolerant_parser: bool
        :type scope: str
        :type from_var: str
        :rtype: lxml.etree.Element
        """
        element = etree.Element("XPathExtractor",
                                guiclass="XPathExtractorGui",
                                testclass="XPathExtractor",
                                testname="Get %s" % varname)
        element.append(JMX._string_prop("XPathExtractor.refname", varname))
        element.append(JMX._string_prop("XPathExtractor.xpathQuery", xpath))
        element.append(JMX._string_prop("XPathExtractor.default", default))
        element.append(JMX._bool_prop("XPathExtractor.validate", validate_xml))
        element.append(JMX._bool_prop("XPathExtractor.whitespace", ignore_whitespace))
        element.append(JMX._string_prop("XPathExtractor.matchNumber", match_no))
        element.append(JMX._bool_prop("XPathExtractor.namespace", use_namespaces))
        element.append(JMX._bool_prop("XPathExtractor.tolerant", use_tolerant_parser))

        element.extend(JMX.get_scope_props(scope, from_var))

        return element

    @staticmethod
    def _get_xpath_assertion(xpath, validate_xml, ignore_whitespace, use_tolerant_parser, invert):
        """
        :type xpath: str
        :type validate_xml: bool
        :type ignore_whitespace: bool
        :type use_tolerant_parser: bool
        :return: lxml.etree.Element
        """
        element = etree.Element("XPathAssertion",
                                guiclass="XPathAssertionGui",
                                testclass="XPathAssertion",
                                testname="XPath Assertion")

        element.append(JMX._string_prop("XPath.xpath", xpath))
        element.append(JMX._bool_prop("XPath.validate", validate_xml))
        element.append(JMX._bool_prop("XPath.whitespace", ignore_whitespace))
        element.append(JMX._bool_prop("XPath.tolerant", use_tolerant_parser))
        element.append(JMX._bool_prop("XPath.negate", invert))

        return element

    @staticmethod
    def _get_resp_assertion(field, contains, is_regexp, is_invert, assume_success=False):
        """

        :type field: str
        :type contains: list[str]
        :type is_regexp: bool
        :type is_invert:  bool
        :rtype: lxml.etree.Element
        """
        tname = "Assert %s %s" % ("hasn't" if is_invert else "has",
                                  "[" + ", ".join('"' + str(x) + '"' for x in contains) + "]")
        element = etree.Element("ResponseAssertion", guiclass="AssertionGui",
                                testclass="ResponseAssertion", testname=tname)
        if field == Scenario.FIELD_HEADERS:
            fld = "Assertion.response_headers"
        elif field == Scenario.FIELD_RESP_CODE:
            fld = "Assertion.response_code"
        else:
            fld = "Assertion.response_data"

        if is_regexp:
            if is_invert:
                mtype = 6  # not contains
            else:
                mtype = 2  # contains
        else:
            if is_invert:
                mtype = 20  # not substring
            else:
                mtype = 16  # substring

        element.append(JMX._string_prop("Assertion.test_field", fld))
        element.append(JMX._string_prop("Assertion.test_type", mtype))
        element.append(JMX._bool_prop("Assertion.assume_success", assume_success))

        coll_prop = etree.Element("collectionProp", name="Asserion.test_strings")
        for string in contains:
            coll_prop.append(JMX._string_prop("", string))
        element.append(coll_prop)

        return element

    @staticmethod
    def _get_jsr223_element(language, script_file, parameters, execute, script_text=None, cache_key='true'):
        if execute == "before":
            proc = "JSR223PreProcessor"
        else:
            proc = "JSR223PostProcessor"

        element = etree.Element(proc, guiclass="TestBeanGUI", testclass=proc, testname=proc)

        element.append(JMX._string_prop("filename", script_file if script_file else ''))
        element.append(JMX._string_prop("script", script_text if script_text else ''))
        element.append(JMX._string_prop("parameters", parameters))
        element.append(JMX._string_prop("scriptLanguage", language))
        element.append(JMX._string_prop("cacheKey", cache_key))

        return element

    @staticmethod
    def _get_csv_config(path, delimiter, loop, variable_names, is_quoted):
        """

        :type path: str
        :type delimiter: str
        :type is_quoted: bool
        :type loop: bool
        :type variable_names: string
        :return:
        """
        element = etree.Element("CSVDataSet", guiclass="TestBeanGUI",
                                testclass="CSVDataSet", testname="CSV %s" % os.path.basename(path))
        element.append(JMX._string_prop("filename", path))
        element.append(JMX._string_prop("delimiter", delimiter))
        element.append(JMX._bool_prop("quotedData", is_quoted))
        element.append(JMX._bool_prop("recycle", loop))
        element.append(JMX._bool_prop("stopThread", not loop))
        element.append(JMX._string_prop("variableNames", variable_names))

        return element

    @staticmethod
    def _get_csv_config_random(path, delimiter, loop, variable_names):
        """

        :type path: str
        :type delimiter: str
        :type loop: bool
        :type variable_names: string
        :return:
        """
        element = etree.Element("com.blazemeter.jmeter.RandomCSVDataSetConfig",
                                guiclass="com.blazemeter.jmeter.RandomCSVDataSetConfigGui",
                                testclass="com.blazemeter.jmeter.RandomCSVDataSetConfig",
                                testname="bzm - Random CSV Data Set Config")
        element.append(JMX._string_prop("filename", path))
        element.append(JMX._string_prop("fileEncoding", "UTF-8"))
        element.append(JMX._string_prop("delimiter", delimiter))
        element.append(JMX._string_prop("variableNames", variable_names))
        element.append(JMX._bool_prop("randomOrder", True))
        element.append(JMX._bool_prop("ignoreFirstLine", False if variable_names else True))
        element.append(JMX._bool_prop("rewindOnTheEndOfList", loop))
        element.append(JMX._bool_prop("independentListPerThread", False))

        return element

    def set_enabled(self, sel, state):
        """
        Toggle items by selector

        :type sel: str
        :type state: bool
        """
        items = self.get(sel)
        self.log.debug("Enable %s elements %s: %s", state, sel, items)
        for item in items:
            item.set("enabled", 'true' if state else 'false')

    def set_text(self, sel, text):
        """
        Set text value

        :type sel: str
        :type text: str
        """
        items = self.get(sel)
        res = 0
        for item in items:
            item.text = str(text)
            res += 1

        return res

    @staticmethod
    def _get_simple_controller(name):
        return etree.Element("GenericController", guiclass="LogicControllerGui", testclass="GenericController",
                             testname=name)

    def _add_results_tree(self):
        dbg_tree = etree.Element("ResultCollector",
                                 testname="View Results Tree",
                                 testclass="ResultCollector",
                                 guiclass="ViewResultsFullVisualizer")
        self.append(self.TEST_PLAN_SEL, dbg_tree)
        self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))

    @staticmethod
    def _get_results_tree():
        dbg_tree = etree.Element("ResultCollector",
                                 testname="View Results Tree",
                                 testclass="ResultCollector",
                                 guiclass="ViewResultsFullVisualizer")
        return dbg_tree

    @staticmethod
    def _get_if_controller(condition):
        controller = etree.Element("IfController", guiclass="IfControllerPanel", testclass="IfController",
                                   testname="If Controller")
        controller.append(JMX._string_prop("IfController.condition", condition))
        return controller

    @staticmethod
    def _get_once_controller():
        """
        Generates Once Only Controller

        :return: etree element, OnceOnlyController
        """
        controller = etree.Element("OnceOnlyController", guiclass="OnceOnlyControllerGui",
                                   testclass="OnceOnlyController", testname="Once Only Controller")

        return controller

    @staticmethod
    def _get_loop_controller(loops):
        """
        Generates Loop Controller

        Expected values(by JMeter):
            LoopController.loops(iterations): int
            LoopController.continue_forever: boolean

        :return: etree element, LoopController
        """
        if loops == 'forever':
            iterations = -1
        else:
            iterations = loops
        controller = etree.Element("LoopController", guiclass="LoopControlPanel", testclass="LoopController",
                                   testname="Loop Controller")

        # 'false' means controller can be called only one time (by parent)
        controller.append(JMX._bool_prop("LoopController.continue_forever", True))
        controller.append(JMX._string_prop("LoopController.loops", str(iterations)))
        return controller

    @staticmethod
    def _get_foreach_controller(input_var, loop_var):
        # TODO: useSeparator option
        controller = etree.Element("ForeachController", guiclass="ForeachControlPanel", testclass="ForeachController",
                                   testname="ForEach Controller")
        controller.append(JMX._string_prop("ForeachController.inputVal", input_var))
        controller.append(JMX._string_prop("ForeachController.returnVal", loop_var))
        controller.append(JMX._bool_prop("ForeachController.useSeparator", True))
        return controller

    @staticmethod
    def _get_while_controller(condition):
        controller = etree.Element("WhileController", guiclass="WhileControllerGui", testclass="WhileController",
                                   testname="While Controller")
        controller.append(JMX._string_prop("WhileController.condition", condition))
        return controller

    @staticmethod
    def _get_transaction_controller(transaction_name, force_parent_sample=False, include_timers=False):
        controller = etree.Element("TransactionController", guiclass="TransactionControllerGui",
                                   testclass="TransactionController", testname=transaction_name)
        controller.append(JMX._bool_prop("TransactionController.parent", force_parent_sample))
        controller.append(JMX._bool_prop("TransactionController.includeTimers", include_timers))
        return controller

    @staticmethod
    def _get_functional_mode_prop(enabled):
        return JMX._bool_prop("TestPlan.functional_mode", enabled)

    @staticmethod
    def _get_action_block(action_index, target_index, duration_ms):
        action = etree.Element("TestAction", guiclass="TestActionGui", testclass="TestAction", testname="Test Action")
        action.append(JMX.int_prop("ActionProcessor.action", action_index))
        action.append(JMX.int_prop("ActionProcessor.target", target_index))
        action.append(JMX._string_prop("ActionProcessor.duration", str(duration_ms)))
        return action
