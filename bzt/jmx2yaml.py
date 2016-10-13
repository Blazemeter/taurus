#! /usr/bin/env python
"""
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
import itertools
import logging
import os
import sys
import traceback
from collections import namedtuple
from copy import deepcopy
from optparse import OptionParser

from cssselect import GenericTranslator

from bzt.cli import CLI
from bzt.engine import Configuration, ScenarioExecutor
from bzt.jmx import JMX
from bzt.utils import get_full_path

KNOWN_TAGS = ["hashTree", "jmeterTestPlan", "TestPlan", "ResultCollector",
              "HTTPSamplerProxy",
              "ThreadGroup",
              "kg.apc.jmeter.timers.VariableThroughputTimer",
              "kg.apc.jmeter.threads.SteppingThreadGroup",
              "DNSCacheManager",
              "HeaderManager",
              "CacheManager",
              "CookieManager",
              "ConfigTestElement",
              "DurationAssertion",
              "ConstantTimer",
              "RegexExtractor",
              "HtmlExtractor",
              "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor.JSONPathExtractor",
              "com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion.JSONPathAssertion",
              "XPathAssertion",
              "XPathExtractor",
              "ResponseAssertion",
              "CSVDataSet",
              "GenericController",
              "ResultCollector",
              "Arguments",
              "IfController",
              "LoopController",
              "WhileController",
              "ForeachController",
              "TransactionController",
              "JSR223PreProcessor",
              "JSR223PostProcessor",
              ]


class JMXasDict(JMX):
    """
    Model
    """

    def __init__(self, log):
        super(JMXasDict, self).__init__()
        self.log = log.getChild(self.__class__.__name__)
        self.global_objects = []
        self.scenario = {ScenarioExecutor.EXEC: None, "scenarios": None}
        self.additional_files = {}  # dict(filename -> file content)

    def load(self, original):
        super(JMXasDict, self).load(original)
        self._clean_disabled_elements(self.tree)
        self._clean_jmx_tree(self.tree)
        self._get_global_objects()

    def _get_bool_prop(self, element, prop_name):
        """
        Gets bool prop from element
        :param element:
        :param prop_name:
        :return:
        """
        prop_element = element.find(".//boolProp[@name='" + prop_name + "']")
        if prop_element is not None and prop_element.text:
            if prop_element.text.lower() == 'true':
                return True
            elif prop_element.text.lower() == 'false':
                return False
        else:
            self.log.debug("boolProp %s was not found in %s element!", prop_name, element.tag)
            return None

    def _get_string_prop(self, element, prop_name, default=None):
        """
        Gets string prop from element
        :param element:
        :param prop_name:
        :return:
        """
        prop_element = element.find(".//stringProp[@name='" + prop_name + "']")
        if prop_element is not None and prop_element.text:
            return prop_element.text
        else:
            self.log.debug("stringProp %s was not found in %s element!", prop_name, element.tag)
            return default

    def _get_concurrency(self, element):
        """
        concurrency option in tg execution settings
        :return:
        """
        concurrency = self._get_option_string_with_default(element, 'ThreadGroup.num_threads', "concurrency", 1)
        self.log.debug('Got %s for concurrency in %s (%s)', concurrency, element.tag, element.get("testname"))
        return concurrency

    def _get_ramp_up(self, element):
        """
        ramp_up option in tg settings
        :param element:
        :return:
        """
        ramp_up = self._get_option_string_with_default(element, 'ThreadGroup.ramp_time', "ramp-up", 1)
        self.log.debug('Got %s for rampup in %s (%s)', ramp_up, element.tag, element.get("testname"))
        return ramp_up

    def _get_iterations(self, element):
        """
        iteration option in tg execution settings
        :param element:
        :return:
        """
        controller_element = element.find('.//elementProp')
        iterations = self._get_option_string_with_default(controller_element, 'LoopController.loops', "iterations", 1)
        self.log.debug('Got %s for iterations in %s (%s)', iterations, element.tag, element.get("testname"))
        return iterations

    def _get_duration(self, tg_element):
        """
        get hold_for and ramp_up from tg element.
        :param tg_element:
        :return:
        """
        result = {}
        ramp_up = self._get_ramp_up(tg_element)
        if self._get_bool_prop(tg_element, 'ThreadGroup.scheduler'):
            duration_element = tg_element.find("*[@name='ThreadGroup.duration']")
            if duration_element is not None and duration_element.text and duration_element.text.isdigit():
                duration = int(duration_element.text)
                if ramp_up:
                    result.update({"hold-for": str(duration - ramp_up["ramp-up"]) + "s"})
                else:
                    result.update({"hold-for": str(duration) + "s"})
        if ramp_up:
            ramp_up["ramp-up"] = str(ramp_up["ramp-up"]) + "s"
            result.update(ramp_up)
        self.log.debug('Got %s for duration in %s (%s)', result, tg_element.tag, tg_element.get("testname"))
        return result

    def _get_throughput(self, element):
        """
        Gets throughput from variable throughput timer
        :param element:
        :return:
        """
        result = {}
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            property_pattern = "kg.apc.jmeter.timers.VariableThroughputTimer"
            timer_elements = [element for element in hashtree.iterchildren() if element.tag == property_pattern]
            if timer_elements:
                load_profile = timer_elements[0].find(".//collectionProp[@name='load_profile']")
                if load_profile is not None:
                    col_props = load_profile.findall(".//collectionProp")
                    if col_props:
                        col_prop = col_props[-1]
                        st_prop = col_prop.findall(".//stringProp")
                        throughput = st_prop[-2].text
                        if throughput and throughput.isdigit():
                            result["throughput"] = int(throughput)
                            self.log.debug('Got %s for throughput in %s (%s)', throughput, element.tag,
                                           element.get("testname"))
        return result

    def _get_option_string_with_default(self, element, prop_name, opt_name, default):
        """
        :param element:
        :return: dict
        """
        result = {}
        if element is not None:
            prop_value = self._get_string_prop(element, prop_name)
            if prop_value and prop_value.isdigit() and int(prop_value) != default:
                result[opt_name] = int(prop_value)
        return result

    def _get_request_body(self, element):
        """
        Get body params from sampler
        :param element:
        :return: dict
        """

        raw_body = self._get_bool_prop(element, 'HTTPSampler.postBodyRaw')
        if raw_body:
            query = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp>elementProp'
            xpath = GenericTranslator().css_to_xpath(query)
            http_args_element = element.xpath(xpath)[0]
            body = self._get_string_prop(http_args_element, 'Argument.value')
            if body:
                self.log.debug('Got %s for body in %s (%s)', body, element.tag, element.get("name"))
                return {"body": body}
            else:
                return {}
        else:
            body_params = {}
            query = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp>elementProp'
            xpath = GenericTranslator().css_to_xpath(query)
            http_args_collection = element.xpath(xpath)
            for element in http_args_collection:
                val = self._get_string_prop(element, 'Argument.value')
                if val is None and self._get_bool_prop(element, 'HTTPArgument.use_equals'):
                    val = ''
                body_params[element.get("name")] = val

            if body_params:
                self.log.debug('Got %s for body in %s (%s)', body_params, element.tag, element.get("name"))
                return {"body": body_params}
            else:
                return {}

    def _get_upload_files(self, element):
        """
        Extract upload files from element
        :param element:
        :return: dict
        """
        query = 'elementProp[name="HTTPsampler.Files"]>collectionProp'
        xpath = GenericTranslator().css_to_xpath(query)
        colls = element.xpath(xpath)
        if not colls:
            return {}

        upload_files = []
        for elem in colls[0]:
            path = self._get_string_prop(elem, 'File.path')
            param = self._get_string_prop(elem, 'File.paramname')
            mime = self._get_string_prop(elem, 'File.mimetype')
            if path is None or param is None:
                continue
            upload_files.append({'param': param, 'path': path, 'mime-type': mime})

        return {'upload-files': upload_files}

    def _get_headers(self, element):
        """
        Get local request headers
        :return:
        """
        headers = {}
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            headers_elements = [element for element in hashtree.iterchildren() if element.tag == "HeaderManager"]
            for headers_element in headers_elements:
                if headers_element is not None:
                    for header in headers_element.find(".//collectionProp").findall(".//elementProp"):
                        header_name = self._get_string_prop(header, 'Header.name')
                        header_value = self._get_string_prop(header, 'Header.value')
                        if header_name and header_value:
                            headers[header_name] = header_value
        if headers:
            self.log.debug('Got %s for headers in %s (%s)', headers, element.tag, element.get("testname"))
            return {"headers": headers}
        else:
            return headers

    def _get_store_cache(self, element):
        """
        store-cache option
        :param element:
        :return:
        """
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            cache_managers = [element for element in hashtree.iterchildren() if element.tag == "CacheManager"]
            if cache_managers:
                self.log.debug('Got %s for cache_managers in %s (%s)', True, element.tag, element.get("testname"))
                return {"store-cache": True}
        return {}

    def _get_store_cookie(self, element):
        """
        store-cookie option
        :param element:
        :return:
        """
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            cookie_managers = [element for element in hashtree.iterchildren() if element.tag == "CookieManager"]
            if cookie_managers:
                self.log.debug('Got %s for cookie_managers in %s (%s)', True, element.tag, element.get("testname"))
                return {"store-cookie": True}
        return {}

    def _get_dns_mgr(self, element):
        """
        use-dns-cache-mgr option
        :param element:
        :return:
        """
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            dns_managers = [element for element in hashtree.iterchildren() if element.tag == "DNSCacheManager"]
            if dns_managers:
                self.log.debug('Got %s for dns_managers in %s (%s)', True, element.tag, element.get("testname"))
                return {"use-dns-cache-mgr": True}
        return {}

    def __get_constant_timer(self, element):
        """
        think-time option
        :param element:
        :return:
        """
        timer = {}
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            timer_element = [element for element in hashtree.iterchildren() if element.tag == "ConstantTimer"]
            if timer_element:
                timer_delay = self._get_string_prop(timer_element[0], 'ConstantTimer.delay')
                if timer_delay:
                    timer = {"think-time": timer_delay + "ms"}
                    self.log.debug('Got %s for timer in %s (%s)', timer_delay, element.tag, element.get("testname"))
        return timer

    def _get_http_request_defaults(self, element):
        """
        timeout
        default-address
        keepalive
        retrieve-resources
        concurrent-pool-size
        :param element:
        :return:
        """
        request_defaults = {}
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            http_defaults = [element for element in hashtree.iterchildren() if element.tag == "ConfigTestElement"]
            if http_defaults:
                http_defaults = http_defaults[0]
                url_info = self._extract_url_info(http_defaults)
                if url_info:
                    default_address = self._make_url(url_info)
                    if default_address:
                        request_defaults["default-address"] = default_address
                if url_info.timeout:
                    request_defaults["timeout"] = url_info.timeout + "ms"
                if url_info.retrieve_resources is not None:
                    request_defaults["retrieve-resources"] = url_info.retrieve_resources
                    if url_info.retrieve_resources:
                        if url_info.retrieve_concurrency and url_info.retrieve_concurrency.isdigit():
                            request_defaults["concurrent-pool-size"] = int(url_info.retrieve_concurrency)
        self.log.debug('Got %s for request-defaults in %s (%s)', request_defaults, element.tag, element.get("testname"))
        return request_defaults

    @staticmethod
    def _make_url(url_info):
        """
        :type url_info: urllib.ParseResults
        :return: string
        """
        path = "/" if not url_info.path else url_info.path
        port = "" if not url_info.port or url_info.port == "80" else ":" + url_info.port
        protocol = "http" if not url_info.protocol else url_info.protocol
        if url_info.domain:
            return protocol + "://" + url_info.domain + port + path
        return ""

    def _extract_url_info(self, element):
        """
        extracts domain, port, etc from element
        :return:
        """
        http_sampler_info = namedtuple("http_sampler_info",
                                       ["domain", "port", "timeout", "protocol", "path", "method", "retrieve_resources",
                                        "retrieve_concurrency"])
        if element is not None:
            domain = self._get_string_prop(element, 'HTTPSampler.domain')
            port = self._get_string_prop(element, 'HTTPSampler.port')
            timeout = self._get_string_prop(element, 'HTTPSampler.connect_timeout')
            protocol = self._get_string_prop(element, 'HTTPSampler.protocol')
            path = self._get_string_prop(element, 'HTTPSampler.path')
            retrieve_resources = self._get_bool_prop(element, 'HTTPSampler.image_parser')
            retrieve_concurrency = self._get_string_prop(element, 'HTTPSampler.concurrentPool')
            method = self._get_string_prop(element, 'HTTPSampler.method')
            url_info = http_sampler_info(domain, port, timeout, protocol, path, method, retrieve_resources,
                                         retrieve_concurrency)
            return url_info
        return None

    def _get_request_base(self, element):
        """
        Base request settings
        :return:
        """
        base_settings = {}
        url_info = self._extract_url_info(element)
        if url_info is not None:
            full_url = self._make_url(url_info)
            base_settings["url"] = full_url
            if url_info.method:
                base_settings["method"] = url_info.method
        if element.get("testname"):
            base_settings["label"] = element.get("testname")
        self.log.debug('Got %s for base settings in %s (%s)', base_settings, element.tag, element.get("testname"))
        return base_settings

    def _get_data_sources(self, element):
        """
        data-sources option
        :param element:
        :return: list of dicts
        """
        data_sources = []
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            data_sources_elements = [element for element in hashtree.iterchildren() if element.tag == "CSVDataSet"]
            for data_source in data_sources_elements:
                self.log.debug("datasource file: %s", data_source.get("testname"))
                if data_source is not None:
                    data_source_dict = {}
                    f_name_prop = self._get_string_prop(data_source, 'filename')
                    if f_name_prop:
                        data_source_dict["path"] = f_name_prop
                    else:
                        self.log.warning("File name was not set in %s, skipping", data_source.tag)
                        continue
                    delimiter_prop = self._get_string_prop(data_source, 'delimiter')
                    if delimiter_prop:
                        data_source_dict["delimiter"] = delimiter_prop
                    else:
                        self.log.warning("Delimiter was not set in %s, using default - ','", data_source.tag)
                        data_source_dict["delimiter"] = ","
                    quoted_prop = self._get_bool_prop(data_source, 'quotedData')
                    if quoted_prop is not None:
                        data_source_dict["quoted"] = quoted_prop
                    else:
                        self.log.warning("Quoted property was not set in %s, using default False", data_source.tag)
                        data_source_dict["quoted"] = False

                    loop_prop = self._get_bool_prop(data_source, 'recycle')
                    if loop_prop is None:
                        self.log.warning("Loop property was not set in %s, using default False", data_source.tag)
                        loop_prop = False

                    stop_prop = self._get_bool_prop(data_source, 'stopThread')
                    if stop_prop is None:
                        self.log.warning("'Stop Thread on EOF' property was not set in %s, using default False",
                                         data_source.tag)
                        stop_prop = False

                    if loop_prop:
                        data_source_dict["loop"] = True
                    elif stop_prop:
                        data_source_dict["loop"] = False

                    data_sources.append(data_source_dict)
        if data_sources:
            self.log.debug('Got %s for data_sources in %s (%s)', data_sources, element.tag, element.get("testname"))
            return {"data-sources": data_sources}
        else:
            return {}

    def _get_request_timeout(self, element):
        timeout = {}
        timeout_prop = self._get_string_prop(element, 'HTTPSampler.connect_timeout')
        if timeout_prop:
            self.log.debug('Got %s for request timeout in %s (%s)', timeout_prop, element.tag, element.get("testname"))
            timeout = {"timeout": timeout_prop + "ms"}
        return timeout

    def _get_extractors(self, element):
        """
        Gets xpath, jsonpath and regexp extractors
        :param element:
        :return:
        """
        extractors = {}
        regexp_extractors = self._get_regexp_extractor(element)
        if regexp_extractors:
            extractors.update({"extract-regexp": regexp_extractors})
        jsonpath_extractors = self._get_json_path_extractors(element)
        if jsonpath_extractors:
            extractors.update({"extract-jsonpath": jsonpath_extractors})
        xpath_extractors = self._get_xpath_extractors(element)
        if xpath_extractors:
            extractors.update({"extract-xpath": xpath_extractors})
        self.log.debug('Got %s for extractors in %s (%s)', extractors, element.tag, element.get("testname"))
        return extractors

    def _get_regexp_extractor(self, element):
        """
        extract-regexp option
        :param element:
        :return:
        """
        regexp_extractors = {}
        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            extractor_elements = [element for element in hashtree.iterchildren() if element.tag == "RegexExtractor"]
            for extractor_element in extractor_elements:
                regexp_extractor = {}
                if extractor_element is not None:
                    refname = self._get_string_prop(extractor_element, 'RegexExtractor.refname')

                    if refname:
                        extractor_props = {}
                        regexp_prop = self._get_string_prop(extractor_element, 'RegexExtractor.regex')
                        if regexp_prop:
                            extractor_props["regexp"] = regexp_prop
                        else:
                            self.log.warning("No regexp expression found in %s, skipping", extractor_element.tag)
                            continue

                        default_prop = self._get_string_prop(extractor_element, 'RegexExtractor.default')

                        if default_prop:
                            extractor_props["default"] = default_prop
                        else:
                            self.log.warning("No default value found in %s", extractor_element.tag)
                            extractor_props["default"] = ""

                        match_no_prop = self._get_string_prop(extractor_element, 'RegexExtractor.match_number')

                        if match_no_prop and match_no_prop.isdigit():
                            extractor_props["match-no"] = int(match_no_prop)
                        else:
                            self.log.warning("No match number found in %s, using 0 as default", extractor_element.tag)
                            extractor_props["match-no"] = 0

                        template_prop = self._get_string_prop(extractor_element, 'RegexExtractor.template')

                        if template_prop:
                            extractor_props["template"] = template_prop
                        else:
                            self.log.warning("No template property found in %s, using $0$ as default",
                                             extractor_element.tag)
                            extractor_props["template"] = '$0$'

                        regexp_extractor.update({refname: extractor_props})
                    else:
                        self.log.warning("refname property element not found in %s skipping", extractor_element.tag)
                        continue
                regexp_extractors.update(regexp_extractor)

        return regexp_extractors

    def _get_json_path_extractors(self, element):
        """
        extract-jsonpath option
        :param element:
        :return:
        """

        json_path_extractors = {}

        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            property_pattern = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor.JSONPathExtractor"
            extractor_elements = [element for element in hashtree.iterchildren() if element.tag == property_pattern]
            for extractor_element in extractor_elements:
                json_path_extractor = {}
                if extractor_element is not None:
                    varname = self._get_string_prop(extractor_element, 'VAR')
                    if varname:
                        extractor_props = {}
                        jsonpath_prop = self._get_string_prop(extractor_element, 'JSONPATH')

                        if jsonpath_prop:
                            extractor_props["jsonpath"] = jsonpath_prop
                        else:
                            self.log.warning("No json expression found in %s, skipping element", extractor_element.tag)
                            continue

                        default_prop = self._get_string_prop(extractor_element, 'DEFAULT')

                        if default_prop:
                            extractor_props["default"] = default_prop
                        else:
                            self.log.warning("No default value found in %s", extractor_element.tag)
                            extractor_props["default"] = ""

                        json_path_extractor.update({varname: extractor_props})

                    else:
                        self.log.warning("Not found varname in %s, skipping", extractor_element.tag)
                        continue

                json_path_extractors.update(json_path_extractor)

        return json_path_extractors

    def _get_xpath_extractors(self, element):
        """
        extract-xpath option
        :param element:
        :return:
        """

        extractors = {}

        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            elements = [element for element in hashtree.iterchildren() if element.tag == "XPathExtractor"]
            for element in elements:
                extractor = {}
                if element is not None:
                    varname = self._get_string_prop(element, 'XPathExtractor.refname')
                    if varname:
                        props = {}
                        xpath = self._get_string_prop(element, 'XPathExtractor.xpathQuery')

                        if xpath:
                            props["xpath"] = xpath
                        else:
                            self.log.warning("No xpath query found in %s, skipping element", element.tag)
                            continue

                        default = self._get_string_prop(element, 'XPathExtractor.default')

                        if default:
                            props["default"] = default
                        else:
                            self.log.warning("No default value found in %s", element.tag)
                            props["default"] = ""

                        validate = self._get_bool_prop(element, 'XPathExtractor.validate')
                        props["validate-xml"] = validate if validate else False
                        whitespace = self._get_bool_prop(element, 'XPathExtractor.whitespace')
                        props["ignore-whitespace"] = whitespace if whitespace else False
                        tolerant = self._get_bool_prop(element, 'XPathExtractor.tolerant')
                        props["use-tolerant-parser"] = tolerant if tolerant else False

                        extractor.update({varname: props})

                    else:
                        self.log.warning("Not found varname in %s, skipping", element.tag)
                        continue

                extractors.update(extractor)

        return extractors

    def _get_assertions(self, element):
        """
        assertions:
        assert, assert-jsonpath, assert-xpath
        :param element:
        :return:
        """
        assertions = {}
        simple_assertions = self._get_response_assertions(element)
        if simple_assertions:
            assertions.update({"assert": simple_assertions})
        jsonpath_assertions = self._get_jsonpath_assertions(element)
        if jsonpath_assertions:
            assertions.update({"assert-jsonpath": jsonpath_assertions})
        xpath_assertions = self._get_xpath_assertions(element)
        if xpath_assertions:
            assertions.update({"assert-xpath": xpath_assertions})
        self.log.debug('Got %s for assertions in %s (%s)', assertions, element.tag, element.get("testname"))
        return assertions

    def _get_response_assertions(self, element):
        """
        :param element:
        :return: list of dicts
        """
        response_assertions = []
        subjects = {"Assertion.response_data": "body", "Assertion.response_headers": "headers",
                    "Assertion.response_code": "http-code"}
        test_types = {'6': (True, True), '2': (True, False), '20': (False, True),
                      '16': (False, False)}  # (is_regexp, is_inverted)
        hashtree = element.getnext()

        if hashtree is not None and hashtree.tag == "hashTree":
            response_assertion_elements = [element for element in hashtree.iterchildren() if
                                           element.tag == "ResponseAssertion"]

            for response_assertion_element in response_assertion_elements:
                response_assertion = {}
                selector = ".//collectionProp[@name='Asserion.test_strings']"
                assertion_collection = response_assertion_element.find(selector)

                if assertion_collection is None:
                    self.log.warning("Collection not found in %s, skipping", response_assertion_element.tag)
                    continue
                test_string_props = assertion_collection.findall(".//stringProp")
                test_strings = []
                for string_prop in test_string_props:
                    if string_prop is not None and string_prop.text:
                        test_strings.append(string_prop.text)

                if not test_strings:
                    self.log.warning("No test strings in %s, skipping", response_assertion_element.tag)
                    continue
                response_assertion["contains"] = test_strings
                test_field_prop = self._get_string_prop(response_assertion_element, 'Assertion.test_field')

                if test_field_prop:
                    test_subject = subjects.get(test_field_prop, "body")
                else:
                    self.log.warning("No test subject provided in %s, skipping", response_assertion_element.tag)
                    continue
                response_assertion["subject"] = test_subject
                test_type_element = response_assertion_element.find(".//*[@name='Assertion.test_type']")

                if test_type_element is not None and test_type_element.text:
                    test_type = test_types.get(test_type_element.text)
                    if test_type:
                        is_regexp, is_inverted = test_type
                        response_assertion["regexp"] = is_regexp
                        response_assertion["not"] = is_inverted
                    else:
                        self.log.warning("Unknown test type in %s, skipping", response_assertion_element.tag)
                        continue
                else:
                    self.log.warning("No test subject provided in %s, skipping", response_assertion_element.tag)
                    continue
                response_assertions.append(response_assertion)
        return response_assertions

    def _get_jsonpath_assertions(self, element):
        """
        assert-jsonpath option
        :param element:
        :return: list of dicts
        """
        json_path_assertions = []
        hashtree = element.getnext()

        if hashtree is not None and hashtree.tag == "hashTree":
            pattern = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion.JSONPathAssertion"
            json_path_assertion_elements = [element for element in hashtree.iterchildren() if element.tag == pattern]
            for json_path_assertion_element in json_path_assertion_elements:
                json_path_assertion = {}
                json_path_element = self._get_string_prop(json_path_assertion_element, 'JSON_PATH')

                if json_path_element:
                    json_path_assertion["jsonpath"] = json_path_element
                else:
                    self.log.warning("No json path in %s, skipping", json_path_assertion_element.tag)
                    continue

                expected_value_element = self._get_string_prop(json_path_assertion_element, 'EXPECTED_VALUE')

                if expected_value_element:
                    json_path_assertion["expected-value"] = expected_value_element

                validate_element = self._get_bool_prop(json_path_assertion_element, 'JSONVALIDATION')
                json_path_assertion["validate"] = validate_element if validate_element else False
                expect_null_element = self._get_bool_prop(json_path_assertion_element, 'EXPECT_NULL')
                json_path_assertion["expect-null"] = expect_null_element if expect_null_element else False
                invert_elem = self._get_bool_prop(json_path_assertion_element, 'INVERT')
                json_path_assertion["invert"] = invert_elem if invert_elem else False
                json_path_assertions.append(json_path_assertion)

        return json_path_assertions

    def _get_xpath_assertions(self, element):
        """
        assert-xpath option
        :param element:
        :return: list of dicts
        """
        assertions = []
        hashtree = element.getnext()

        if hashtree is not None and hashtree.tag == "hashTree":
            assertion_elements = [element for element in hashtree.iterchildren() if element.tag == "XPathAssertion"]
            for assertion_element in assertion_elements:
                assertion = {}
                xpath_element = self._get_string_prop(assertion_element, 'XPath.xpath')

                if xpath_element:
                    assertion["xpath"] = xpath_element
                else:
                    self.log.warning("No xpath in %s, skipping", assertion_element.tag)
                    continue

                validate = self._get_bool_prop(assertion_element, 'XPath.validate')
                assertion["validate-xml"] = validate if validate else False
                whitespace = self._get_bool_prop(assertion_element, 'XPath.whitespace')
                assertion["ignore-whitespace"] = whitespace if whitespace else False
                tolerant = self._get_bool_prop(assertion_element, 'XPath.tolerant')
                assertion["use-tolerant-parser"] = tolerant if tolerant else False
                invert = self._get_bool_prop(assertion_element, 'XPath.negate')
                assertion["invert"] = invert if invert else False

                assertions.append(assertion)

        return assertions

    def _get_variables(self, element):
        """
        :param element:
        :return:
        """
        variables = {}
        hashtree = element.getnext()

        if hashtree is not None and hashtree.tag == "hashTree":
            arguments = [element for element in hashtree.iterchildren() if element.tag == "Arguments"]
            for argument in arguments:
                for element in argument.find(".//collectionProp").findall(".//elementProp"):
                    var_name = self._get_string_prop(element, 'Argument.name')
                    var_value = self._get_string_prop(element, 'Argument.value')
                    if var_name and var_value:
                        variables[var_name] = var_value

        return {"variables": variables} if variables else {}

    def _get_jsr223_processors(self, element):
        """
        jsr223 option
        :param element:
        :return:
        """
        extensions = {
            'javascript': '.js',
            'beanshell': '.bsh',
            'bsh': '.bsh',
            'ecmascript': '.js',
            'groovy': '.groovy',
            'java': '.java',
            'jexl': '.jexl',
            'jexl2': '.jexl2',
        }
        jsrs = []

        hashtree = element.getnext()
        if hashtree is not None and hashtree.tag == "hashTree":
            elements = [element
                        for element in hashtree.iterchildren()
                        if element.tag in ("JSR223PreProcessor", "JSR223PostProcessor")]
            for element in elements:
                if element is not None:
                    language = self._get_string_prop(element, 'scriptLanguage')
                    filename = self._get_string_prop(element, 'filename')
                    params = self._get_string_prop(element, 'parameters')
                    script = self._get_string_prop(element, 'script')
                    execute = "before" if element.tag == "JSR223PreProcessor" else "after"
                    if filename:
                        jsr = {
                            "language": language,
                            "script-file": filename,
                            "parameters": params,
                            "execute": execute,
                        }
                    elif script:
                        # TODO: extract script to filename
                        ext = extensions.get(language, '.js')
                        filename = self._record_additional_file('script', ext, script)
                        self.additional_files[filename] = script
                        jsr = {
                            "language": language,
                            "script-file": filename,
                            "parameters": params,
                            "execute": execute,
                        }
                    else:
                        tmpl = "%s element doesn't have neither script nor script-file, skipping"
                        self.log.warning(tmpl, element.tag)
                        continue
                    jsrs.append(jsr)
        if jsrs:
            return {"jsr223": jsrs}
        else:
            return {}

    def process_tg(self, tg_etree_element):
        """
        Get execution and scenario settings for TG
        :param tg_etree_element:
        :return: dict
        """
        self.log.debug("Processing thread group... %s", tg_etree_element.get("testname"))
        tg_scenario_settings = self._get_tg_scenario_settings(tg_etree_element)
        tg_name = tg_etree_element.get("testname")

        if not tg_name:
            tg_name = str(tg_etree_element.__hash__())
        ht_element = tg_etree_element.getnext()

        if ht_element.tag == "hashTree":
            requests = self.__extract_requests(ht_element)
            self.log.debug("Total requests in tg groups: %d", len(requests))
            if not requests:
                self.log.warning("No requests in %s (%s)", tg_etree_element.tag, tg_etree_element.get("testname"))
            tg_scenario_settings["requests"].extend(requests)
        tg_scenario_dict = {tg_name: tg_scenario_settings}
        tg_executions_dict = {"scenario": tg_name}
        tg_executions_dict.update(self._get_tg_execution_settings(tg_etree_element))
        return tg_executions_dict, tg_scenario_dict

    def __extract_requests(self, ht_element):
        requests = []
        children = ht_element.iterchildren()
        while True:
            try:
                elem = next(children)
            except StopIteration:
                break
            if elem.tag == 'IfController':
                hash_tree = next(children)
                if_block = self.__extract_if_controller(elem, hash_tree)
                requests.append(if_block)
            elif elem.tag == 'LoopController':
                hash_tree = next(children)
                loop_block = self.__extract_loop_controller(elem, hash_tree)
                requests.append(loop_block)
            elif elem.tag == 'WhileController':
                hash_tree = next(children)
                while_block = self.__extract_while_controller(elem, hash_tree)
                requests.append(while_block)
            elif elem.tag == 'ForeachController':
                hash_tree = next(children)
                block = self.__extract_foreach_controller(elem, hash_tree)
                requests.append(block)
            elif elem.tag == 'TransactionController':
                hash_tree = next(children)
                block = self.__extract_trans_controller(elem, hash_tree)
                requests.append(block)
            elif elem.tag == 'HTTPSamplerProxy':
                request = self._get_request_settings(elem)
                requests.append(request)
            elif elem.tag:
                subrequests = self.__extract_requests(elem)
                requests.extend(subrequests)
        return requests

    def __extract_if_controller(self, controller, ht_element):
        condition = self._get_string_prop(controller, "IfController.condition")
        requests = self.__extract_requests(ht_element)
        return {'if': condition, 'then': requests}

    def __extract_loop_controller(self, controller, ht_element):
        iterations = self._get_string_prop(controller, "LoopController.loops")
        if iterations.isdigit():
            iterations = int(iterations)
        forever = self._get_bool_prop(controller, "LoopController.continue_forever")
        requests = self.__extract_requests(ht_element)
        loops = 'forever' if forever else iterations
        return {'loop': loops, 'do': requests}

    def __extract_while_controller(self, controller, ht_element):
        condition = self._get_string_prop(controller, "WhileController.condition")
        requests = self.__extract_requests(ht_element)
        return {'while': condition, 'do': requests}

    def __extract_foreach_controller(self, controller, ht_element):
        input_var = self._get_string_prop(controller, "ForeachController.inputVal", default="")
        loop_var = self._get_string_prop(controller, "ForeachController.returnVal", default="")
        requests = self.__extract_requests(ht_element)
        iteration_str = '%s in %s' % (loop_var, input_var)
        return {'foreach': iteration_str, 'do': requests}

    def __extract_trans_controller(self, controller, ht_element):
        name = controller.get('testname')
        requests = self.__extract_requests(ht_element)
        return {'transaction': name, 'do': requests}

    def _get_request_settings(self, request_element):
        """
        Gets all possible request settings
        :param request_element:
        :return: dict
        """
        request_config = {}
        request_config.update(self._get_request_base(request_element))
        request_config.update(self._get_request_body(request_element))
        request_config.update(self._get_upload_files(request_element))
        request_config.update(self._get_headers(request_element))
        request_config.update(self.__get_constant_timer(request_element))
        request_config.update(self._get_request_timeout(request_element))
        request_config.update(self._get_extractors(request_element))
        request_config.update(self._get_assertions(request_element))
        request_config.update(self._get_jsr223_processors(request_element))
        return request_config

    def _get_tg_scenario_settings(self, tg_etree_element):
        """
        Gets all possible tg settings and applies global overrides
        :param tg_etree_element:
        :return:
        """
        default_tg_settings = {"store-cookie": False, "store-cache": False, "use-dns-cache-mgr": False}
        global_tg_settings = self._get_global_tg_scenario()
        tg_settings = {"requests": []}
        tg_settings.update(default_tg_settings)
        tg_settings.update(self._get_data_sources(tg_etree_element))
        tg_settings.update(self._get_headers(tg_etree_element))
        tg_settings.update(self._get_store_cache(tg_etree_element))
        tg_settings.update(self._get_store_cookie(tg_etree_element))
        tg_settings.update(self._get_dns_mgr(tg_etree_element))
        tg_settings.update(self._get_extractors(tg_etree_element))
        tg_settings.update(self._get_assertions(tg_etree_element))
        tg_settings.update(self._get_variables(tg_etree_element))
        # apply global test plan settings:
        self._apply_global_tg_settings(global_tg_settings, tg_settings)
        # those settings will override global:
        tg_settings.update(self.__get_constant_timer(tg_etree_element))
        tg_settings.update(self._get_http_request_defaults(tg_etree_element))
        return tg_settings

    def _get_tg_execution_settings(self, tg_etree_element):
        """
        Gets execution settings
        :param tg_etree_element:
        :return: dict
        """
        global_execution_settings = self._get_global_tg_execution()
        execution_settings = {
            'concurrency': 1,
            'iterations': 1,
            'hold-for': '60s',
            'ramp-up': '60s',
        }
        self._apply_global_tg_settings(global_execution_settings, execution_settings)
        execution_settings.update(self._get_concurrency(tg_etree_element))
        execution_settings.update(self._get_iterations(tg_etree_element))
        execution_settings.update(self._get_duration(tg_etree_element))
        execution_settings.update(self._get_throughput(tg_etree_element))

        return execution_settings

    def _get_global_objects(self):
        """
        list of global objects in test plan
        :return:
        """
        self.global_objects = []
        try:
            ht_object = self.tree.find(".//hashTree").find(".//TestPlan").getnext()
        except:
            raise RuntimeError("Bad jmx format")
        for obj in ht_object.iterchildren():
            if obj.tag != 'hashTree' and obj.tag != 'ThreadGroup':
                self.global_objects.append(obj)

    def _get_global_tg_scenario(self):
        """
        :return: dict
        """
        default_tg_settings = {}
        testplan_element = self.tree.find(".//TestPlan")
        default_tg_settings.update(self._get_headers(testplan_element))
        default_tg_settings.update(self._get_data_sources(testplan_element))
        default_tg_settings.update(self._get_store_cache(testplan_element))
        default_tg_settings.update(self._get_store_cookie(testplan_element))
        default_tg_settings.update(self._get_dns_mgr(testplan_element))
        default_tg_settings.update(self.__get_constant_timer(testplan_element))
        default_tg_settings.update(self._get_http_request_defaults(testplan_element))
        default_tg_settings.update(self._get_extractors(testplan_element))
        default_tg_settings.update(self._get_assertions(testplan_element))
        default_tg_settings.update(self._get_variables(testplan_element))
        return default_tg_settings

    def _get_global_tg_execution(self):
        """
        :return: dict
        """
        default_tg_settings = {}
        testplan_element = self.tree.find(".//TestPlan")
        default_tg_settings.update(self._get_throughput(testplan_element))
        return default_tg_settings

    @staticmethod
    def _apply_global_tg_settings(defaults, tg_dict, override=True):
        for default_key, default_value in defaults.items():
            if isinstance(default_value, list):
                if default_key in tg_dict.keys():
                    tg_dict[default_key].extend(deepcopy(default_value))
                else:
                    tg_dict[default_key] = deepcopy(default_value)
            elif isinstance(default_value, dict):
                if default_key in tg_dict.keys():
                    tg_dict[default_key].update(default_value)
                else:
                    tg_dict[default_key] = defaults[default_key]
            else:
                if default_key not in tg_dict.keys():
                    tg_dict[default_key] = defaults[default_key]
                else:
                    if override:
                        tg_dict[default_key] = defaults[default_key]

    @staticmethod
    def _remove_element(element):
        sibling = element.getnext()
        if sibling is not None and sibling.tag == "hashTree":
            sibling.getparent().remove(sibling)
        element.getparent().remove(element)

    def _clean_disabled_elements(self, element):
        """
        Removes all disabled elements
        :param element:
        :return:
        """
        for subelement in element.iter():
            if subelement.tag.endswith("prop"):
                continue
            if subelement.get("enabled") == 'false':
                self.log.info("Removing disabled element %s (%s)", subelement.tag, subelement.get("testname"))
                self._remove_element(subelement)
                self._clean_disabled_elements(element)
                return

    def _clean_jmx_tree(self, element):
        """
        Removes all unknown elements
        :return:
        """
        for subelement in element.iter():
            if subelement.tag.lower().endswith("prop"):
                continue
            if subelement.tag not in KNOWN_TAGS and not subelement.tag.endswith("Controller"):
                self.log.warning("Removing unknown element: %s (%s)", subelement.tag, subelement.get("testname"))
                self._remove_element(subelement)
                self._clean_jmx_tree(element)
                return

    def _record_additional_file(self, base_filename, extension, content):
        filename = base_filename + extension
        if filename not in self.additional_files:
            self.additional_files[filename] = content
            return filename

        for index in itertools.count(start=1):
            suffix = '-%d' % index
            filename = base_filename + suffix + extension
            if filename not in self.additional_files:
                self.additional_files[filename] = content
                return filename


class Converter(object):
    """
    View
    """

    def __init__(self, log):
        self.log = log.getChild(self.__class__.__name__)
        self.dialect = JMXasDict(self.log)

    def convert(self, file_to_convert, dump_modified_jmx_to=None):
        """
        Get all thread groups from jmx, convert to dict.
        :type dump_modified_jmx_to: str
        :type file_to_convert: str
        :rtype: dict
        """
        self.dialect.load(file_to_convert)
        base_script = {"scenarios": {}, ScenarioExecutor.EXEC: []}
        self.log.debug("Processing thread groups...")
        tg_etree_elements = self.dialect.tree.findall(".//ThreadGroup")
        if not tg_etree_elements:
            raise RuntimeError("No thread groups found!")

        if tg_etree_elements:
            self.log.debug("Total thread groups: %d", len(tg_etree_elements))

            for tg_etree_element in tg_etree_elements:
                td_executions_dict, tg_scenario_dict = self.dialect.process_tg(tg_etree_element)
                base_script["scenarios"].update(tg_scenario_dict)
                base_script[ScenarioExecutor.EXEC].append(td_executions_dict)
                self.log.debug("Done processing thread group %s", tg_etree_element.get("testname"))

            if dump_modified_jmx_to:
                self._dump_modified_jmx(dump_modified_jmx_to)
            return base_script
        else:
            self.log.error("No thread groups was found!")
            raise RuntimeError("No thread groups was found in JMX file!")

    def _dump_modified_jmx(self, dump_jmx):
        """
        Dump modified JMX from dialect object
        """
        if dump_jmx:
            if os.path.exists(dump_jmx):
                self.log.warning("%s already exists and will be overwritten", dump_jmx)
                self.dialect.save(dump_jmx)
                self.log.info("Cleaned JMX saved in %s", dump_jmx)


class JMX2YAML(object):
    """
    Controller
    """

    def __init__(self, options, file_name):
        self.log = logging.getLogger(self.__class__.__name__)
        self.options = options
        self.setup_logging()
        self.converter = None
        self.file_to_convert = file_name

    def setup_logging(self):
        CLI.setup_logging(self.options)
        if self.options.quiet:
            logging.disable(logging.WARNING)

    def process(self):
        """
        Process file
        :return:
        """
        output_format = Configuration.JSON if self.options.json else Configuration.YAML

        self.log.info('Loading jmx file %s', self.file_to_convert)
        self.file_to_convert = os.path.abspath(os.path.expanduser(self.file_to_convert))
        if not os.path.exists(self.file_to_convert):
            self.log.error("File %s does not exist", self.file_to_convert)
            raise RuntimeError("File does not exist: %s" % self.file_to_convert)
        self.converter = Converter(self.log)
        try:
            jmx_as_dict = self.converter.convert(self.file_to_convert, self.options.dump_jmx)
        except BaseException:
            self.log.error("Error while processing jmx file: %s", self.file_to_convert)
            raise

        exporter = Configuration()
        exporter.merge(jmx_as_dict)

        if self.options.file_name:
            file_name = self.options.file_name
        else:
            file_name = self.file_to_convert + "." + output_format.lower()

        exporter.dump(file_name, output_format)

        additional_files_dir = get_full_path(file_name, step_up=1)
        for filename in self.converter.dialect.additional_files:
            path = os.path.join(additional_files_dir, filename)
            self.log.info("Writing additional file: %s", path)
            content = self.converter.dialect.additional_files[filename]
            with open(path, 'w') as f:
                f.write(content)

        self.log.info("Done processing, result saved in %s", file_name)


def main():
    usage = "Usage: jmx2yaml [input jmx file] [options]"
    parser = OptionParser(usage=usage, prog="jmx2yaml")
    parser.add_option('-v', '--verbose', action='store_true', default=False,
                      help="Prints all logging messages to console")
    parser.add_option('-o', '--out', dest="file_name",
                      help="Set output .yml file name, by default input file name + .yml is used")
    parser.add_option('-d', '--dump-jmx', action='store', dest='dump_jmx', default=False,
                      help="Dumps processed JMX to file")
    parser.add_option('-q', '--quiet', action='store_true', default=False, dest='quiet',
                      help="Do not display any log messages")
    parser.add_option('-j', '--json', action='store_true', default=False, dest='json',
                      help="Use JSON format")
    parser.add_option('-l', '--log', action='store', default=False, help="Log file location")
    parsed_options, args = parser.parse_args()
    if len(args) > 0:
        tool = JMX2YAML(parsed_options, args[0])
        code = 0
        try:
            tool.process()
        except BaseException as exc:
            logging.error("Exception: %s", exc)
            logging.debug("Exception: %s", traceback.format_exc())
            code = 1
        exit(code)
    else:
        sys.stdout.write(usage + "\n")


if __name__ == "__main__":
    main()
