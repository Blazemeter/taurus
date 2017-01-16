"""
SoapUI support for Taurus.

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
import os
from lxml import etree

from bzt import TaurusInternalException
from bzt.engine import Service


class SoapUIService(Service):
    def __init__(self):
        super(SoapUIService, self).__init__()

    def prepare(self):
        script = self.parameters.get("script", ValueError("'script' for soapui should be provided"))
        executor = self.parameters.get("executor", None)

        converter = SoapUIScriptConverter(self.log)
        config = converter.convert(script)

        for ex in config.get("execution", []):
            if executor:
                ex["executor"] = executor

        self.engine.config.merge(config)

    def startup(self):
        pass

    def check(self):
        return False

    def shutdown(self):
        pass

    def post_process(self):
        pass


class SoapUIScriptConverter(object):
    NAMESPACES = dict(con="http://eviware.com/soapui/config")

    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.tree = None

    def load(self, path):
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(path)
        except BaseException as exc:
            msg = "XML parsing failed for file %s: %s"
            raise TaurusInternalException(msg % (path, exc))

    def _extract_headers(self, config_elem):
        headers_settings = config_elem.find(
            './/con:settings/con:setting[@id="com.eviware.soapui.impl.wsdl.WsdlRequest@request-headers"]',
            namespaces=self.NAMESPACES)
        if headers_settings is None:
            return None
        headers = etree.fromstring(headers_settings.text)
        if "{" + self.NAMESPACES['con'] + "}" + "entry" == headers.tag:
            entries = [headers]
        else:
            entries = headers.findall(".//con:entry", namespaces=self.NAMESPACES)

        headers = {entry.get('key'): entry.get('value')
                   for entry in entries}
        return headers

    def _extract_assertions(self, config_elem):
        assertions = []
        assertion_tags = config_elem.findall('.//con:assertion', namespaces=self.NAMESPACES)
        for assertion in assertion_tags:
            if assertion.get('type') in ('Simple Contains', 'Simple NotContains'):
                subject = assertion.findtext('./con:configuration/token', namespaces=self.NAMESPACES)
                use_regex = assertion.findtext('./con:configuration/useRegEx', namespaces=self.NAMESPACES)
                negate = assertion.get('type') == 'Simple NotContains'

                assertions.append({"contains": [subject],
                                   "subject": "body",
                                   "regexp": use_regex == "false",
                                   "not": negate,
                                   })
        return assertions

    def _extract_http_request(self, test_step):
        label = test_step.get('name')
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        method = config.get('method')
        endpoint = config.find('.//con:endpoint', namespaces=self.NAMESPACES)
        url = endpoint.text
        headers = self._extract_headers(config)
        assertions = self._extract_assertions(config)

        request = {"url": url, "label": label}

        if method is not None and method != "GET":
            request["method"] = method

        if headers:
            request["headers"] = headers

        if assertions:
            request["assert"] = assertions

        return request

    def _extract_rest_request(self, test_step):
        label = test_step.get('name')
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        method = config.get('method')
        url = config.get('service') + config.get('resourcePath')
        headers = self._extract_headers(config)
        assertions = self._extract_assertions(config)

        request = {"url": url, "label": label}

        if method is not None and method != "GET":
            request["method"] = method

        if headers:
            request["headers"] = headers

        if assertions:
            request["assert"] = assertions

        return request

    def convert(self, script_path):
        if not os.path.exists(script_path):
            raise ValueError("SoapUI script %s doesn't exist" % script_path)

        self.load(script_path)

        self.log.debug("Found namespaces: %s", self.NAMESPACES)

        # project - con:soapui-project
        projects = self.tree.xpath('//con:soapui-project', namespaces=self.NAMESPACES)
        self.log.debug("Found projects: %s", projects)
        project = projects[0]

        # interface - con:interface (inside project)
        interface = project.find('.//con:interface', namespaces=self.NAMESPACES)
        self.log.debug("Found interface: %s", interface)

        # test suite - con:testSuite (inside project)
        test_suites = project.findall('.//con:testSuite', namespaces=self.NAMESPACES)
        self.log.debug("Found test suites: %s", test_suites)

        execution = []
        scenarios = {}
        for suite in test_suites:
            test_cases = suite.findall('.//con:testCase', namespaces=self.NAMESPACES)
            for case in test_cases:
                scenario_name = suite.get("name") + "-" + case.get("name")
                requests = []
                steps = case.findall('.//con:testStep', namespaces=self.NAMESPACES)
                for step in steps:
                    if step.get("type") == "httprequest":
                        request = self._extract_http_request(step)
                        if request is not None:
                            requests.append(request)
                    elif step.get("type") == "restrequest":
                        request = self._extract_rest_request(step)
                        if request is not None:
                            requests.append(request)

                scenarios[scenario_name] = {"requests": requests}
                self.log.debug("Extracted scenario: %s", scenario_name)

                load_exec = {}
                load_test = case.find('./con:loadTest', namespaces=self.NAMESPACES)
                if load_test is not None:
                    load_exec['concurrency'] = int(load_test.find('./con:threadCount', self.NAMESPACES).text)
                    load_exec['hold-for'] = int(load_test.find('./con:testLimit', self.NAMESPACES).text)
                else:
                    load_exec['concurrency'] = 1

                load_exec['scenario'] = scenario_name
                self.log.debug("Extracted execution for scenario %s", scenario_name)

                execution.append(load_exec)

        return {
            "execution": execution,
            "scenarios": scenarios,
        }


# Strategy #1: execute SoapUI with given script (or generate the script from given scenario)
# Pros:
# - simple execution scheme
# - script generation is entirely possible
# Cons:
# - extracting live stats for the dashboard seems kind of tricky (or even impossible for the free version)

# Strategy #2: convert SoapUI script into YAML format and execute it with JMeter (or convert straight to JMX)
# Pros:
# - simpler
# - allows to execute the test with whatever Taurus supports
# - allows to capture raw load test stats
# Cons:
# - not actually executing SoapUI
# - need to marry SoapUI and Taurus semantics

# Conversion strategy.
# For all test suites:
#   For each test case:
#     convert test steps into a scenario
#     convert corresponding load tests into an execution
# if test case name (or load test name) is given - convert only corresponding test case
