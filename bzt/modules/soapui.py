"""
SoapUI module for Taurus.

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

from bzt import TaurusInternalException
from bzt.six import etree, iteritems
from bzt.utils import BetterDict


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
        body = config.findtext('./con:request', namespaces=self.NAMESPACES)
        params = config.findall('./con:parameters/con:parameter', namespaces=self.NAMESPACES)

        request = {"url": url, "label": label}

        if method is not None and method != "GET":
            request["method"] = method

        if headers:
            request["headers"] = headers

        if assertions:
            request["assert"] = assertions

        if body is not None:
            request["body"] = body

        if params:
            request["body"] = {
                param.findtext("./con:name", namespaces=self.NAMESPACES): param.findtext("./con:value",
                                                                                         namespaces=self.NAMESPACES)
                for param in params
            }

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

    def _extract_properties(self, test_step):
        properties = test_step.findall('./con:config/con:properties/con:property', namespaces=self.NAMESPACES)
        return {
            prop.findtext('./con:name',
                          namespaces=self.NAMESPACES): prop.findtext('./con:value',
                                                                     namespaces=self.NAMESPACES)
            for prop in properties
        }

    def _extract_execution(self, test_case):
        load_exec = {}
        load_test = test_case.find('./con:loadTest', namespaces=self.NAMESPACES)
        if load_test is not None:
            load_exec['concurrency'] = int(load_test.find('./con:threadCount', self.NAMESPACES).text)
            load_exec['hold-for'] = int(load_test.find('./con:testLimit', self.NAMESPACES).text)
        else:
            load_exec['concurrency'] = 1
        return load_exec

    def _extract_property_transfer(self, test_step):
        extractors = BetterDict()  # label -> {extract-xpath: ..., extract-jsonpath: ...}
        transfers = test_step.findall('./con:config/con:transfers', namespaces=self.NAMESPACES)
        if not transfers:
            return None

        for transfer in transfers:
            source_type = transfer.findtext('./con:sourceType', namespaces=self.NAMESPACES)
            source_step_name = transfer.findtext('./con:sourceStep', namespaces=self.NAMESPACES)
            query = transfer.findtext('./con:sourcePath', namespaces=self.NAMESPACES)
            transfer_type = transfer.findtext('./con:type', namespaces=self.NAMESPACES)
            target_step_name = transfer.findtext('./con:targetStep', namespaces=self.NAMESPACES)
            target_prop = transfer.findtext('./con:targetType', namespaces=self.NAMESPACES)

            if source_type != "Response":
                self.log.warning("Found Property Transfer with non-response source (%s). Skipping", source_type)
                continue

            if transfer_type not in ["JSONPATH", "XPATH"]:
                self.log.warning("Found Property Transfer with unsupported type (%s). Skipping", transfer_type)
                continue

            source_step = self.tree.find("//con:testStep[@name='%s']" % source_step_name, namespaces=self.NAMESPACES)
            if source_step is None:
                self.log.warning("Can't find source step (%s) for Property Transfer. Skipping", source_step_name)
                continue

            source_step_type = source_step.get("type")
            if source_step_type not in ["httprequest", "restrequest"]:
                self.log.warning("Unsupported source step type for Property Transfer (%s). Skipping", source_step_type)
                continue

            target_step = self.tree.find("//con:testStep[@name='%s']" % target_step_name, namespaces=self.NAMESPACES)
            if target_step is None:
                self.log.warning("Can't find target step (%s) for Property Transfer. Skipping", target_step_name)
                continue

            target_step_type = target_step.get("type")
            if target_step_type != "properties":
                self.log.warning("Unsupported source step type for Property Transfer (%s). Skipping", target_step_type)
                continue

            extractor = BetterDict()
            if transfer_type == "JSONPATH":
                extractor.merge({
                    'extract-jsonpath': {
                        target_prop: {
                            'jsonpath': query,
                            'default': 'NOT_FOUND',
                        }
                    }
                })
            elif transfer_type == "XPATH":
                extractor.merge({
                    'extract-xpath': {
                        target_prop: {
                            'xpath': query,
                            'default': 'NOT_FOUND',
                        }
                    }
                })
            extractors.merge({source_step_name: extractor})

        return extractors

    def _extract_scenario(self, test_case):
        variables = {}
        requests = []

        extractors = BetterDict()

        steps = test_case.findall('.//con:testStep', namespaces=self.NAMESPACES)
        for step in steps:
            request = None
            if step.get("type") == "httprequest":
                request = self._extract_http_request(step)
            elif step.get("type") == "restrequest":
                request = self._extract_rest_request(step)
            elif step.get("type") == "properties":
                props = self._extract_properties(step)
                variables.update(props)
            elif step.get("type") == "transfer":
                extracted_extractors = self._extract_property_transfer(step)  # label -> extractor
                if extracted_extractors:
                    self.log.info("extracted extractors: %s", extracted_extractors)
                    extractors.merge(extracted_extractors)

            if request is not None:
                requests.append(request)

        for request in requests:
            label = request["label"]
            if label in extractors:
                request.update(extractors[label])

        scenario = {
            "test-case": test_case.get("name"),
            "requests": requests
        }
        if variables:
            scenario["variables"] = variables

        return scenario

    def _extract_config(self, test_suites, target_test_case=None):
        execution = []
        scenarios = {}

        for suite in test_suites:
            test_cases = suite.findall('.//con:testCase', namespaces=self.NAMESPACES)
            for case in test_cases:
                case_name = case.get("name")
                scenario_name = suite.get("name") + "-" + case_name
                scenario = self._extract_scenario(case)
                scenario['test-suite'] = suite.get("name")
                self.log.debug("Extracted scenario: %s", scenario_name)

                load_exec = self._extract_execution(case)
                load_exec['scenario'] = scenario_name
                self.log.debug("Extracted execution for scenario %s", scenario_name)

                if target_test_case is None or target_test_case == case_name:
                    scenarios[scenario_name] = scenario
                    execution.append(load_exec)

        return {
            "execution": execution,
            "scenarios": scenarios,
        }

    def convert_script(self, script_path, target_test_case=None):
        if not os.path.exists(script_path):
            raise ValueError("SoapUI script %s doesn't exist" % script_path)

        self.load(script_path)

        self.log.debug("Found namespaces: %s", self.NAMESPACES)

        projects = self.tree.xpath('//con:soapui-project', namespaces=self.NAMESPACES)
        self.log.debug("Found projects: %s", projects)
        project = projects[0]

        interface = project.find('.//con:interface', namespaces=self.NAMESPACES)
        self.log.debug("Found interface: %s", interface)

        test_suites = project.findall('.//con:testSuite', namespaces=self.NAMESPACES)
        self.log.debug("Found test suites: %s", test_suites)

        config = self._extract_config(test_suites, target_test_case=target_test_case)

        if not config["scenarios"]:
            self.log.warning("No scenarios were extracted")

        if not config["execution"]:
            self.log.warning("No load tests were extracted")

        return config

    def find_soapui_test_case(self, test_case, scenarios):
        matching_scenarios = [
            (name, scen)
            for name, scen in iteritems(scenarios)
            if scen.get("test-case") == test_case
        ]
        if len(matching_scenarios) == 0:
            sorted_scenarios = sorted((name, scen) for name, scen in iteritems(scenarios))
            scenario_name, scenario = next(iter(sorted_scenarios))
            if test_case is None:
                self.log.warning("No `test-case` specified for SoapUI script, will use '%s'", scenario.get("test-case"))
            else:
                msg = "No matching test cases found for name '%s', using the '%s'"
                self.log.warning(msg, test_case, scenario.get("test-case"))
        elif len(matching_scenarios) > 1:
            scenario_name, scenario = next(iter(matching_scenarios))
            msg = "Multiple test cases found for name '%s', using case '%s' from suite '%s'"
            self.log.warning(msg, test_case, scenario.get('test-case'), scenario.get('test-suite'))
        else:
            scenario_name, scenario = next(iter(matching_scenarios))
        return scenario_name, scenario
