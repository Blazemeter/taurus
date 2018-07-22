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
import copy
import os

from bzt import TaurusInternalException
from bzt.six import etree, iteritems
from bzt.utils import BetterDict


class SoapUIScriptConverter(object):
    NAMESPACES = dict(con="http://eviware.com/soapui/config")

    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.tree = None
        self.interface = None

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
            # TODO: XPath assertions / JSONPath assertions ?
            if assertion.get('type') in ('Simple Contains', 'Simple NotContains'):
                subject = assertion.findtext('./con:configuration/token', namespaces=self.NAMESPACES)
                use_regex = assertion.findtext('./con:configuration/useRegEx', namespaces=self.NAMESPACES)
                negate = assertion.get('type') == 'Simple NotContains'

                assertions.append({"contains": [subject],
                                   "subject": "body",
                                   "regexp": use_regex == "true",
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
            body = {}
            for param in params:
                key = param.findtext("./con:name", namespaces=self.NAMESPACES)
                value = param.findtext("./con:value", namespaces=self.NAMESPACES)
                body[key] = value
            request["body"] = body

        return request

    def _extract_soap_endpoint(self, interface_name, operation_name):
        interface = self.tree.find("//con:interface[@name='%s']" % interface_name, namespaces=self.NAMESPACES)
        if interface is None:
            self.log.warning("Can't find intreface %s for operation %s, skipping", interface_name, operation_name)
            return None

        interface_endpoint = interface.findtext("./con:endpoints/con:endpoint", namespaces=self.NAMESPACES)

        operation = interface.find(".//con:operation[@name='%s']" % operation_name, namespaces=self.NAMESPACES)
        if operation is None:
            self.log.warning("Can't find operation %s for interface %s, skipping", operation_name, interface_name)
            return None

        operation_endpoint = operation.findtext(".//con:endpoint", namespaces=self.NAMESPACES)

        if operation_endpoint is not None:
            return operation_endpoint
        elif interface_endpoint is not None:
            return interface_endpoint
        else:
            self.log.warning("Can't find endpoint for %s:%s", interface_name, operation_name)
            return None

    def _extract_soap_request(self, test_step):
        label = test_step.get('name')
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        body = config.findtext('./con:request/con:request', namespaces=self.NAMESPACES)

        interface = config.findtext('./con:interface', namespaces=self.NAMESPACES)
        operation = config.findtext('./con:operation', namespaces=self.NAMESPACES)
        self.log.debug("Extracting SOAP request, interface=%r, operation=%r", interface, operation)
        endpoint = self._extract_soap_endpoint(interface, operation)

        if endpoint is None:
            return

        request = {
            "url": endpoint,
            "label": label,
            "method": "POST",
            "headers": {
                "Content-Type": "text/xml; charset=utf-8",
            }
        }

        if body:
            request["body"] = body

        return request

    def _calc_base_address(self, test_step):
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        service = config.get('service')
        interfaces = self.tree.xpath('//con:interface', namespaces=self.NAMESPACES)
        for interface in interfaces:
            if interface.get("name") == service:
                endpoint = interface.find('.//con:endpoints/con:endpoint', namespaces=self.NAMESPACES)
                if endpoint is not None:
                    service = endpoint.text
                    break
        return service

    def _extract_rest_request(self, test_step):
        label = test_step.get('name')
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        method = config.get('method')

        method_name = config.get('methodName')
        method_obj = self.interface.find('.//con:method[@name="%s"]' % method_name, namespaces=self.NAMESPACES)
        params = BetterDict()
        if method_obj is not None:
            parent = method_obj.getparent()
            while parent.tag.endswith('resource'):
                for param in parent.findall('./con:parameters/con:parameter', namespaces=self.NAMESPACES):
                    param_name = param.findtext('./con:name', namespaces=self.NAMESPACES)
                    param_value = param.findtext('./con:value', namespaces=self.NAMESPACES)
                    def_value = param.findtext('./con:default', namespaces=self.NAMESPACES)
                    if param_value:
                        params[param_name] = param_value
                    elif def_value:
                        params[param_name] = def_value

                parent = parent.getparent()

        url = self._calc_base_address(test_step) + config.get('resourcePath')
        headers = self._extract_headers(config)
        assertions = self._extract_assertions(config)

        params.merge({
            entry.get("key"): entry.get("value")
            for entry in config.findall('./con:restRequest/con:parameters/con:entry', namespaces=self.NAMESPACES)
        })

        for param_name in copy.copy(list(params.keys())):
            template = "{" + param_name + "}"
            if template in url:
                param_value = params.pop(param_name)
                url = url.replace(template, param_value)

        request = {"url": url, "label": label}

        if method is not None and method != "GET":
            request["method"] = method

        if headers:
            request["headers"] = headers

        if assertions:
            request["assert"] = assertions

        body = {}
        for key, value in iteritems(params):
            body[key] = value

        if body:
            request["body"] = body

        return request

    def _extract_properties(self, block, key_prefix=""):
        properties = block.findall('./con:properties/con:property', namespaces=self.NAMESPACES)
        prop_map = {}
        for prop in properties:
            key = key_prefix + prop.findtext('./con:name', namespaces=self.NAMESPACES)
            value = prop.findtext('./con:value', namespaces=self.NAMESPACES)
            prop_map[key] = value
        return prop_map

    def _extract_execution(self, test_case):
        load_exec = {}
        load_test = test_case.find('./con:loadTest', namespaces=self.NAMESPACES)
        if load_test is not None:
            load_exec['concurrency'] = int(load_test.find('./con:threadCount', self.NAMESPACES).text)
            load_exec['hold-for'] = int(load_test.find('./con:testLimit', self.NAMESPACES).text)
        else:
            load_exec['concurrency'] = 1
        return load_exec

    def _validate_transfer(self, source_type, source_step_name, transfer_type, target_step_name):
        source_step = self.tree.find("//con:testStep[@name='%s']" % source_step_name, namespaces=self.NAMESPACES)
        if source_step is None:
            self.log.warning("Can't find source step (%s) for Property Transfer. Skipping", source_step_name)
            return False

        source_step_type = source_step.get("type")
        if source_step_type not in ["httprequest", "restrequest", "request"]:
            self.log.warning("Unsupported source step type for Property Transfer (%s). Skipping", source_step_type)
            return False

        if source_type != "Response":
            self.log.warning("Found Property Transfer with non-response source (%s). Skipping", source_type)
            return False

        if transfer_type not in ["JSONPATH", "XPATH"]:
            self.log.warning("Found Property Transfer with unsupported type (%s). Skipping", transfer_type)
            return False

        target_step = self.tree.find("//con:testStep[@name='%s']" % target_step_name, namespaces=self.NAMESPACES)
        if target_step is None:
            self.log.warning("Can't find target step (%s) for Property Transfer. Skipping", target_step_name)
            return False

        target_step_type = target_step.get("type")
        if target_step_type != "properties":
            self.log.warning("Unsupported target step type for Property Transfer (%s). Skipping", target_step_type)
            return False

        return True

    def _extract_transfer(self, transfer):
        source_type = transfer.findtext('./con:sourceType', namespaces=self.NAMESPACES)
        source_step_name = transfer.findtext('./con:sourceStep', namespaces=self.NAMESPACES)
        query = transfer.findtext('./con:sourcePath', namespaces=self.NAMESPACES)
        transfer_type = transfer.findtext('./con:type', namespaces=self.NAMESPACES)
        target_step_name = transfer.findtext('./con:targetStep', namespaces=self.NAMESPACES)
        target_prop = transfer.findtext('./con:targetType', namespaces=self.NAMESPACES)

        if source_step_name.startswith("#") and source_step_name.endswith("#"):
            source_step_name = source_step_name[1:-1]

        if not self._validate_transfer(source_type, source_step_name, transfer_type, target_step_name):
            return None

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
        return {source_step_name: extractor}

    def _extract_property_transfers(self, test_step):
        extractors = BetterDict()  # label -> {extract-xpath: ..., extract-jsonpath: ...}
        transfers = test_step.findall('./con:config/con:transfers', namespaces=self.NAMESPACES)
        if not transfers:
            return None

        for transfer in transfers:
            extracted_transfer = self._extract_transfer(transfer)
            if extracted_transfer is not None:
                extractors.merge(extracted_transfer)

        return extractors

    def _extract_scenario(self, test_case, case_level_props):
        variables = BetterDict.from_dict(case_level_props)
        requests = []

        extractors = BetterDict()

        steps = test_case.findall('.//con:testStep', namespaces=self.NAMESPACES)
        for step in steps:
            request = None
            if step.get("type") == "httprequest":
                request = self._extract_http_request(step)
            elif step.get("type") == "restrequest":
                request = self._extract_rest_request(step)
            elif step.get("type") == "request":
                request = self._extract_soap_request(step)
            elif step.get("type") == "properties":
                config_block = step.find('./con:config', namespaces=self.NAMESPACES)
                if config_block is not None:
                    props = self._extract_properties(config_block)
                    variables.merge(props)
            elif step.get("type") == "transfer":
                extracted_extractors = self._extract_property_transfers(step)  # label -> extractor
                if extracted_extractors:
                    extractors.merge(extracted_extractors)
            elif step.get("type") == "groovy":
                request = self._extract_script(step)

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

    def _extract_script(self, test_step):
        label = test_step.get("name", "Script")
        script = test_step.find('./con:config/script', namespaces=self.NAMESPACES).text
        if script is not None:
            script = script.strip()
            return {
                "label": label,
                "action": "pause",
                "target": "current-thread",
                "pause-duration": "0ms",
                "jsr223": [{
                    "language": "groovy",
                    "script-text": script,
                }]
            }

    def _extract_test_case(self, test_case, test_suite, suite_level_props):
        case_name = test_case.get("name")
        scenario_name = test_suite.get("name") + "-" + case_name

        case_properties = self._extract_properties(test_case)
        case_properties = {
            "#TestCase#" + key: value
            for key, value in iteritems(case_properties)
            }
        case_level_props = BetterDict.from_dict(suite_level_props)
        case_level_props.merge(case_properties)

        scenario = self._extract_scenario(test_case, case_level_props)
        scenario['test-suite'] = test_suite.get("name")

        return scenario_name, scenario

    def _extract_config(self, project, test_suites, target_test_case=None):
        execution = []
        scenarios = {}

        project_properties = self._extract_properties(project, key_prefix="#Project#")

        for suite in test_suites:
            suite_props = BetterDict.from_dict(project_properties)
            suite_props.merge(self._extract_properties(suite, key_prefix="#TestSuite#"))

            test_cases = suite.findall('.//con:testCase', namespaces=self.NAMESPACES)
            for case in test_cases:
                case_name = case.get("name")
                scenario_name, scenario = self._extract_test_case(case, suite, suite_props)

                load_exec = self._extract_execution(case)
                load_exec['scenario'] = scenario_name
                self.log.debug("Extracted execution for scenario %s", scenario_name)

                if not scenario["requests"]:
                    self.log.warning("No requests extracted for scenario %s, skipping it" % scenario_name)
                    continue

                if target_test_case is None or target_test_case == case_name:
                    self.log.debug("Extracted scenario: %s", scenario_name)
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

        self.interface = project.find('.//con:interface', namespaces=self.NAMESPACES)
        self.log.debug("Found interface: %s", self.interface)

        test_suites = project.findall('.//con:testSuite', namespaces=self.NAMESPACES)
        self.log.debug("Found test suites: %s", test_suites)

        config = self._extract_config(project, test_suites, target_test_case=target_test_case)

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
                self.log.warning("No `test-case` specified for SoapUI project, will use '%s'",
                                 scenario.get("test-case"))
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
