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
    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)

    def load(self, path):
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(path)
        except BaseException as exc:
            msg = "XML parsing failed for file %s: %s"
            raise TaurusInternalException(msg % (path, exc))

    def convert(self, script_path, test_case=None):
        if not os.path.exists(script_path):
            raise ValueError("SoapUI script %s doesn't exist" % script_path)

        self.load(script_path)

        namespaces = dict(con="http://eviware.com/soapui/config")
        self.log.debug("Found namespaces: %s", namespaces)

        # project - con:soapui-project
        projects = self.tree.xpath('//con:soapui-project', namespaces=namespaces)
        self.log.debug("Found projects: %s", projects)
        project = projects[0]

        # interface - con:interface (inside project)
        interface = project.find('.//con:interface', namespaces=namespaces)
        self.log.debug("Found interface: %s", interface)

        # test suite - con:testSuite (inside project)
        test_suites = project.findall('.//con:testSuite', namespaces=namespaces)
        self.log.debug("Found test suites: %s", test_suites)

        execution = []
        scenarios = {}
        for suite in test_suites:
            test_cases = suite.findall('.//con:testCase', namespaces=namespaces)
            for case in test_cases:
                scenario_name = suite.get("name") + "-" + case.get("name")
                requests = []
                steps = case.findall('.//con:testStep', namespaces=namespaces)
                for step in steps:
                    config = step.find('./con:config', namespaces=namespaces)
                    service = config.get('service')
                    resource_path = config.get('resourcePath')
                    requests.append({"url": service + resource_path, "label": step.get('name')})
                scenarios[scenario_name] = {"requests": requests}
                self.log.debug("Extracted scenario: %s", scenario_name)

                load_exec = {}
                load_test = case.find('./con:loadTest', namespaces=namespaces)
                if load_test is not None:
                    load_exec['concurrency'] = int(load_test.find('./con:threadCount', namespaces).text)
                    load_exec['hold-for'] = int(load_test.find('./con:testLimit', namespaces).text)
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
