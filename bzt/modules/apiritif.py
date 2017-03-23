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
import os
import re
import sys

from bzt import ToolError, TaurusConfigError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.functional import FunctionalAggregator
from bzt.modules.selenium import FuncSamplesReader, LoadSamplesReader, SeleniumWidget
from bzt.requests_model import HTTPRequest
from bzt.utils import get_full_path, shutdown_process, PythonGenerator


class ApiritifExecutor(ScenarioExecutor):
    def __init__(self):
        super(ApiritifExecutor, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2), "resources", "nose_plugin.py")
        self.process = None
        self.stdout_path = None
        self.stderr_path = None
        self.stdout_file = None
        self.stderr_file = None
        self.script = None
        self.report_path = None
        self.generated_script = None

    def prepare(self):
        scenario = self.get_scenario()
        if "requests" in scenario:
            self.script = self._generate_script(scenario)
            self.generated_script = True
        elif "script" in scenario:
            self.script = self.get_script_path()
            self.generated_script = False
        else:
            raise TaurusConfigError("You must specify either 'requests' or 'script' for Apiritif")

        self.stdout_path = self.engine.create_artifact("nose", ".out")
        self.stderr_path = self.engine.create_artifact("nose", ".err")
        self.report_path = self.engine.create_artifact("report", ".ldjson")

        if self.engine.is_functional_mode():
            self.reader = FuncSamplesReader(self.report_path, self.log, [])
            if isinstance(self.engine.aggregator, FunctionalAggregator):
                self.engine.aggregator.add_underling(self.reader)
        else:
            self.reader = LoadSamplesReader(self.report_path, self.log, [])
            if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        load = self.get_load()
        executable = self.settings.get("interpreter", sys.executable)
        nose_command_line = [executable, self.plugin_path, '--report-file', self.report_path]

        if load.iterations:
            nose_command_line += ['-i', str(load.iterations)]

        if load.hold:
            nose_command_line += ['-d', str(load.hold)]

        nose_command_line += [self.script]

        self.stdout_file = open(self.stdout_path, "wt")
        self.stderr_file = open(self.stderr_path, "wt")
        self.process = self.execute(nose_command_line, stdout=self.stdout_file, stderr=self.stderr_file)

    def check(self):
        if self.widget:
            self.widget.update()

        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                with open(self.stderr_path) as fds:
                    std_err = fds.read()
                msg = "Nose %s (%s) has failed with retcode %s \n %s"
                raise ToolError(msg % (self.label, self.__class__.__name__, ret_code, std_err.strip()))
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)
        if self.stdout_file:
            self.stdout_file.close()
        if self.stderr_file:
            self.stderr_file.close()

    def post_process(self):
        pass

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.stdout_path)
        return self.widget

    def _generate_script(self, scenario):
        test_file = self.engine.create_artifact("test_api", ".py")
        test_gen = ApiritifScriptBuilder(scenario, self.log)
        test_gen.build_source_code()
        test_gen.save(test_file)
        return test_file


class ApiritifScriptBuilder(PythonGenerator):
    IMPORTS = """\
import apiritif

"""

    def __init__(self, scenario, parent_logger):
        super(ApiritifScriptBuilder, self).__init__(scenario, parent_logger)

    def build_source_code(self):
        self.log.debug("Generating Test Case test methods")
        imports = self.add_imports()
        self.root.append(imports)
        test_class = self.gen_class_definition("TestRequests", ["apiritif.APITestCase"])
        self.root.append(test_class)

        for index, req in enumerate(self.scenario.get_requests()):
            if not isinstance(req, HTTPRequest):
                msg = "Apiritif script generator doesn't support '%s' blocks, skipping"
                self.log.warning(msg, req.NAME)
                continue

            mod_label = re.sub('[^0-9a-zA-Z]+', '_', req.url[:30])
            method_name = 'test_%05d_%s' % (index, mod_label)
            test_method = self.gen_test_method(method_name)

            self._add_url_request(req, test_method)

            test_class.append(test_method)
            test_method.append(self.gen_new_line())

    def _add_url_request(self, req, test_method):
        test_method.append(self.gen_statement("response = self.get(%r)" % req.url))
        test_method.append(self.gen_statement("self.assertOk(response)"))

    def gen_test_method(self, name):
        self.log.debug("Generating test method %s", name)
        test_method = self.gen_method_definition(name, ["self"])
        return test_method
