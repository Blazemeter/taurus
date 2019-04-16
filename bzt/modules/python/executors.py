"""
Copyright 2018 BlazeMeter Inc.

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
import shlex
import sys
import copy

import yaml

from bzt import TaurusConfigError
from bzt.engine import HavingInstallableTools, SETTINGS
from bzt.modules import SubprocessedExecutor, ConsolidatingAggregator, FuncSamplesReader
from bzt.modules.functional import FunctionalResultsReader
from bzt.modules.jmeter import JTLReader
from bzt.six import string_types, text_type
from bzt.utils import FileReader, get_full_path, RESOURCES_DIR, BZT_DIR
from .generators import ApiritifScriptGenerator
from .tools import TaurusPytestRunner, TaurusRobotRunner, Robot

IGNORED_LINE = re.compile(r"[^,]+,Total:\d+ Passed:\d+ Failed:\d+")


class ApiritifNoseExecutor(SubprocessedExecutor):
    """
    :type _tailer: FileReader
    """

    def __init__(self):
        super(ApiritifNoseExecutor, self).__init__()
        self._tailer = FileReader(file_opener=lambda _: None, parent_logger=self.log)

    def resource_files(self):
        files = super(ApiritifNoseExecutor, self).resource_files()
        for source in self.get_scenario().get_data_sources():
            files.append(source['path'])

        return files

    def create_func_reader(self, report_file):
        del report_file
        return ApiritifFuncReader(self.engine, self.log)

    def create_load_reader(self, report_file):
        del report_file
        reader = ApiritifLoadReader(self.log)
        reader.engine = self.engine
        return reader

    def prepare(self):
        super(ApiritifNoseExecutor, self).prepare()
        self.script = self.get_script_path()
        if not self.script:
            if "requests" in self.get_scenario():
                self.script = self.__tests_from_requests()
            else:
                raise TaurusConfigError("Nothing to test, no requests were provided in scenario")

        # todo: requred tools?

        # path to taurus dir. It's necessary for bzt usage inside tools/helpers
        self.env.add_path({"PYTHONPATH": get_full_path(BZT_DIR, step_up=1)})

        self.reporting_setup()  # no prefix/suffix because we don't fully control report file names

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        test_mode = self.execution.get("test-mode", "apiritif")
        scenario = self.get_scenario()

        if test_mode == "apiritif":
            builder = ApiritifScriptGenerator(self.engine, scenario, self.label, self.log, test_mode=test_mode)
            builder.verbose = self.__is_verbose()
        else:
            wdlog = self.engine.create_artifact('webdriver', '.log')

            generate_markers = self.settings.get('generate-flow-markers', None)
            generate_markers = scenario.get('generate-flow-markers', generate_markers)

            capabilities = copy.deepcopy(self.settings.get("capabilities"))
            capabilities.merge(copy.deepcopy(self.execution.get("capabilities")))

            scenario_caps = copy.deepcopy(scenario.get("capabilities"))

            # todo: just for legacy support, remove it later
            if isinstance(scenario_caps, list):
                self.log.warning("Obsolete format of capabilities found (list), should be dict")
                scenario_caps = {item.keys()[0]: item.values()[0] for item in scenario_caps}

            capabilities.merge(scenario_caps)

            remote = self.settings.get("remote", None)
            remote = self.execution.get("remote", remote)
            remote = scenario.get("remote", remote)

            builder = ApiritifScriptGenerator(
                self.engine, scenario, self.label, self.log, wdlog,
                utils_file=os.path.join(RESOURCES_DIR, "selenium_taurus_extras.py"),
                ignore_unknown_actions=self.settings.get("ignore-unknown-actions", False),
                generate_markers=generate_markers,
                capabilities=capabilities,
                wd_addr=remote, test_mode=test_mode)

        builder.build_source_code()
        builder.save(filename)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator) and isinstance(builder, ApiritifScriptGenerator):
            self.engine.aggregator.ignored_labels.extend(builder.service_methods)
        return filename

    def startup(self):
        executable = self.settings.get("interpreter", sys.executable)

        report_type = ".ldjson" if self.engine.is_functional_mode() else ".csv"
        report_tpl = self.engine.create_artifact("apiritif", ".") + "%s" + report_type
        cmdline = [executable, "-m", "apiritif.loadgen", '--result-file-template', report_tpl]

        load = self.get_load()
        if load.concurrency:
            cmdline += ['--concurrency', str(load.concurrency)]

        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            cmdline += ['--hold-for', str(load.hold)]

        if load.ramp_up:
            cmdline += ['--ramp-up', str(load.ramp_up)]

        if load.steps:
            cmdline += ['--steps', str(load.steps)]

        if self.__is_verbose():
            cmdline += ['--verbose']

        cmdline += [self.script]
        self.process = self._execute(cmdline)
        self._tailer = FileReader(filename=self.stdout.name, parent_logger=self.log)

    def has_results(self):
        if not self.reader:
            return False
        return self.reader.read_records

    @staticmethod
    def _normalize_label(label):
        for char in ":/":
            if char in label:
                label = label.replace(char, '_')
        return label

    def _check_stdout(self):
        for line in self._tailer.get_lines():
            if "Adding worker" in line:
                marker = "results="
                pos = line.index(marker)
                fname = line[pos + len(marker):].strip()
                self.log.debug("Adding result reader for %s", fname)
                self.reader.register_file(fname)
            elif "Transaction started" in line:
                colon = line.index('::')
                values = {
                    part.split('=')[0]: part.split('=')[1]
                    for part in line[colon + 2:].strip().split(',')
                }
                label = self._normalize_label(values['name'])
                start_time = float(values['start_time'])
                self.transaction_started(label, start_time)
            elif "Transaction ended" in line:
                colon = line.index('::')
                values = {
                    part.split('=')[0]: part.split('=')[1]
                    for part in line[colon + 2:].strip().split(',')
                }
                label = self._normalize_label(values['name'])
                duration = float(values['duration'])
                self.transacion_ended(label, duration)

    def check(self):
        self._check_stdout()
        return super(ApiritifNoseExecutor, self).check()

    def __log_lines(self):
        lines = []
        for line in self._tailer.get_lines():
            if not IGNORED_LINE.match(line):
                lines.append(line)

        if lines:
            self.log.info("\n".join(lines))

    def post_process(self):
        self._check_stdout()
        self.__log_lines()
        self._tailer.close()
        super(ApiritifNoseExecutor, self).post_process()

    def __is_verbose(self):
        engine_verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        executor_verbose = self.settings.get("verbose", engine_verbose)
        return executor_verbose


class NoseTester(ApiritifNoseExecutor):
    pass


class ApiritifLoadReader(ConsolidatingAggregator):
    def __init__(self, parent_log):
        super(ApiritifLoadReader, self).__init__()
        self.log = parent_log.getChild(self.__class__.__name__)

    def register_file(self, report_filename):
        reader = JTLReader(report_filename, self.log)
        self.add_underling(reader)

    @property
    def read_records(self):
        for reader in self.underlings:  # type: JTLReader
            if reader.read_records > 0:
                return True
        return False


class ApiritifFuncReader(FunctionalResultsReader):
    def __init__(self, engine, parent_log):
        super(ApiritifFuncReader, self).__init__()
        self.engine = engine
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filenames = []
        self.readers = []
        self.read_records = False

    def register_file(self, report_filename):
        self.filenames.append(report_filename)
        reader = FuncSamplesReader(report_filename, self.engine, self.log)
        self.readers.append(reader)

    def read(self, last_pass=False):
        for reader in self.readers:
            for sample in reader.read(last_pass):
                if not self.read_records:
                    self.read_records = True
                yield sample


class PyTestExecutor(SubprocessedExecutor, HavingInstallableTools):
    def __init__(self):
        super(PyTestExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "pytest_runner.py")
        self._tailer = FileReader('', file_opener=lambda _: None, parent_logger=self.log)
        self._additional_args = []

    def prepare(self):
        super(PyTestExecutor, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for pytest executor")

        scenario = self.get_scenario()
        if "additional-args" in scenario:
            argv = scenario.get("additional-args")
            self._additional_args = shlex.split(argv)

        self.reporting_setup(suffix=".ldjson")

    def __is_verbose(self):
        engine_verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        executor_verbose = self.settings.get("verbose", engine_verbose)
        return executor_verbose

    def install_required_tools(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warning("You are using Python 3, make sure that your scripts are able to run in Python 3")

        self._check_tools([self._get_tool(TaurusPytestRunner, tool_path=self.runner_path)])

    def startup(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)

        cmdline = [executable, self.runner_path, '--report-file', self.report_file]

        load = self.get_load()
        if load.iterations:
            cmdline += ['-i', str(load.iterations)]

        if load.hold:
            cmdline += ['-d', str(load.hold)]

        cmdline += self._additional_args
        cmdline += [self.script]

        self.process = self._execute(cmdline)

        if self.__is_verbose():
            self._tailer = FileReader(filename=self.stdout.name, parent_logger=self.log)

    def check(self):
        self.__log_lines()
        return super(PyTestExecutor, self).check()

    def post_process(self):
        super(PyTestExecutor, self).post_process()
        self.__log_lines()

    def __log_lines(self):
        lines = []
        for line in self._tailer.get_lines():
            if not IGNORED_LINE.match(line):
                lines.append(line)

        if lines:
            self.log.info("\n".join(lines))


class RobotExecutor(SubprocessedExecutor, HavingInstallableTools):
    def __init__(self):
        super(RobotExecutor, self).__init__()
        self.runner_path = os.path.join(RESOURCES_DIR, "robot_runner.py")
        self.variables_file = None
        self.tags = None

    def resource_files(self):
        files = super(RobotExecutor, self).resource_files()
        scenario = self.get_scenario()
        if "variables" in scenario and isinstance(scenario["variables"], (string_types, text_type)):
            files.append(scenario["variables"])
        return files

    def prepare(self):
        super(RobotExecutor, self).prepare()
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for robot executor")

        self.reporting_setup(suffix=".ldjson")

        scenario = self.get_scenario()
        variables = scenario.get("variables")
        if variables:
            if isinstance(variables, (string_types, text_type)):
                self.variables_file = get_full_path(variables)
            elif isinstance(variables, dict):
                self.variables_file = self.engine.create_artifact("robot-vars", ".yaml")
                with open(self.variables_file, 'wb') as fds:
                    yml = yaml.safe_dump(variables,
                                         default_flow_style=False, explicit_start=True, canonical=False,
                                         allow_unicode=True,
                                         encoding='utf-8', width=float("inf"))
                    fds.write(yml)
            else:
                raise TaurusConfigError("`variables` is neither file nor dict")
        tags = scenario.get("tags", None)
        if tags:
            if isinstance(tags, (string_types, text_type)):
                self.tags = tags
            else:
                raise TaurusConfigError("`tags` is not a string or text")

    def install_required_tools(self):
        tools = [self._get_tool(TaurusRobotRunner, tool_path=self.runner_path),
                 self._get_tool(Robot, python=self.settings.get("interpreter", sys.executable))]
        self._check_tools(tools)

    def startup(self):
        executable = self.settings.get("interpreter", sys.executable)

        cmdline = [executable, self.runner_path, '--report-file', self.report_file]

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            cmdline += ['--duration', str(load.hold)]

        if self.variables_file is not None:
            cmdline += ['--variablefile', self.variables_file]

        if self.tags is not None:
            cmdline += ['--include', self.tags]

        cmdline += [self.script]
        self.process = self._execute(cmdline)
