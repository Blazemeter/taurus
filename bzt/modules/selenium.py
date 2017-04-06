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
import json
import time
from abc import abstractmethod

import os
from bzt import TaurusConfigError, TaurusInternalException
from urwid import Text, Pile

from bzt.engine import Scenario, FileLister, SubprocessedExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.functional import FunctionalAggregator, FuncSamplesReader, LoadSamplesReader
from bzt.modules.python import NoseTester
from bzt.utils import is_windows, BetterDict, get_full_path, get_files_recursive

try:
    from pyvirtualdisplay.smartdisplay import SmartDisplay as Display
except ImportError:
    from pyvirtualdisplay import Display


class AbstractSeleniumExecutor(SubprocessedExecutor):  # NOTE: just for compatibility
    SHARED_VIRTUAL_DISPLAY = {}

    @abstractmethod
    def get_virtual_display(self):
        """
        Return virtual display instance used by this executor.
        :rtype: Display
        """
        pass

    @abstractmethod
    def add_env(self, env):
        """
        Add environment variables into selenium process env
        :type env: dict[str,str]
        """
        pass


class SeleniumExecutor(AbstractSeleniumExecutor, WidgetProvider, FileLister):
    """
    Selenium executor
    :type virtual_display: Display
    :type runner: SubprocessedExecutor
    """

    SUPPORTED_RUNNERS = ["nose", "junit", "testng", "rspec", "mocha"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.additional_env = {}
        self.virtual_display = None
        self.end_time = None
        self.runner = None
        self.report_file = None
        self.scenario = None
        self.script = None
        self.generated_methods = BetterDict()
        self.runner_working_dir = None
        self.register_reader = True

    def get_virtual_display(self):
        return self.virtual_display

    def add_env(self, env):
        self.additional_env.update(env)

    def set_virtual_display(self):
        display_conf = self.settings.get("virtual-display")
        if display_conf:
            if is_windows():
                self.log.warning("Cannot have virtual display on Windows, ignoring")
            else:
                if self.engine in SeleniumExecutor.SHARED_VIRTUAL_DISPLAY:
                    self.virtual_display = SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine]
                else:
                    width = display_conf.get("width", 1024)
                    height = display_conf.get("height", 768)
                    self.virtual_display = Display(size=(width, height))
                    msg = "Starting virtual display[%s]: %s"
                    self.log.info(msg, self.virtual_display.size, self.virtual_display.new_display_var)
                    self.virtual_display.start()
                    SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine] = self.virtual_display

    def free_virtual_display(self):
        if self.virtual_display and self.virtual_display.is_alive():
            self.virtual_display.stop()
        if self.engine in SeleniumExecutor.SHARED_VIRTUAL_DISPLAY:
            del SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine]

    def get_runner_working_dir(self):
        if self.runner_working_dir is None:
            self.runner_working_dir = self.engine.create_artifact("classes", "")
        return self.runner_working_dir

    def _get_testng_xml(self):
        if 'testng-xml' in self.scenario:
            testng_xml = self.scenario.get('testng-xml')
            if testng_xml:
                return testng_xml
            else:
                return None  # empty value for switch off testng.xml path autodetect

        script_path = self.get_script_path()
        if script_path:
            script_dir = get_full_path(script_path, step_up=1)
            testng_xml = os.path.join(script_dir, 'testng.xml')
            if os.path.exists(testng_xml):
                self.log.info("Detected testng.xml file at %s", testng_xml)
                self.scenario['testng-xml'] = testng_xml
                return testng_xml

        return None

    def _create_runner(self, report_file):
        script_type = self.detect_script_type()
        runner = self.engine.instantiate_module(script_type)
        runner.parameters = self.parameters
        runner.provisioning = self.provisioning
        runner.execution = self.execution

        if script_type == "nose":
            runner.generated_methods = self.generated_methods
        elif script_type == "junit":
            pass
        elif script_type == "testng":
            testng_config = self._get_testng_xml()
            if testng_config:
                runner.execution['testng-xml'] = self.engine.find_file(testng_config)
        elif script_type == "rspec":
            pass
        elif script_type == "mocha":
            pass
        else:
            raise TaurusConfigError("Unsupported script type: %s" % script_type)

        runner.execution["report-file"] = report_file  # TODO: shouldn't it be the field?
        return runner

    def _register_reader(self, report_file):
        if self.engine.is_functional_mode():
            reader = FuncSamplesReader(report_file, self.engine, self.log, self.generated_methods)
            if isinstance(self.engine.aggregator, FunctionalAggregator):
                self.engine.aggregator.add_underling(reader)
        else:
            reader = LoadSamplesReader(report_file, self.log, self.generated_methods)
            if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                self.engine.aggregator.add_underling(reader)
        return reader

    def prepare(self):
        if self.get_load().concurrency and self.get_load().concurrency > 1:
            msg = 'Selenium supports concurrency in cloud provisioning mode only\n'
            msg += 'For details look at http://gettaurus.org/docs/Cloud.md'
            self.log.warning(msg)
        self.set_virtual_display()
        self.scenario = self.get_scenario()
        self.script = self.get_script_path()

        self.report_file = self.engine.create_artifact("selenium_tests_report", ".ldjson")
        self.runner = self._create_runner(self.report_file)
        self.runner.prepare()
        if isinstance(self.runner, NoseTester):
            self.script = self.runner._script
        if self.register_reader:
            self.reader = self._register_reader(self.report_file)

    def detect_script_type(self):
        if not self.script and "requests" in self.scenario:
            return "nose"

        if not os.path.exists(self.script):
            raise TaurusConfigError("Script '%s' doesn't exist" % self.script)

        if "runner" in self.execution:
            runner = self.execution["runner"]
            if runner not in SeleniumExecutor.SUPPORTED_RUNNERS:
                msg = "Runner '%s' is not supported. Supported runners: %s"
                raise TaurusConfigError(msg % (runner, SeleniumExecutor.SUPPORTED_RUNNERS))
            self.log.debug("Using script type: %s", runner)
            return runner

        file_types = set()

        if os.path.isfile(self.script):  # regular file received
            file_types.add(os.path.splitext(self.script)[1].lower())
        else:  # dir received: check contained files
            for file_name in get_files_recursive(self.script):
                file_types.add(os.path.splitext(file_name)[1].lower())

        if '.java' in file_types or '.jar' in file_types:
            if self._get_testng_xml() is not None:
                script_type = 'testng'
            else:
                script_type = 'junit'
        elif '.py' in file_types:
            script_type = 'nose'
        elif '.rb' in file_types:
            script_type = 'rspec'
        elif '.js' in file_types:
            script_type = 'mocha'
        else:
            raise TaurusConfigError("Unsupported script type: %s" % self.script)

        self.log.debug("Detected script type: %s", script_type)

        return script_type

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        self.runner.env = self.additional_env
        self.runner.startup()

    def check_virtual_display(self):
        if self.virtual_display:
            if not self.virtual_display.is_alive():
                self.log.info("Virtual display out: %s", self.virtual_display.stdout)
                self.log.warning("Virtual display err: %s", self.virtual_display.stderr)
                raise TaurusInternalException("Virtual display failed: %s" % self.virtual_display.return_code)

    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            self.widget.update()

        self.check_virtual_display()

        return self.runner.check()

    def report_test_duration(self):
        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium tests ran for %s seconds", self.end_time - self.start_time)

    def shutdown(self):
        """
        shutdown test_runner
        :return:
        """
        self.runner.shutdown()
        self.report_test_duration()

    def post_process(self):
        if os.path.exists("geckodriver.log"):
            self.engine.existing_artifact("geckodriver.log", True)
        self.free_virtual_display()

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.runner._stdout_file)
        return self.widget

    def resource_files(self):
        resources = []

        self.scenario = self.get_scenario()
        script = self.scenario.get(Scenario.SCRIPT, None)
        if script:
            resources.append(script)

        resources.extend(self.scenario.get("additional-classpath", []))
        resources.extend(self.settings.get("additional-classpath", []))

        testng_config = self._get_testng_xml()
        if testng_config:
            resources.append(testng_config)

        return resources


class SeleniumWidget(Pile, PrioritizedWidget):
    def __init__(self, script, runner_output):
        widgets = []
        self.script_name = Text("Selenium: %s" % os.path.basename(script))
        self.summary_stats = Text("Delayed...")
        self.runner_output = runner_output
        widgets.append(self.script_name)
        widgets.append(self.summary_stats)
        super(SeleniumWidget, self).__init__(widgets)
        PrioritizedWidget.__init__(self, priority=10)

    def update(self):
        reader_summary = ''
        if self.runner_output is not None and os.path.exists(self.runner_output):
            with open(self.runner_output, "rt") as fds:
                lines = fds.readlines()
                if lines:
                    line = lines[-1]
                    if not line.endswith("\n") and len(lines) > 1:
                        line = lines[-2]
                    if line and "," in line:
                        reader_summary = line.split(",")[-1]

        if reader_summary:
            self.summary_stats.set_text(reader_summary)
        else:
            self.summary_stats.set_text('In progress...')

        self._invalidate()
