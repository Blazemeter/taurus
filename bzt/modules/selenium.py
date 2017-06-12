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
import copy
import time
from abc import abstractmethod

import os
from bzt import TaurusConfigError
from urwid import Text, Pile

from bzt.engine import FileLister, Service
from bzt.modules import ReportableExecutor
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.services import VirtualDisplay
from bzt.utils import BetterDict, get_files_recursive, get_full_path


class AbstractSeleniumExecutor(ReportableExecutor):
    # FIXME: deprecated, as it's replaced with virtual-display service
    SHARED_VIRTUAL_DISPLAY = {}

    @abstractmethod
    def get_virtual_display(self):
        """
        Return virtual display instance, if any.
        :return:
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
    :type runner: SubprocessedExecutor
    :type virtual_display_service: VirtualDisplay
    """

    SUPPORTED_RUNNERS = ["nose", "junit", "testng", "rspec", "mocha"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.additional_env = {}
        self.end_time = None
        self.runner = None
        self.script = None
        self.generated_methods = BetterDict()
        self.runner_working_dir = None
        self.virtual_display_service = Service()  # TODO: remove compatibility with deprecated virtual-display setting

    def add_env(self, env):
        self.additional_env.update(env)

    def get_runner_working_dir(self):
        if self.runner_working_dir is None:
            self.runner_working_dir = self.engine.create_artifact("classes", "")
        return self.runner_working_dir

    def create_runner(self):
        runner_type = self.get_runner_type()
        self.runner = self.engine.instantiate_module(runner_type)

        # todo: deprecated, remove it later
        self.runner.settings.merge(self.settings.get('selenium-tools').get(runner_type))

        self.runner.parameters = self.parameters
        self.runner.provisioning = self.provisioning
        self.runner.execution = self.execution
        self.runner.execution['executor'] = runner_type

        if runner_type == "nose":
            self.runner.execution["test-mode"] = "selenium"

    def get_virtual_display(self):
        if isinstance(self.virtual_display_service, VirtualDisplay):
            return self.virtual_display_service.get_virtual_display()

    def prepare(self):
        if self.get_load().concurrency and self.get_load().concurrency > 1:
            msg = 'Selenium supports concurrency in cloud provisioning mode only\n'
            msg += 'For details look at http://gettaurus.org/docs/Cloud.md'
            self.log.warning(msg)

        # backwards-compatible virtual-display settings
        vd_conf = self.settings.get("virtual-display")
        if vd_conf:
            self.log.warning("Configuring virtual-display in Selenium module settings is deprecated."
                             " Use the service approach instead")
            service_conf = copy.deepcopy(vd_conf)
            service_conf["module"] = "virtual-display"
            self.virtual_display_service = VirtualDisplay()
            self.virtual_display_service.parameters.merge(service_conf)
            self.virtual_display_service.prepare()

        self.create_runner()
        self.runner.prepare()
        self.script = self.runner.script

    def get_runner_type(self):
        if "runner" in self.execution:
            runner = self.execution["runner"]
            if runner not in SeleniumExecutor.SUPPORTED_RUNNERS:
                msg = "Runner '%s' is not supported. Supported runners: %s"
                raise TaurusConfigError(msg % (runner, SeleniumExecutor.SUPPORTED_RUNNERS))
            self.log.debug("Using script type: %s", runner)
            return runner

        script_name = self.get_script_path()
        if script_name:
            return self.detect_script_type(script_name)
        else:
            if "requests" in self.get_scenario():
                return "nose"
            else:
                raise TaurusConfigError("You must specify either script or list of requests to run Selenium")

    def resource_files(self):
        self.create_runner()
        return self.runner.resource_files()

    def detect_script_type(self, script_name):
        if not os.path.exists(script_name):
            raise TaurusConfigError("Script '%s' doesn't exist" % script_name)

        file_types = set()

        # gather file extensions and choose script_type according to priority
        if os.path.isfile(script_name):  # regular file received
            file_types.add(os.path.splitext(script_name)[1].lower())
        else:  # dir received: check contained files
            for file_name in get_files_recursive(script_name):
                file_types.add(os.path.splitext(file_name)[1].lower())

        if '.java' in file_types or '.jar' in file_types:
            # todo: next detection logic is duplicated in TestNGTester - can we avoid it?
            script_dir = get_full_path(self.get_script_path(), step_up=1)
            if os.path.exists(os.path.join(script_dir, 'testng.xml')) or self.execution.get('testng-xml', None):
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
            raise TaurusConfigError("Supported script files not found, script detection is failed")

        self.log.debug("Detected script type: %s", script_type)

        return script_type

    def startup(self):
        """
        Start runner
        :return:
        """
        self.virtual_display_service.startup()
        self.start_time = time.time()
        self.runner.env.update(self.additional_env)
        self.runner.startup()

    def check(self):
        """
        check if test completed
        :return:
        """
        self.virtual_display_service.check()

        if self.widget:
            self.widget.update()

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
        self.virtual_display_service.shutdown()

        self.runner.shutdown()
        self.report_test_duration()

    def post_process(self):
        self.virtual_display_service.post_process()
        self.runner.post_process()

        if os.path.exists("geckodriver.log"):
            self.engine.existing_artifact("geckodriver.log", True)

    def has_results(self):
        return self.runner.has_results()

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.runner.stdout_file)
        return self.widget


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
