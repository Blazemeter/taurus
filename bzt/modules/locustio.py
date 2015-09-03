"""
Module holds all stuff regarding Grinder tool usage

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
from subprocess import STDOUT
import sys
import math
import time
import os

from bzt.engine import ScenarioExecutor, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.jmeter import JTLReader
from bzt.utils import shutdown_process, shell_exec, RequiredTool, TclLibrary
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.six import PY3
from imp import find_module


class LocustIOExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    def __init__(self):
        super(LocustIOExecutor, self).__init__()
        self.locustfile = None
        self.kpi_jtl = None
        self.process = None
        self.__devnull = None
        self.widget = None
        self.start_time = None

    def prepare(self):
        self.run_checklist()
        self.locustfile = self.get_locust_file()
        self.engine.existing_artifact(self.locustfile)

        self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
        reader = JTLReader(self.kpi_jtl, self.log, None)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(reader)

    def startup(self):
        self.start_time = time.time()
        load = self.get_load()
        hatch = load.concurrency / load.ramp_up if load.ramp_up else load.concurrency
        wrapper = os.path.join(os.path.dirname(__file__), os.pardir, "resources", "locustio-taurus-wrapper.py")
        args = [sys.executable, ]
        args += [os.path.realpath(wrapper), ]
        args += ['-f', os.path.realpath(self.locustfile)]
        args += ['--logfile=%s' % self.engine.create_artifact("locust", ".log")]
        args += ["--no-web", "--only-summary", ]
        args += ["--clients=%d" % load.concurrency, "--hatch-rate=%d" % math.ceil(hatch), ]
        if load.iterations:
            args.append("--num-request=%d" % load.iterations)

        host = self.get_scenario().get("default-address", None)
        if host:
            args.append("--host=%s" % host)

        self.__devnull = open(self.engine.create_artifact("locust", ".out"), 'w')
        env = {"JTL": self.kpi_jtl, "PYTHONPATH": self.engine.artifacts_dir + os.pathsep + os.getcwd()}
        self.process = shell_exec(args, stderr=STDOUT, stdout=self.__devnull, env=env)

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :return:
        """
        if not self.widget:
            if self.locustfile is not None:
                label = "Script: %s" % os.path.basename(self.locustfile)
            else:
                label = None
            self.widget = SidebarWidget(self, label)
        return self.widget

    def check(self):

        if self.widget:
            self.widget.update()

        retcode = self.process.poll()
        if retcode is not None:
            self.log.info("Locust exit code: %s", retcode)
            if retcode != 0:
                raise RuntimeError("Locust exited with non-zero code")

            return True

        return False

    def resource_files(self):
        if not self.locustfile:
            self.locustfile = self.get_locust_file()
        return [os.path.basename(self.locustfile)]

    def get_locust_file(self):
        scenario = self.get_scenario()
        locustfile = scenario.get("script", ValueError("Please specify locusfile in 'script' option"))
        if not os.path.exists(locustfile):
            raise ValueError("Locust file not found: %s" % locustfile)
        return locustfile

    def shutdown(self):
        shutdown_process(self.process, self.log)
        self.__devnull.close()

    def run_checklist(self):
        """
        check tools
        """
        required_tools = []
        required_tools.append(TclLibrary(self.log))
        required_tools.append(LocustIO(self.log))
        self.check_tools(required_tools)

    def check_tools(self, required_tools):
        for tool in required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s", tool.tool_name)
                tool.install()


class LocustIO(RequiredTool):
    def __init__(self, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)
        super(LocustIO, self).__init__("LocustIO", "", "")

    def check_if_installed(self):
        try:
            find_module("locust")
            self.already_installed = True
        except ImportError:
            self.log.error("LocustIO is not installed, see http://docs.locust.io/en/latest/installation.html")
            return False
        return True

    def install(self):
        if PY3:
            raise RuntimeError("LocustIO is not currently compatible with Python 3.x")
        raise RuntimeError("Unable to locate locustio package. Please install it like this: pip install locustio")
