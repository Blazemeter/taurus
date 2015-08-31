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
import os
from subprocess import STDOUT
import sys
import math

from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.jmeter import JTLReader
from bzt.utils import shutdown_process, shell_exec


class LocustIOExecutor(ScenarioExecutor):
    def __init__(self):
        super(LocustIOExecutor, self).__init__()
        self.locustfile = None
        self.kpi_jtl = None
        self.process = None
        self.__devnull = None

    def prepare(self):
        # TODO: check that locust installed and tell how to install it if not present
        scenario = self.get_scenario()
        self.locustfile = scenario.get("script", ValueError("Please specify locusfile in 'script' option"))
        if not os.path.exists(self.locustfile):
            raise ValueError("Locust file not found: %s" % self.locustfile)
        self.engine.existing_artifact(self.locustfile)

        self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
        reader = JTLReader(self.kpi_jtl, self.log, None)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(reader)

    def startup(self):
        load = self.get_load()
        hatch = load.concurrency / load.ramp_up if load.ramp_up else load.concurrency

        args = [sys.executable, ]
        args += [os.path.join(os.path.dirname(__file__), os.pardir, "resources", "locustio-taurus-wrapper.py"), ]
        args += ['-f', self.locustfile]
        args += ['--logfile=%s' % self.engine.create_artifact("locust", ".log")]
        args += ["--no-web", "--only-summary", ]
        args += ["--clients=%d" % load.concurrency, "--hatch-rate=%d" % math.ceil(hatch), ]
        if load.iterations:
            args.append("--num-request=%d" % load.iterations)

        host = self.get_scenario().get("default-address", None)
        if host:
            args.append("--host=%s" % host)

        self.__devnull = open(self.engine.create_artifact("locust", ".out"), 'w')
        self.process = shell_exec(args, stderr=STDOUT, stdout=self.__devnull,
                                  cwd=self.engine.artifacts_dir, env={"JTL": self.kpi_jtl})

    def check(self):
        retcode = self.process.poll()
        if retcode is not None:
            self.log.info("Locust exit code: %s", retcode)
            if retcode != 0:
                raise RuntimeError("Locust exited with non-zero code")

            return True

        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)
        self.__devnull.close()
