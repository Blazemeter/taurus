"""
Base BZT classes

Copyright 2019 BlazeMeter Inc.

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
import codecs
import logging
import hashlib
import os
import time
from collections import namedtuple

from bzt import TaurusConfigError
from bzt import ToolError

from bzt.utils import numeric_types, Environment, RequiredTool, PIPE, SoapUIScriptConverter
from bzt.utils import to_json, BetterDict, ensure_is_dict, dehumanize_time

from .templates import FileLister
from .dicts import Scenario
from .names import EXEC, SCENARIO


class EngineModule(object):
    """
    Base class for any BZT engine module

    :type engine: Engine
    :type settings: BetterDict
    """

    def __init__(self):
        self.log = logging.getLogger('')
        self.engine = None
        self.settings = BetterDict()
        self.parameters = BetterDict()

    def prepare(self):
        """
        Preparation stage, at which configuration is being read, configs
        and tools being prepared. All long preparations and checks should be
        made here, to make `startup` stage as fast as possible.
        """
        pass

    def startup(self):
        """
        Startup should be as fast as possible. Launch background processes,
        do some API calls for initiation of actual work. Consider making all
        checks and preparations on `prepare` stage.
        """
        pass

    def check(self):
        """
        Check if work should be finished

        :rtype: bool
        :return: True if should be finished
        """
        return False

    def shutdown(self):
        """
        Stop all processes that were started in `startup` stage.
        Should also be as fast as possible, deferring all long operations to
        `post_process` stage.
        """
        pass

    def post_process(self):
        """
        Do all possibly long analysis and processing on run results
        """
        pass

    def _should_run(self):
        """
        Returns True if provisioning matches run-at
        """
        prov = self.engine.config.get(Provisioning.PROV)
        runat = self.parameters.get("run-at", None)
        if runat is not None and prov != runat:
            self.log.debug("Should not run because of non-matching prov: %s != %s", prov, runat)
            return False
        return True


class Provisioning(EngineModule):
    """
    Base class for any provisioning type. Provisioning is the way to
    get the resources that will run the job. For example, local provisoning
    means using local machine to run executors, remote means using
    remote machines with BZT API nodes on them.

    :type executors: list[ScenarioExecutor]
    """
    PROV = "provisioning"

    def __init__(self):
        super(Provisioning, self).__init__()
        self.extend_configs = False
        self.executors = []
        self.disallow_empty_execution = True

    def prepare(self):
        """
        Preparation in provisioning begins with reading executions list
        and instantiating ScenarioExecutor classes for them
        """
        super(Provisioning, self).prepare()

        exc = TaurusConfigError("No 'execution' is configured. Did you forget to pass config files?")
        executions = self.engine.config.get(EXEC, [])
        if not executions and self.disallow_empty_execution:
            raise exc

        for execution in executions:
            instance = self.engine.instantiate_module(execution.get("executor"))
            instance.provisioning = self
            instance.execution = execution
            self.executors.append(instance)


class Reporter(EngineModule):
    """
    This type of modules is responsible for
    in-test and post-test results analysis
    """

    REP = "reporting"

    def should_run(self):
        return self._should_run()


class Service(EngineModule):
    """
    This type of modules is responsible for
    in-test and post-test results analysis
    """

    SERV = "services"

    def should_run(self):
        return self._should_run()


class Aggregator(EngineModule):
    def __init__(self, is_functional):
        super(Aggregator, self).__init__()
        self.is_functional = is_functional


class ScenarioExecutor(EngineModule):
    """
    :type provisioning: engine.Provisioning
    :type execution: BetterDict
    """

    EXEC = EXEC     # backward compatibility
    RAMP_UP = "ramp-up"
    HOLD_FOR = "hold-for"
    CONCURR = "concurrency"
    THRPT = "throughput"
    STEPS = "steps"
    LOAD_FMT = namedtuple("LoadSpec", "concurrency throughput ramp_up hold iterations duration steps")

    def __init__(self):
        super(ScenarioExecutor, self).__init__()
        self.env = Environment(log=self.log)
        self.provisioning = None
        self.execution = BetterDict()  # FIXME: why have this field if we have `parameters` from base class?
        self._cached_scenario = None
        self.label = None
        self.widget = None
        self.reader = None
        self.stdout = None
        self.stderr = None
        self.delay = None
        self.start_time = None
        self.preprocess_args = lambda x: None

    def _get_tool(self, tool, **kwargs):
        instance = tool(env=self.env, log=self.log, http_client=self.engine.get_http_client(), **kwargs)
        assert isinstance(instance, RequiredTool)

        return instance

    def has_results(self):
        if self.reader and self.reader.buffer:
            return True
        else:
            return False

    def get_script_path(self, required=False, scenario=None):
        """
        :type required: bool
        :type scenario: Scenario
        """
        if scenario is None:
            scenario = self.get_scenario()

        if required:
            exc = TaurusConfigError("You must provide script for %s" % self)
            script = scenario.get(Scenario.SCRIPT, exc)
        else:
            script = scenario.get(Scenario.SCRIPT)

        if script:
            script = self.engine.find_file(script)
            scenario[Scenario.SCRIPT] = script

        return script

    def get_scenario(self, name=None):
        """
        Returns scenario dict, extract if scenario is inlined

        :return: DictOfDicts
        """
        if name is None and self._cached_scenario is not None:
            return self._cached_scenario

        scenarios = self.engine.config.get("scenarios", force_set=True)

        label = self._get_scenario_label(name, scenarios)

        exc = TaurusConfigError("Scenario '%s' not found in scenarios: %s" % (label, scenarios.keys()))
        scenario_dict = scenarios.get(label, exc)
        scenario_obj = Scenario(self.engine, scenario_dict)

        if self.engine.provisioning.extend_configs:
            script = self.get_script_path(required=False, scenario=scenario_dict)
            if script and script.lower().endswith('xml'):
                script_content = ''
                try:
                    with codecs.open(script, encoding="UTF-8") as fds:
                        script_content = fds.read()
                except UnicodeDecodeError:
                    pass

                if "con:soapui-project" in script_content:
                    scenario_obj = self._convert_soap_scenario(scenario_obj, script)

        if name is None:
            self._cached_scenario = scenario_obj

        return scenario_obj

    def _convert_soap_scenario(self, scenario_obj, script):
        self.log.info("SoapUI project detected")
        new_scenario_name, scenario_dict = self._extract_scenario_from_soapui(scenario_obj, script)
        self.engine.config["scenarios"].merge({new_scenario_name: scenario_dict})
        prev_scenario_name = self.execution["scenario"]
        self.execution["scenario"] = new_scenario_name
        for execution in self.engine.config.get(EXEC):
            if execution.get(SCENARIO) == prev_scenario_name:
                execution[SCENARIO] = new_scenario_name

        return Scenario(self.engine, scenario_dict)

    def _get_scenario_label(self, name, scenarios):
        if name is None:  # get current scenario
            exc = TaurusConfigError("Scenario is not found in execution: %s" % self.execution)
            label = self.execution.get('scenario', exc)

            is_script = isinstance(label, str) and label not in scenarios and \
                        os.path.exists(self.engine.find_file(label))
            if isinstance(label, list):
                msg = "Invalid content of scenario, list type instead of dict or string: %s"
                raise TaurusConfigError(msg % label)
            if isinstance(label, dict) or is_script:
                self.log.debug("Extract %s into scenarios" % label)
                if isinstance(label, str):
                    scenario = BetterDict.from_dict({Scenario.SCRIPT: label})
                else:
                    scenario = label

                path = self.get_script_path(scenario=Scenario(self.engine, scenario))
                if path:
                    label = os.path.basename(path)
                if not path or label in scenarios:
                    hash_str = str(hashlib.md5(to_json(scenario).encode()).hexdigest())
                    label = 'autogenerated_' + hash_str[-10:]

                scenarios[label] = scenario
                self.execution['scenario'] = label

            self.label = label
        else:  # get scenario by name
            label = name

        return label

    def _extract_scenario_from_soapui(self, base_scenario, script_path):
        test_case = base_scenario.get("test-case", None)
        converter = SoapUIScriptConverter(self.log)
        conv_config = converter.convert_script(script_path)
        conv_scenarios = conv_config["scenarios"]
        scenario_name, conv_scenario = converter.find_soapui_test_case(test_case, conv_scenarios)

        new_name = scenario_name
        counter = 1
        while new_name in self.engine.config["scenarios"]:
            new_name = scenario_name + ("-%s" % counter)
            counter += 1

        if new_name != scenario_name:
            self.log.info("Scenario name '%s' is already taken, renaming to '%s'", scenario_name, new_name)
            scenario_name = new_name

        merged_scenario = BetterDict.from_dict(conv_scenario)
        merged_scenario.merge(base_scenario.data)
        for field in [Scenario.SCRIPT, "test-case"]:
            if field in merged_scenario:
                merged_scenario.pop(field)

        return scenario_name, merged_scenario

    def get_raw_load(self):
        prov_type = self.engine.config.get(Provisioning.PROV)

        for param in (ScenarioExecutor.THRPT, ScenarioExecutor.CONCURR):
            ensure_is_dict(self.execution, param, prov_type)

        throughput = self.execution.get(ScenarioExecutor.THRPT).get(prov_type, None)
        concurrency = self.execution.get(ScenarioExecutor.CONCURR).get(prov_type, None)

        iterations = self.execution.get("iterations", None)

        steps = self.execution.get(ScenarioExecutor.STEPS, None)

        hold = self.execution.get(ScenarioExecutor.HOLD_FOR, None)
        ramp_up = self.execution.get(ScenarioExecutor.RAMP_UP, None)

        return self.LOAD_FMT(concurrency=concurrency, ramp_up=ramp_up, throughput=throughput, hold=hold,
                             iterations=iterations, duration=None, steps=steps)

    def get_load(self):
        """
        Helper method to read load specification
        """

        def eval_int(value):
            try:
                return int(value)
            except (ValueError, TypeError):
                return value

        def eval_float(value):
            try:
                return int(value)
            except (ValueError, TypeError):
                return value

        raw_load = self.get_raw_load()

        iterations = eval_int(raw_load.iterations)
        ramp_up = raw_load.ramp_up

        throughput = eval_float(raw_load.throughput or 0)
        concurrency = eval_int(raw_load.concurrency or 0)

        steps = eval_int(raw_load.steps)
        hold = dehumanize_time(raw_load.hold or 0)

        if ramp_up is None:
            duration = hold
        else:
            ramp_up = dehumanize_time(raw_load.ramp_up)
            duration = hold + ramp_up

        if not iterations:
            if duration:
                iterations = 0  # infinite
            else:
                iterations = 1

        msg = ''
        if not isinstance(concurrency, numeric_types + (type(None),)):
            msg += "Invalid concurrency value[%s]: %s " % (type(concurrency).__name__, concurrency)
        if not isinstance(throughput, numeric_types + (type(None),)):
            msg += "Invalid throughput value[%s]: %s " % (type(throughput).__name__, throughput)
        if not isinstance(steps, numeric_types + (type(None),)):
            msg += "Invalid throughput value[%s]: %s " % (type(steps).__name__, steps)
        if not isinstance(iterations, numeric_types + (type(None),)):
            msg += "Invalid throughput value[%s]: %s " % (type(iterations).__name__, iterations)

        if msg:
            raise TaurusConfigError(msg)

        return self.LOAD_FMT(concurrency=concurrency, ramp_up=ramp_up, throughput=throughput, hold=hold,
                             iterations=iterations, duration=duration, steps=steps)

    def get_resource_files(self):
        files_list = []
        if isinstance(self, FileLister):
            files_list.extend(self.resource_files())
        files_list.extend(self.execution.get("files", []))
        return files_list

    def __repr__(self):
        return "%s/%s" % (self.execution.get("executor", None), self.label if self.label else id(self))

    def prepare(self):
        super(ScenarioExecutor, self).prepare()
        self.env.set(self.execution.get("env"))

    def _execute(self, args, **kwargs):
        self.preprocess_args(args)

        # for compatibility with other executors
        kwargs["stdout"] = kwargs.get("stdout", self.stdout) or PIPE
        kwargs["stderr"] = kwargs.get("stderr", self.stderr) or PIPE

        kwargs["cwd"] = kwargs.get("cwd", None)
        kwargs["env"] = self.env

        self.start_time = time.time()

        try:
            process = self.engine.start_subprocess(args=args, **kwargs)
        except OSError as exc:
            raise ToolError("Failed to start %s: %s (%s)" % (self.__class__.__name__, exc, args))
        return process

    def post_process(self):
        if self.stdout:
            self.stdout.close()
        if self.stderr:
            self.stderr.close()
        super(ScenarioExecutor, self).post_process()

