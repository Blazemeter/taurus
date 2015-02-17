"""
Modules package holds EngineModule implementations
"""
from collections import namedtuple
import logging

from bzt.utils import BetterDict, dehumanize_time, ensure_is_dict


class EngineModule(object):
    """
    Base class for any BZT engine module

    :type engine: bzt.Engine
    :type settings: BetterDict
    """

    def __init__(self):
        self.log = logging.getLogger('')
        self.engine = None
        self.settings = BetterDict()

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

    @staticmethod
    def check_modules_list(modules, require_all=False):
        """
        Helper for bulk check

        :type modules: list
        :param require_all:
        :return:
        """
        finished = require_all
        for module in modules:
            logging.debug("Checking %s", module)
            if require_all:
                finished &= module.check()
            else:
                finished |= module.check()
        return finished


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
        self.executors = []

    def prepare(self):
        """
        Preparation in provisioning begins with reading executions list
        and instantiating ScenarioExecutor classes for them
        """
        super(Provisioning, self).prepare()
        esettings = self.engine.config.get("settings")
        default_executor = esettings.get("default_executor", None)

        if ScenarioExecutor.EXEC not in self.engine.config:
            raise ValueError("No execution is configured")

        executions = self.engine.config.get(ScenarioExecutor.EXEC)
        if not isinstance(executions, list):
            executions = [executions]

        for execution in executions:
            executor = execution.get("executor", default_executor)
            if not executor:
                msg = "Cannot determine executor type and no default executor"
                raise RuntimeError(msg)
            instance = self.engine.instantiate_module(executor)
            instance.provisioning = self
            instance.execution = execution
            self.executors.append(instance)


class Scenario(BetterDict):
    """
    Test scenario entity
    """
    SCRIPT = "script"

    # TODO: add HAR file support
    def get_headers(self):
        """
        Returns global headers

        :rtype: dict[str,str]
        """
        scenario = self
        headers = scenario.get("headers", [])
        return headers

    def get_requests(self):
        """
        Generator object to read requests
        """
        scenario = self
        requests = scenario.get("requests", [])
        for key, val in enumerate(requests):
            ensure_is_dict(requests, key, "url")
            res = namedtuple("HTTPReq",
                             ('url', 'label', 'method', 'headers', 'timeout', 'think_time', 'config', "body"))
            url = requests[key]["url"]
            label = requests[key].get("label", url)
            method = requests[key].get("method", "GET")
            headers = requests[key].get("headers", {})
            timeout = requests[key].get("timeout", None)
            think_time = requests[key].get("think-time", None)

            body = None
            bodyfile = requests[key].get("body-file", None)
            if bodyfile:
                with open(bodyfile) as fhd:
                    body = fhd.read()
            body = requests[key].get("body", body)

            yield res(config=requests[key], label=label,
                      url=url, method=method, headers=headers,
                      timeout=timeout, think_time=think_time, body=body)


class ScenarioExecutor(EngineModule):
    """
    :type provisioning: Provisioning
    :type execution: BetterDict
    """

    CONCURR = "concurrency"
    THRPT = "throughput"
    EXEC = "execution"

    def __init__(self):
        super(ScenarioExecutor, self).__init__()
        self.provisioning = None
        self.execution = BetterDict()
        self.__scenario = None

    def get_scenario(self):
        """
        Returns scenario dict, either inlined, or referenced by alias

        :return: DictOfDicts
        """
        if self.__scenario is None:
            scenario = self.execution.get('scenario', {})
            if isinstance(scenario, basestring):
                scenarios = self.engine.config.get("scenarios")
                if scenario not in scenarios:
                    raise ValueError("Scenario not found in scenarios: %s" % scenario)
                scenario = scenarios.get(scenario)
            self.__scenario = Scenario()
            self.__scenario.merge(scenario)
        return self.__scenario

    def get_load(self):
        """
        Helper method to read load specification

        :return:
        """
        prov_type = self.engine.config.get(Provisioning.PROV, None)

        ensure_is_dict(self.execution, ScenarioExecutor.THRPT, prov_type)
        throughput = self.execution[ScenarioExecutor.THRPT].get(prov_type, 0)

        ensure_is_dict(self.execution, ScenarioExecutor.CONCURR, prov_type)
        concurrency = self.execution[ScenarioExecutor.CONCURR].get(prov_type, 0)

        iterations = self.execution.get("iterations", None)

        ramp_up = self.execution.get("ramp-up", None)
        hold = dehumanize_time(self.execution.get("hold", 0))
        if ramp_up is None:
            ramp_up = None
            duration = hold
        else:
            ramp_up = dehumanize_time(ramp_up)
            duration = hold + ramp_up

        if duration and not iterations:
            iterations = 0  # which means infinite

        res = namedtuple("LoadSpec",
                         ('concurrency', "throughput", 'ramp_up', 'hold',
                          'iterations', 'duration'))
        return res(concurrency=concurrency, ramp_up=ramp_up,
                   throughput=throughput, hold=hold, iterations=iterations,
                   duration=duration)


class Reporter(EngineModule):
    """
    This type of modules is responsible for
    in-test and post-test results analysis
    """

    REP = "reporting"

    def __init__(self):
        super(Reporter, self).__init__()
        self.parameters = BetterDict()


class AggregatorListener(object):
    """
    Mixin for listeners of aggregator data
    """

    def aggregated_second(self, data):
        """
        Notification about new data point

        :param data: bzt.modules.reporting.DataPoint
        """
        raise NotImplementedError()

    def finalize(self):
        """
        This method is called at the end of run
        to close open file descriptors etc.
        """
        pass


