"""
Main BZT classes

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
from collections import namedtuple
from collections import defaultdict
import copy
import datetime
import json
import logging
import os
import shutil
import tempfile
import time
import traceback
from json import encoder

import psutil
import six
import yaml
from yaml.representer import SafeRepresenter

from bzt import ManualShutdown, NormalShutdown
from bzt.utils import load_class, to_json, BetterDict, ensure_is_dict, dehumanize_time


try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser

try:
    from UserDict import DictMixin
except ImportError:
    from collections import MutableMapping as DictMixin


class Engine(object):
    """
    Core entity of the technology, used to coordinate whole process

    :type reporters: list[Reporter]
    :type log: logging.Logger
    :type aggregator: bzt.modules.aggregator.ConsolidatingAggregator
    :type stopping_reason: BaseException
    """

    def __init__(self, parent_logger):
        """

        :type parent_logger: logging.Logger
        """
        self.__artifacts = []
        self.reporters = []
        self.file_search_path = None
        self.artifacts_base_dir = os.getcwd()
        self.artifacts_dir = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.config = Configuration()
        self.config.log = self.log.getChild(Configuration.__name__)
        self.modules = {}
        self.provisioning = Provisioning()
        self.aggregator = EngineModule()
        self.interrupted = False
        self.check_interval = 1
        self.stopping_reason = None

        self.__counters_ts = None

    def configure(self, user_configs):
        """
        Load configuration files
        """
        self.log.info("Configuring...")
        self._create_artifacts_dir()
        dump = self.create_artifact("effective", "")  # FIXME: not good since this file not exists
        self.config.set_dump_file(dump)
        self.__load_configs(user_configs)

    def prepare(self):
        """
        Prepare engine for work, will call preparing of Provisioning and add
        downstream EngineModule instances
        """
        self.log.info("Preparing...")
        self.__prepare_provisioning()
        self.__prepare_reporters()

        interval = self.config.get("settings").get("check-interval", self.check_interval)
        self.check_interval = dehumanize_time(interval)

        self.config.dump()

    def run(self):
        """
        Run the job. Calls `startup`, does periodic `check`,
        calls `shutdown` in any case
        """
        self.log.info("Starting...")
        try:
            if self.aggregator:
                self.aggregator.startup()

            for module in self.reporters:
                module.startup()

            self.provisioning.startup()
            self.config.dump()
            self.__wait()
        except NormalShutdown as exc:
            self.log.debug("Normal shutdown called: %s", traceback.format_exc())
            self.stopping_reason = exc if not self.stopping_reason else self.stopping_reason
        except BaseException as exc:
            self.stopping_reason = exc if not self.stopping_reason else self.stopping_reason
            raise
        finally:
            self.__shutdown()

    def __wait(self):
        self.log.info("Waiting for finish...")
        prev = time.time()
        while not self.provisioning.check() \
                and not self.aggregator.check() \
                and not EngineModule.check_modules_list(self.reporters):
            now = time.time()
            diff = now - prev
            delay = self.check_interval - diff
            self.log.debug("Iteration took %.3f sec, sleeping for %.3f sec...", diff, delay)
            if delay > 0:
                time.sleep(delay)
            prev = time.time()
            if self.interrupted:
                raise ManualShutdown()
        self.config.dump()

    def __shutdown(self):
        self.log.info("Shutting down...")
        try:
            self.provisioning.shutdown()
            self.aggregator.shutdown()

            exception = None
            for module in self.reporters:
                try:
                    module.shutdown()
                except BaseException as exc:
                    self.log.error("Error while shutting down: %s", traceback.format_exc())
                    self.stopping_reason = exc if not self.stopping_reason else self.stopping_reason
                    if not exception:
                        exception = exc

                if exception:
                    raise exception
        except BaseException as exc:
            self.log.error("Error while shutting down: %s", traceback.format_exc())
            self.stopping_reason = exc if not self.stopping_reason else self.stopping_reason
            raise
        finally:
            self.config.dump()

    def post_process(self):
        """
        Do post-run analysis and processing for the results.
        """
        self.log.info("Post-processing...")
        exception = None
        try:
            for module in [self.provisioning, self.aggregator] + self.reporters:
                try:
                    module.post_process()
                except KeyboardInterrupt as exc:
                    self.log.error("Shutdown: %s", exc)
                    self.stopping_reason = exc if not self.stopping_reason else self.stopping_reason
                    if not exception:
                        exception = exc
                except BaseException as exc:
                    self.log.error("Error while post-processing: %s", traceback.format_exc())
                    self.stopping_reason = exc if not self.stopping_reason else self.stopping_reason
                    if not exception:
                        exception = exc
        finally:
            self.__finalize()

        self.config.dump()

        if exception:
            self.log.debug("Exception in post-process: %s", traceback.format_exc())
            self.stopping_reason = exception if not self.stopping_reason else self.stopping_reason

        if isinstance(exception, KeyboardInterrupt):
            raise exception
        elif exception:
            raise RuntimeError("Failed post-processing, see errors above")

    def __finalize(self):
        """
        Finalize the Engine. For example, copy artifacts
        into artifacts directory
        """
        pass

    def create_artifact(self, prefix, suffix):
        """
        Create new artifact in artifacts dir with given prefix and suffix

        :type prefix: str
        :type suffix: str
        :return: Path to created file
        :rtype: str
        :raise ValueError: if no artifacts dir set
        """
        if not self.artifacts_dir:
            raise ValueError("Cannot create artifact: no artifacts_dir set up")

        diff = ""
        base = self.artifacts_dir + os.path.sep + prefix
        while os.path.exists(base + diff + suffix) or base + diff + suffix in self.__artifacts:
            if diff:
                diff = "-%s" % (int(diff[1:]) + 1)
            else:
                diff = "-1"

        filename = base + diff + suffix
        self.log.debug("New artifact filename: %s", filename)
        self.__artifacts.append(filename)
        return filename

    def existing_artifact(self, filename, move=False):
        """
        Add existing artifact, it will be collected at the end of job. If
        move=True, the original file will be deleted

        :type filename: str
        :type move: bool
        """
        self.log.debug("Add existing artifact (move=%s): %s", move, filename)
        newname = self.artifacts_dir
        newname += os.path.sep + os.path.basename(filename)
        self.__artifacts.append(newname)

        if os.path.realpath(filename) == os.path.realpath(newname):
            self.log.debug("No need to copy %s", filename)
            return

        if not os.path.exists(filename):
            self.log.warning("Artifact file not exists: %s", filename)
            return

        if move:
            self.log.debug("Moving %s to %s", filename, newname)
            shutil.move(filename, newname)
        else:
            self.log.debug("Copying %s to %s", filename, newname)
            shutil.copy(filename, newname)

    def _create_artifacts_dir(self):
        if not self.artifacts_dir:
            date_str = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S.")
            if not os.path.isdir(self.artifacts_base_dir):
                os.makedirs(self.artifacts_base_dir)
            self.artifacts_dir = tempfile.mkdtemp(prefix=date_str,
                                                  dir=os.path.expanduser(
                                                      self.artifacts_base_dir))
        else:
            self.artifacts_dir = os.path.expanduser(self.artifacts_dir)
        self.log.info("Artifacts dir: %s", self.artifacts_dir)

        if not os.path.isdir(self.artifacts_dir):
            os.makedirs(self.artifacts_dir)

    def __load_module(self, alias):
        if alias in self.modules:
            return self.modules[alias]

        mod_conf = self.config.get('modules')
        if alias not in mod_conf:
            raise ValueError("Module alias '%s' not found in module settings" % alias)

        settings = ensure_is_dict(mod_conf, alias, "class")

        acopy = copy.deepcopy(settings)
        BetterDict.traverse(acopy, Configuration.masq_sensitive)
        self.log.debug("Module config: %s %s", alias, acopy)

        clsname = settings.get('class', None)
        try:
            self.modules[alias] = load_class(clsname)
            if not issubclass(self.modules[alias], EngineModule):
                raise TypeError("Module class does not inherit from EngineModule: %s" % clsname)
        except BaseException as exc:
            self.log.debug("Failed to load class %s: %s", clsname, traceback.format_exc())
            raise ValueError("Cannot load module '%s' with class %s" % (alias, clsname))

        return self.modules[alias]

    def instantiate_module(self, alias):
        """
        Create new instance for module using its alias from module settings
        section of config. Thus, to instantiate module it should be mentioned
        in settings.

        :type alias: str
        :rtype: EngineModule
        """
        classobj = self.__load_module(alias)
        instance = classobj()
        instance.log = self.log.getChild(classobj.__name__)
        instance.engine = self
        settings = self.config.get("modules")
        instance.settings = settings.get(alias)

        return instance

    def find_file(self, filename):
        """
        Try to find file in search_path if it was specified. Helps finding files
        in non-CLI environments

        :param filename:
        :return: :raise IOError:
        """
        if os.path.isfile(filename):
            return filename
        elif self.file_search_path:
            location = self.file_search_path + os.path.sep + os.path.basename(filename)
            self.log.warning("Guessed location for file %s: %s", filename, location)
            return location
        else:
            raise IOError("File not found: %s" % filename)

    def __load_configs(self, user_configs):
        for fname in user_configs:
            self.existing_artifact(fname)

        # prepare base configs
        base_configs = []
        machine_dir = os.getenv("VIRTUAL_ENV", "")  # respect virtualenv
        machine_dir += os.path.sep + "etc" + os.path.sep + "bzt.d"
        if os.path.isdir(machine_dir):
            self.log.debug("Reading machine configs from: %s", machine_dir)
            for cfile in os.listdir(machine_dir):
                fname = machine_dir + os.path.sep + cfile
                if os.path.isfile(fname):
                    base_configs.append(fname)
        else:
            self.log.info("No machine configs dir: %s", machine_dir)

        user_file = os.path.expanduser('~' + os.path.sep + ".bzt-rc")
        if os.path.isfile(user_file):
            self.log.debug("Adding personal config: %s", user_file)
            base_configs.append(user_file)
        else:
            self.log.info("No personal config: %s", user_file)

        # load user configs
        user_config = Configuration()
        user_config.load(user_configs)
        user_config.dump(self.create_artifact("merged", ".yml"), Configuration.YAML)
        user_config.dump(self.create_artifact("merged", ".json"), Configuration.JSON)

        # load base and merge user into it
        self.config.load(base_configs)
        self.config.merge(user_config)

    def __prepare_provisioning(self):
        cls = self.config.get(Provisioning.PROV, "")
        if not cls:
            raise ValueError("Please configure provisioning settings")
        self.provisioning = self.instantiate_module(cls)
        self.__prepare_aggregator()
        self.provisioning.prepare()

    def __prepare_reporters(self):
        # instantiate reporters
        reporting = self.config.get(Reporter.REP, [])
        for index, reporter in enumerate(reporting):
            reporter = ensure_is_dict(reporting, index, "module")
            cls = reporter.get('module', '')
            instance = self.instantiate_module(cls)
            instance.parameters = reporter
            if isinstance(instance, AggregatorListener):
                self.aggregator.add_listener(instance)  # NOTE: bad design, add_listener method is unknown
            self.reporters.append(instance)

        # then prepare them in case they would like to interact
        for module in self.reporters:
            module.prepare()

    def __prepare_aggregator(self):
        cls = self.config.get("settings").get("aggregator", "")
        if not cls:
            self.log.warning("Proceeding without aggregator, no results analysis")
            self.aggregator = EngineModule()
        else:
            self.aggregator = self.instantiate_module(cls)
        self.aggregator.prepare()

    def engine_resource_stats(self):
        """
        Get local resource stats

        :return: namedtuple
        """
        stats = namedtuple("ResourceStats", ('cpu', 'disk_usage', 'mem_usage',
                                             'rx', 'tx', 'dru', 'dwu'))
        rx, tx, dru, dwu = self.__get_resource_stats()
        # TODO: measure and report check loop utilization
        return stats(
            cpu=psutil.cpu_percent(interval=None),
            disk_usage=psutil.disk_usage(self.artifacts_dir).percent,
            mem_usage=psutil.virtual_memory().percent,
            rx=rx, tx=tx, dru=dru, dwu=dwu
        )

    def __get_resource_stats(self):
        if not self.__counters_ts:
            self.__disk_counters = psutil.disk_io_counters()
            self.__net_counters = psutil.net_io_counters()
            self.__counters_ts = datetime.datetime.now()
            time.sleep(0.2)  # small enough for human, big enough for machine

        now = datetime.datetime.now()
        interval = (now - self.__counters_ts).total_seconds()

        net = psutil.net_io_counters()
        tx = (net.bytes_sent - self.__net_counters.bytes_sent) / interval
        rx = (net.bytes_recv - self.__net_counters.bytes_recv) / interval
        self.__net_counters = net

        disk = psutil.disk_io_counters()
        dru = (disk.read_bytes - self.__disk_counters.read_bytes) / interval
        dwu = (disk.write_bytes - self.__disk_counters.write_bytes) / interval
        self.__disk_counters = disk

        self.__counters_ts = now
        return rx, tx, dru, dwu


class Configuration(BetterDict):
    """
    loading both JSONs and YAMLs and .properties-like override
    dump effective config into files
    first config should not contain action prefixes
    """
    JSON = "JSON"
    YAML = "YAML"
    INI = "INI"

    def __init__(self):
        super(Configuration, self).__init__()
        self.log = logging.getLogger('')
        self.dump_filename = None

    def load(self, configs):
        """
        Load and merge JSON/YAML files into current dict

        :type configs: list[str]
        """
        self.log.debug("Configs: %s", configs)
        for config_file in configs:
            config, ctype = self.__read_file(config_file)

            if isinstance(config, list):
                self.__apply_overrides(config)
            else:
                self.merge(config)

    def __read_file(self, filename):
        with open(filename) as fh:
            first_line = "#"
            while first_line.startswith("#"):
                first_line = fh.readline().strip()
            fh.seek(0)

            if first_line.startswith('---'):
                self.log.debug("Reading %s as YAML", filename)
                return yaml.load(fh), self.YAML
            elif first_line.startswith('{'):
                self.log.debug("Reading %s as JSON", filename)
                return json.loads(fh.read()), self.JSON
            elif first_line.startswith('['):
                self.log.debug("Reading %s as INI", filename)
                parser = ConfigParser.SafeConfigParser()
                parser.read(filename)
                res = []
                parser.add_section("BZT")
                for option in parser.options("BZT"):
                    res.append((option, parser.get("BZT", option)))
                return res, self.INI
            else:
                raise ValueError("Cannot detect file format for %s" % filename)

    def set_dump_file(self, filename):
        """
        Set default file and format to be used by `dump` method

        :type filename: str
        """
        self.dump_filename = filename

    def write(self, fds, fmt):
        """
        Write config into opened file

        :type fds: file
        :type fmt: str
        :raise ValueError:
        """
        if fmt == self.JSON:
            fds.write(to_json(self))
        elif fmt == self.YAML:
            yml = yaml.dump(self, default_flow_style=False,
                            explicit_start=True, canonical=False)
            fds.write(yml)
        elif fmt == self.INI:
            fds.write("[DEFAULT]\n")
            fds.write(self.__dict_to_overrides(self))
        else:
            raise ValueError("Unknown dump format: %s" % fmt)
        fds.write("\n")

    @classmethod
    def __dict_to_overrides(cls, obj, path=''):
        """
        Converts dict into OVERRIDES format, which is properties-like format

        :type path: str or unicode
        :return:
        """
        if isinstance(obj, dict):
            result = ''
            for key, val in six.iteritems(obj):
                result += cls.__dict_to_overrides(val, '%s.%s' % (path, key))
            return result
        elif isinstance(obj, list):
            result = ''
            for key, val in enumerate(obj):
                result += cls.__dict_to_overrides(val, '%s.%s' % (path, key))
            return result
        else:
            return "%s=%s\n" % (path[1:], obj)

    def dump(self, filename=None, fmt=None):
        """
        Dump current state of dict into file. If no filename or format
        specified, defaults are used

        :type filename: str or NoneType
        :type fmt: str or NoneType
        :raise ValueError:
        """
        if not filename:
            filename = self.dump_filename

        if filename:
            if not fmt:
                self.dump(filename + ".yml", self.YAML)
                self.dump(filename + ".json", self.JSON)
                return

            acopy = copy.deepcopy(self)
            BetterDict.traverse(acopy, self.masq_sensitive)
            with open(filename, "w") as fhd:
                self.log.debug("Dumping %s config into %s", fmt, filename)
                acopy.write(fhd, fmt)

    @staticmethod
    def masq_sensitive(config):
        for key in config.keys():
            if key in ('password', 'secret', 'token') and config[key]:
                config[key] = '*' * 8

    def __ensure_list_capacity(self, pointer, part, next_part=None):
        if isinstance(pointer, list) and isinstance(part, int):
            while len(pointer) <= part:
                self.log.debug("Len %s less than %s", len(pointer), part)
                if type(next_part) == int:
                    pointer.append([])
                else:
                    pointer.append(BetterDict())

    def __apply_overrides(self, opts):
        for name, value in opts:
            self.log.debug("Applying %s=%s", name, value)
            parts = [(int(x) if x.isdigit() else x) for x in name.split(".")]
            pointer = self
            for index, part in enumerate(parts[:-1]):
                self.__ensure_list_capacity(pointer, part, parts[index + 1])

                if type(part) == int:
                    pointer = pointer[part]
                elif type(parts[index + 1]) == int and isinstance(pointer, dict):
                    pointer = pointer.get(part, [])
                else:
                    pointer = pointer.get(part)

            self.__ensure_list_capacity(pointer, parts[-1])
            self.log.debug("Applying: %s[%s]=%s", pointer, parts[-1], value)
            if isinstance(parts[-1], six.string_types) and parts[-1][0] == '^':
                del pointer[parts[-1][1:]]
            else:
                if value.isdigit():
                    value = float(value)
                if isinstance(pointer, list) and parts[-1] == '-1':
                    pointer.append(value)
                else:
                    pointer[parts[-1]] = value

        self.dump()


yaml.add_representer(Configuration, SafeRepresenter.represent_dict)
yaml.add_representer(BetterDict, SafeRepresenter.represent_dict)
if six.PY2:
    yaml.add_representer(six.text_type, SafeRepresenter.represent_unicode)

# dirty hack from http://stackoverflow.com/questions/1447287/format-floats-with-standard-json-module
encoder.FLOAT_REPR = lambda o: format(o, '.3g')


class EngineModule(object):
    """
    Base class for any BZT engine module

    :type engine: engine.Engine
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
        default_executor = esettings.get("default-executor", None)

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


class FileLister(object):
    """
    A mixin to get required files info from executor
    """

    def resource_files(self):
        """
        Get list of resource files

        :rtype: list
        """
        raise NotImplementedError()


class ScenarioExecutor(EngineModule):
    """
    :type provisioning: engine.Provisioning
    :type execution: BetterDict
    """

    RAMP_UP = "ramp-up"
    HOLD_FOR = "hold-for"
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
            if isinstance(scenario, six.string_types):
                scenarios = self.engine.config.get("scenarios")
                if scenario not in scenarios:
                    raise ValueError("Scenario not found in scenarios: %s" % scenario)
                scenario = scenarios.get(scenario)
                self.__scenario = Scenario(scenario)
            elif isinstance(scenario, dict):
                self.__scenario = Scenario(scenario)
            else:
                raise ValueError("Scenario not configured properly: %s" % scenario)

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

        ramp_up = self.execution.get(ScenarioExecutor.RAMP_UP, None)
        hold = dehumanize_time(self.execution.get(ScenarioExecutor.HOLD_FOR, 0))
        if ramp_up is None:
            ramp_up = None
            duration = hold
        else:
            ramp_up = dehumanize_time(ramp_up)
            duration = hold + ramp_up

        if duration and not iterations:
            iterations = 0  # which means infinite

        res = namedtuple("LoadSpec",
                         ('concurrency', "throughput", 'ramp_up', 'hold', 'iterations', 'duration'))
        return res(concurrency=concurrency, ramp_up=ramp_up,
                   throughput=throughput, hold=hold, iterations=iterations,
                   duration=duration)

    def get_resource_files(self):
        files_list = self.execution.get("files", [])
        if isinstance(self, FileLister):
            files_list.extend(self.resource_files())
        return files_list


class Reporter(EngineModule):
    """
    This type of modules is responsible for
    in-test and post-test results analysis
    """

    REP = "reporting"

    def __init__(self):
        super(Reporter, self).__init__()
        self.parameters = BetterDict()


class Scenario(DictMixin, object):
    """
    Test scenario entity
    """

    SCRIPT = "script"

    def __init__(self, scenario=None):
        super(Scenario, self).__init__()
        self.data = scenario

    def get(self, key, default=defaultdict):
        """

        :param key:
        :type default: object
        :return:
        """
        return self.data.get(key, default)

    def __getitem__(self, item):
        return self.data[item]

    def __setitem__(self, key, value):
        self.data[key] = value

    def __iter__(self):
        for x in self.data:
            yield x

    def __len__(self):
        return len(self.data)

    def __delitem__(self, key):
        return self.data.pop(key)

    def get_headers(self):
        """
        Returns global headers

        :rtype: dict[str,str]
        """
        scenario = self
        headers = scenario.get("headers")
        return headers

    def get_requests(self):
        """
        Generator object to read requests
        """
        scenario = self
        requests = scenario.get("requests", [])
        for key, val in enumerate(requests):
            request = ensure_is_dict(requests, key, "url")
            res = namedtuple("HTTPReq",
                             ('url', 'label', 'method', 'headers', 'timeout', 'think_time', 'config', "body"))
            url = request["url"]
            label = request.get("label", url)
            method = request.get("method", "GET")
            headers = request.get("headers", {})
            timeout = request.get("timeout", None)
            think_time = request.get("think-time", None)

            body = None
            bodyfile = request.get("body-file", None)
            if bodyfile:
                with open(bodyfile) as fhd:
                    body = fhd.read()
            body = request.get("body", body)

            yield res(config=request, label=label,
                      url=url, method=method, headers=headers,
                      timeout=timeout, think_time=think_time, body=body)


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