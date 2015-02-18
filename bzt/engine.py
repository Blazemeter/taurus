""" Main BZT classes """
import ConfigParser
from collections import namedtuple
import copy
import datetime
import json
import logging
import os
import psutil
import shutil
import tempfile
import time
import traceback
import yaml
from yaml.representer import SafeRepresenter
from json import encoder

from bzt import ManualShutdown, NormalShutdown
from bzt.modules import Provisioning, EngineModule, Reporter, AggregatorListener
from bzt.modules.aggregator import NoneAggregator
from bzt.utils import ensure_is_dict, load_class, BetterDict, to_json


class Engine(object):
    """
    Core entity of the technology, used to coordinate whole process

    :type reporters: list[Reporter]
    :type log: logging.Logger
    :type aggregator: bzt.modules.aggregator.ConsolidatingAggregator
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
        self.aggregator = NoneAggregator()
        self.interrupted = False
        self.check_interval = 1

        self.__counters_ts = None

    def prepare(self, user_configs):
        """
        Prepare engine for work, will call preparing of Provisioning and add
        downstream EngineModule instances

        :type user_configs: list
        """
        self.log.info("Preparing...")
        self._create_artifacts_dir()
        dump = self.create_artifact("effective_config", "")
        self.config.set_dump_file(dump, Configuration.YAML)

        self.check_interval = self.config.get("settings").get("check_interval",
                                                              1)
        self.__load_configs(user_configs)
        self.__load_modules()
        self.__prepare_provisioning()
        self.__prepare_reporters()

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
        except NormalShutdown, exc:
            self.log.debug("Normal shutdown called: %s", traceback.format_exc(exc))
        finally:
            self.__shutdown()

    def __wait(self):
        self.log.info("Waiting for finish...")
        prev = time.time()
        while not self.provisioning.check() and not self.aggregator.check() \
                and not EngineModule.check_modules_list(self.reporters):
            now = time.time()
            diff = now - prev
            delay = self.check_interval - diff
            msg = "Iteration took %.3f sec, sleeping for %.3f sec..."
            self.log.debug(msg, diff, delay)
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
            for module in self.reporters:
                module.shutdown()
        except BaseException, exc:
            self.log.error("Error while shutting down: %s", traceback.format_exc(exc))
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
                module.post_process()
        except KeyboardInterrupt, exc:
            self.log.error("Shutdown: %s", exc)
            exception = exc
        except BaseException, exc:
            self.log.error("Error while post-processing: %s", traceback.format_exc(exc))
            exception = exc
        finally:
            self.__finalize()
        self.config.dump()

        if exception:
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
            self.log.warn("Artifact file not exists: %s", filename)
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

    def __load_modules(self):
        mod_conf = self.config.get('modules')
        self.log.debug("Modules config: %s", mod_conf)
        for mod_key in mod_conf.keys():
            settings = ensure_is_dict(mod_conf, mod_key, "class")
            self.log.debug("Module config: %s %s", mod_key, settings)
            default = EngineModule.__module__
            default += "." + EngineModule.__name__
            self.log.debug("Default: %s", default)
            clsname = settings.get('class', default)
            try:
                self.modules[mod_key] = load_class(clsname)
                if not issubclass(self.modules[mod_key], EngineModule):
                    msg = "Module class does not inherit from EngineModule: %s"
                    raise TypeError(msg % clsname)
            except BaseException, exc:
                self.log.debug("Failed to load class %s: %s", clsname,
                               traceback.format_exc(exc))
                raise RuntimeError("Cannot load module: %s" % clsname)
        self.log.debug("Modules: %s", self.modules)

    def instantiate_module(self, cls):
        """
        Create new instance for module using its alias from module settings
        section of config. Thus, to instantiate module it should be mentioned
        in settings.

        :type cls: str
        :rtype: EngineModule
        """
        if cls not in self.modules:
            raise ValueError("Module '%s' not found in module settings" % cls)
        classobj = self.modules[cls]
        instance = classobj()
        instance.log = self.log.getChild(classobj.__name__)
        instance.engine = self
        settings = self.config.get("modules")
        instance.settings = settings.get(cls)

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
            location = self.file_search_path + os.path.sep + os.path.basename(
                filename)
            self.log.warn("Guessed location for file %s: %s", filename,
                          location)
            return location
        else:
            raise IOError("File not found: %s" % filename)

    def __load_configs(self, user_configs):
        for fname in user_configs:
            self.existing_artifact(fname)

        configs = []
        machine_dir = os.getenv("VIRTUAL_ENV", "")  # respect virtualenv
        machine_dir += os.path.sep + "etc" + os.path.sep + "bzt.d"
        if os.path.isdir(machine_dir):
            self.log.debug("Reading machine configs from: %s", machine_dir)
            for cfile in os.listdir(machine_dir):
                fname = machine_dir + os.path.sep + cfile
                if os.path.isfile(fname):
                    configs.append(fname)
        else:
            self.log.info("No machine configs dir: %s", machine_dir)
        configs.extend(user_configs)
        dump_type = self.config.load(configs)
        self.config.dump_format = dump_type

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
                self.aggregator.add_listener(instance)
            self.reporters.append(instance)

        # then prepare them in case they would like to interact
        for module in self.reporters:
            module.prepare()

    def __prepare_aggregator(self):
        cls = self.config.get("aggregator", "")
        if not cls:
            self.log.warn("Proceeding without aggregator, no results analysis")
            self.aggregator = NoneAggregator()
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
    dump effective config into file
    first config should not contain action prefixes
    """
    JSON = "JSON"
    YAML = "YAML"
    INI = "INI"

    def __init__(self):
        super(Configuration, self).__init__()
        self.log = logging.getLogger('')
        self.dump_filename = None
        self.dump_format = self.JSON

    def load(self, configs):
        """
        Load and merge JSON/YAML files into current dict

        :type configs: list[str]
        """
        self.log.debug("Configs: %s", configs)
        dump_type = self.YAML
        for config_file in configs:
            config, ctype = self.__read_file(config_file)

            if ctype != self.INI:
                dump_type = ctype

            if isinstance(config, list):
                self.__apply_overrides(config)
            else:
                self.merge(config)
        return dump_type

    def __read_file(self, filename):
        with open(filename) as fh:
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

    def set_dump_file(self, filename, fmt):
        """
        Set default file and format to be used by `dump` method

        :type filename: str
        :type fmt: str
        """
        self.dump_filename = filename
        self.dump_format = fmt

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
            for key, val in obj.iteritems():
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

        if not fmt:
            fmt = self.dump_format

        if filename:
            acopy = copy.deepcopy(self)
            BetterDict.traverse(acopy, self.__masq_sensitive)
            with open(filename, "w") as fhd:
                self.log.debug("Dumping %s config into %s", fmt, filename)
                acopy.write(fhd, fmt)

    def __masq_sensitive(self, obj):
        for key in obj.keys():
            if key in ('password', 'secret', 'token') and obj[key]:
                obj[key] = '*' * 8

    def __ensure_list_capacity(self, pointer, part, next_part=None):
        if isinstance(pointer, list):
            while len(pointer) <= part:
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
                elif type(parts[index + 1]) == int:
                    pointer = pointer.get(part, [])
                else:
                    pointer = pointer.get(part)

            self.__ensure_list_capacity(pointer, parts[-1])
            self.log.debug("Applying: %s[%s]=%s", pointer, parts[-1], value)
            if isinstance(parts[-1], basestring) and parts[-1][0] == '^':
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
yaml.add_representer(unicode, SafeRepresenter.represent_unicode)

# dirty hack from http://stackoverflow.com/questions/1447287/format-floats-with-standard-json-module
encoder.FLOAT_REPR = lambda o: format(o, '.3g')
