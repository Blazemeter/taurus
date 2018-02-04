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
import copy
import datetime
import hashlib
import json
import logging
import math
import os
import re
import shutil
import sys
import threading
import time
import traceback
import uuid
from abc import abstractmethod
from collections import namedtuple, defaultdict
from distutils.version import LooseVersion
from json import encoder

import yaml
from yaml.representer import SafeRepresenter

import bzt
from bzt import ManualShutdown, get_configs_dir, TaurusConfigError, TaurusInternalException, InvalidTaurusConfiguration
from bzt.requests_model import RequestsParser
from bzt.six import build_opener, install_opener, urlopen, numeric_types
from bzt.six import string_types, text_type, PY2, UserDict, parse, ProxyHandler, reraise
from bzt.utils import PIPE, shell_exec, get_full_path, ExceptionalDownloader, get_uniq_name
from bzt.utils import load_class, to_json, BetterDict, ensure_is_dict, dehumanize_time, is_windows, is_linux
from bzt.utils import str_representer, Environment

SETTINGS = "settings"


class Engine(object):
    """
    Core entity of the technology, used to coordinate whole process

    :type reporters: list[Reporter]
    :type services: list[Service]
    :type log: logging.Logger
    :type aggregator: bzt.modules.aggregator.ConsolidatingAggregator
    :type stopping_reason: BaseException
    """
    ARTIFACTS_DIR = "%Y-%m-%d_%H-%M-%S.%f"

    def __init__(self, parent_logger):
        """

        :type parent_logger: logging.Logger
        """
        self.file_search_paths = []
        self.services = []
        self.__artifacts = []
        self.reporters = []
        self.artifacts_dir = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.env = Environment(self.log, dict(os.environ))
        self.shared_env = Environment(self.log)
        self.config = Configuration()
        self.config.log = self.log.getChild(Configuration.__name__)
        self.modules = {}  # available modules
        self.provisioning = Provisioning()
        self.aggregator = Aggregator(is_functional=False)
        self.interrupted = False
        self.check_interval = 1
        self.stopping_reason = None
        self.engine_loop_utilization = 0
        self.prepared = []
        self.started = []

        self.default_cwd = None
        self.logging_level_down = lambda: None
        self.logging_level_up = lambda: None

    def configure(self, user_configs, read_config_files=True):
        """
        Load configuration files
        :type user_configs: list[str]
        :type read_config_files: bool
        """
        self.log.info("Configuring...")

        if read_config_files:
            self._load_base_configs()

        merged_config = self._load_user_configs(user_configs)

        all_includes = []
        while "included-configs" in self.config:
            includes = self.config.pop("included-configs")
            included_configs = [self.find_file(conf) for conf in includes if conf not in all_includes + user_configs]
            all_includes += includes
            self.config.load(included_configs)
        self.config['included-configs'] = all_includes

        self.config.merge({"version": bzt.VERSION})
        self._set_up_proxy()

        if self.config.get(SETTINGS).get("check-updates", True):
            install_id = self.config.get("install-id", self._generate_id())

            def wrapper():
                return self._check_updates(install_id)

            thread = threading.Thread(target=wrapper)  # intentionally non-daemon thread
            thread.start()

        return merged_config

    def _generate_id(self):
        if os.getenv("JENKINS_HOME"):
            prefix = "jenkins"
        elif os.getenv("TRAVIS"):
            prefix = "travis"
        elif any([key.startswith("bamboo") for key in os.environ.keys()]):
            prefix = "bamboo"
        elif os.getenv("TEAMCITY_VERSION"):
            prefix = "teamcity"
        elif os.getenv("DOCKER_HOST"):
            prefix = "docker"
        elif os.getenv("AWS_"):
            prefix = "amazon"
        elif os.getenv("GOOGLE_APPLICATION_CREDENTIALS") or os.getenv("CLOUDSDK_CONFIG"):
            prefix = "google_cloud"
        elif os.getenv("WEBJOBS_NAME"):
            prefix = "azure"
        elif is_linux():
            prefix = 'linux'
        elif is_windows():
            prefix = 'windows'
        else:
            prefix = 'macos'

        return "%s-%x" % (prefix, uuid.getnode())

    def prepare(self):
        """
        Prepare engine for work, will call preparing of Provisioning and add
        downstream EngineModule instances
        """
        self.log.info("Preparing...")
        interval = self.config.get(SETTINGS).get("check-interval", self.check_interval)
        self.check_interval = dehumanize_time(interval)

        try:
            self.__prepare_aggregator()
            self.__prepare_services()
            self.__prepare_provisioning()
            self.__prepare_reporters()
            self.config.dump()

        except BaseException as exc:
            self.stopping_reason = exc
            raise

    def _startup(self):
        modules = self.services + [self.aggregator] + self.reporters + [self.provisioning]  # order matters
        for module in modules:
            self.log.debug("Startup %s", module)
            self.started.append(module)
            module.startup()
        self.config.dump()

    def start_subprocess(self, args, cwd, stdout, stderr, stdin, shell, env):
        if cwd is None:
            cwd = self.default_cwd

        env = Environment(self.log, env.get())
        env.set(self.shared_env.get())

        return shell_exec(args, cwd=cwd, stdout=stdout, stderr=stderr, stdin=stdin, shell=shell, env=env.get())

    def run(self):
        """
        Run the job. Calls `startup`, does periodic `check`,
        calls `shutdown` in any case
        """
        self.log.info("Starting...")
        exc_info = None
        try:
            self._startup()
            self.logging_level_down()
            self._wait()
        except BaseException as exc:
            self.log.debug("%s:\n%s", exc, traceback.format_exc())
            self.stopping_reason = exc
            exc_info = sys.exc_info()
        finally:
            self.log.warning("Please wait for graceful shutdown...")
            try:
                self.logging_level_up()
                self._shutdown()
            except BaseException as exc:
                self.log.debug("%s:\n%s", exc, traceback.format_exc())
                if not self.stopping_reason:
                    self.stopping_reason = exc
                if not exc_info:
                    exc_info = sys.exc_info()

        if exc_info:
            reraise(exc_info)

    def _check_modules_list(self):
        stop = False
        modules = [self.provisioning, self.aggregator] + self.services + self.reporters  # order matters
        for module in modules:
            if module in self.started:
                self.log.debug("Checking %s", module)
                finished = bool(module.check())
                if finished:
                    self.log.debug("%s finished", module)
                    stop = finished
        return stop

    def _wait(self):
        """
        Wait modules for finish
        :return:
        """
        prev = time.time()

        while not self._check_modules_list():
            now = time.time()
            diff = now - prev
            delay = self.check_interval - diff
            self.engine_loop_utilization = diff / self.check_interval
            self.log.debug("Iteration took %.3f sec, sleeping for %.3f sec...", diff, delay)
            if delay > 0:
                time.sleep(delay)
            prev = time.time()
            if self.interrupted:
                raise ManualShutdown()
        self.config.dump()

    def _shutdown(self):
        """
        Shutdown modules
        :return:
        """
        self.log.info("Shutting down...")
        exc_info = None
        modules = [self.provisioning, self.aggregator] + self.reporters + self.services  # order matters
        for module in modules:
            try:
                if module in self.started:
                    module.shutdown()
            except BaseException as exc:
                self.log.debug("%s:\n%s", exc, traceback.format_exc())
                if not exc_info:
                    exc_info = sys.exc_info()

        self.config.dump()
        if exc_info:
            reraise(exc_info)

    def post_process(self):
        """
        Do post-run analysis and processing for the results.
        """
        self.log.info("Post-processing...")
        # :type exception: BaseException
        exc_info = None
        modules = [self.provisioning, self.aggregator] + self.reporters + self.services  # order matters
        # services are last because of shellexec which is "final-final" action
        for module in modules:
            if module in self.prepared:
                try:
                    module.post_process()
                except BaseException as exc:
                    if isinstance(exc, KeyboardInterrupt):
                        self.log.debug("post_process: %s", exc)
                    else:
                        self.log.debug("post_process: %s\n%s", exc, traceback.format_exc())
                    if not self.stopping_reason:
                        self.stopping_reason = exc
                    if not exc_info:
                        exc_info = sys.exc_info()
        self.config.dump()

        if exc_info:
            reraise(exc_info)

    def create_artifact(self, prefix, suffix):
        """
        Create new artifact in artifacts dir with given prefix and suffix

        :type prefix: str
        :type suffix: str
        :return: Path to created file
        :rtype: str
        :raise TaurusInternalException: if no artifacts dir set
        """
        if not self.artifacts_dir:
            raise TaurusInternalException("Cannot create artifact: no artifacts_dir set up")

        filename = get_uniq_name(self.artifacts_dir, prefix, suffix, self.__artifacts)
        self.__artifacts.append(filename)
        self.log.debug("New artifact filename: %s", filename)
        return filename

    def existing_artifact(self, filename, move=False, target_filename=None):
        """
        Add existing artifact, it will be collected into artifact_dir. If
        move=True, the original file will be deleted

        :type filename: str
        :type move: bool
        :type target_filename: str
        """
        self.log.debug("Add existing artifact (move=%s): %s", move, filename)
        if self.artifacts_dir is None:
            self.log.warning("Artifacts dir has not been set, will not copy %s", filename)
            return

        new_filename = os.path.basename(filename) if target_filename is None else target_filename
        new_name = os.path.join(self.artifacts_dir, new_filename)
        self.__artifacts.append(new_name)

        if get_full_path(filename) == get_full_path(new_name):
            self.log.debug("No need to copy %s", filename)
            return

        if not os.path.exists(filename):
            self.log.warning("Artifact file not exists: %s", filename)
            return

        if move:
            self.log.debug("Moving %s to %s", filename, new_name)
            shutil.move(filename, new_name)
        else:
            self.log.debug("Copying %s to %s", filename, new_name)
            shutil.copy(filename, new_name)

    def create_artifacts_dir(self, existing_artifacts=(), merged_config=None):
        """
        Create directory for artifacts, directory name based on datetime.now()
        """
        if not self.artifacts_dir:
            artifacts_dir = self.config.get(SETTINGS).get("artifacts-dir", self.ARTIFACTS_DIR)
            self.artifacts_dir = datetime.datetime.now().strftime(artifacts_dir)

        self.artifacts_dir = get_full_path(self.artifacts_dir)

        self.log.info("Artifacts dir: %s", self.artifacts_dir)
        self.env.set({"TAURUS_ARTIFACTS_DIR": self.artifacts_dir})

        if not os.path.isdir(self.artifacts_dir):
            os.makedirs(self.artifacts_dir)

        # dump current effective configuration
        dump = self.create_artifact("effective", "")  # TODO: not good since this file not exists
        self.config.set_dump_file(dump)
        self.config.dump()

        # dump merged configuration
        if merged_config:
            merged_config.dump(self.create_artifact("merged", ".yml"), Configuration.YAML)
            merged_config.dump(self.create_artifact("merged", ".json"), Configuration.JSON)

        for artifact in existing_artifacts:
            self.existing_artifact(artifact)

    def is_functional_mode(self):
        return self.aggregator is not None and self.aggregator.is_functional

    def __load_module(self, alias):
        """
        Load module class by alias
        :param alias: str
        :return: class
        """
        if alias in self.modules:
            return self.modules[alias]

        mod_conf = self.config.get('modules')
        if alias not in mod_conf:
            msg = "Module '%s' not found in list of available aliases %s" % (alias, sorted(mod_conf.keys()))
            raise TaurusConfigError(msg)

        settings = ensure_is_dict(mod_conf, alias, "class")

        acopy = copy.deepcopy(settings)
        BetterDict.traverse(acopy, Configuration.masq_sensitive)
        self.log.debug("Module config: %s %s", alias, acopy)

        clsname = settings.get('class', None)
        if clsname is None:
            raise TaurusConfigError("Class name for alias '%s' is not found in module settings: %s" % (alias, settings))

        self.modules[alias] = load_class(clsname)
        if not issubclass(self.modules[alias], EngineModule):
            raise TaurusInternalException("Module class does not inherit from EngineModule: %s" % clsname)

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
        assert isinstance(instance, EngineModule)
        instance.log = self.log.getChild(alias)
        instance.engine = self
        settings = self.config.get("modules")
        instance.settings = settings.get(alias)
        return instance

    def find_file(self, filename):
        """
        Try to find file or dir in search_path if it was specified. Helps finding files
        in non-CLI environments or relative to config path
        :param filename: file basename to find
        :type filename: str
        """
        if not filename:
            return filename
        filename = os.path.expanduser(filename)
        if os.path.exists(filename):
            return filename
        elif filename.lower().startswith("http://") or filename.lower().startswith("https://"):
            parsed_url = parse.urlparse(filename)
            downloader = ExceptionalDownloader()
            self.log.info("Downloading %s", filename)
            tmp_f_name, http_msg = downloader.get(filename)
            cd_header = http_msg.get('Content-Disposition', '')
            dest = cd_header.split('filename=')[-1] if cd_header and 'filename=' in cd_header else ''
            if not dest:
                dest = os.path.basename(parsed_url.path)
            fname, ext = os.path.splitext(dest) if dest else (parsed_url.hostname.replace(".", "_"), '.file')
            dest = self.create_artifact(fname, ext)
            self.log.debug("Moving %s to %s", tmp_f_name, dest)
            shutil.move(tmp_f_name, dest)
            return dest
        elif self.file_search_paths:
            for dirname in self.file_search_paths:
                location = os.path.join(dirname, filename)
                if os.path.exists(location):
                    self.log.warning("Guessed location from search paths for %s: %s", filename, location)
                    return location

        self.log.warning("Could not find location at path: %s", filename)
        return filename

    def _load_base_configs(self):
        base_configs = [os.path.join(get_full_path(__file__, step_up=1), 'resources', 'base-config.yml')]
        machine_dir = get_configs_dir()  # can't refactor machine_dir out - see setup.py
        if os.path.isdir(machine_dir):
            self.log.debug("Reading extension configs from: %s", machine_dir)
            for cfile in sorted(os.listdir(machine_dir)):
                fname = os.path.join(machine_dir, cfile)
                if os.path.isfile(fname):
                    base_configs.append(fname)
        else:
            self.log.debug("No machine configs dir: %s", machine_dir)

        self.log.debug("Base configs list: %s", base_configs)
        self.config.load(base_configs)

    def _load_user_configs(self, user_configs):
        """
        :type user_configs: list[str]
        :rtype: Configuration
        """
        # "tab-replacement-spaces" is not documented 'cause it loads only from base configs
        # so it's sort of half-working last resort
        self.config.tab_replacement_spaces = self.config.get(SETTINGS).get("tab-replacement-spaces", 4)
        self.log.debug("User configs list: %s", user_configs)
        self.config.load(user_configs)
        user_config = Configuration()
        user_config.log = self.log.getChild(Configuration.__name__)
        user_config.tab_replacement_spaces = self.config.tab_replacement_spaces
        user_config.warn_on_tab_replacement = False
        user_config.load(user_configs, self.__config_loaded)
        return user_config

    def __config_loaded(self, config):
        self.file_search_paths.append(get_full_path(config, step_up=1))

    def __prepare_provisioning(self):
        """
        Instantiate provisioning class
        """
        cls = self.config.get(Provisioning.PROV, None)
        if not cls:
            msg = "Please check global config availability or configure provisioning settings"
            raise TaurusConfigError(msg)
        self.provisioning = self.instantiate_module(cls)
        self.prepared.append(self.provisioning)
        self.provisioning.prepare()

    def __prepare_reporters(self):
        """
        Instantiate reporters, then prepare them in case they would like to interact
        """
        reporting = self.config.get(Reporter.REP, [])
        for index, reporter in enumerate(reporting):
            reporter = ensure_is_dict(reporting, index, "module")
            msg = "reporter 'module' field isn't recognized: %s"
            cls = reporter.get('module', TaurusConfigError(msg % reporter))
            instance = self.instantiate_module(cls)
            instance.parameters = reporter
            if self.__singletone_exists(instance, self.reporters):
                continue
            assert isinstance(instance, Reporter)
            self.reporters.append(instance)

        # prepare reporters
        for module in self.reporters:
            self.prepared.append(module)
            module.prepare()

    def __prepare_services(self):
        """
        Instantiate service modules, then prepare them
        """
        services = self.config.get(Service.SERV, [])
        for index, config in enumerate(services):
            config = ensure_is_dict(services, index, "module")
            cls = config.get('module', '')
            instance = self.instantiate_module(cls)
            instance.parameters = config
            if self.__singletone_exists(instance, self.services):
                continue
            assert isinstance(instance, Service)
            if instance.should_run():
                self.services.append(instance)

        for module in self.services:
            self.prepared.append(module)
            module.prepare()

    def __singletone_exists(self, instance, mods_list):
        if not isinstance(instance, Singletone):
            return False

        for mod in mods_list:
            if mod.parameters.get("module") == instance.parameters.get("module"):
                msg = "Module '%s' can be only used once, will merge all new instances into single"
                self.log.warning(msg % mod.parameters.get("module"))
                mod.parameters.merge(instance.parameters)
                return True

    def __prepare_aggregator(self):
        """
        Instantiate aggregators
        :return:
        """
        cls = self.config.get(SETTINGS).get("aggregator", "")
        if not cls:
            self.log.warning("Proceeding without aggregator, no results analysis")
        else:
            self.aggregator = self.instantiate_module(cls)
        self.prepared.append(self.aggregator)
        self.aggregator.prepare()

    def _set_up_proxy(self):
        proxy_settings = self.config.get("settings").get("proxy")
        if proxy_settings and proxy_settings.get("address"):
            proxy_url = parse.urlsplit(proxy_settings.get("address"))
            self.log.debug("Using proxy settings: %s", proxy_url)
            username = proxy_settings.get("username")
            pwd = proxy_settings.get("password")
            scheme = proxy_url.scheme if proxy_url.scheme else 'http'
            if username and pwd:
                proxy_uri = "%s://%s:%s@%s" % (scheme, username, pwd, proxy_url.netloc)
            else:
                proxy_uri = "%s://%s" % (scheme, proxy_url.netloc)
            proxy_handler = ProxyHandler({"https": proxy_uri, "http": proxy_uri})
            opener = build_opener(proxy_handler)
            install_opener(opener)

    def _check_updates(self, installID):
        try:
            params = (bzt.VERSION, installID)
            req = "http://gettaurus.org/updates/?version=%s&installID=%s" % params
            self.log.debug("Requesting updates info: %s", req)
            response = urlopen(req, timeout=10)
            resp = response.read()

            if not isinstance(resp, str):
                resp = resp.decode()

            self.log.debug("Taurus updates info: %s", resp)

            data = json.loads(resp)
            mine = LooseVersion(bzt.VERSION)
            latest = LooseVersion(data['latest'])
            if mine < latest or data['needsUpgrade']:
                msg = "There is newer version of Taurus %s available, consider upgrading. " \
                      "What's new: http://gettaurus.org/docs/Changelog/"
                self.log.warning(msg, latest)
            else:
                self.log.debug("Installation is up-to-date")

        except BaseException:
            self.log.debug("Failed to check for updates: %s", traceback.format_exc())
            self.log.warning("Failed to check for updates")


class Configuration(BetterDict):
    """
    loading both JSONs and YAMLs and .properties-like override
    dump effective config into files
    first config should not contain action prefixes
    """
    JSON = "JSON"
    YAML = "YAML"

    def __init__(self):
        super(Configuration, self).__init__()
        self.log = logging.getLogger('')
        self.dump_filename = None
        self.tab_replacement_spaces = 0
        self.warn_on_tab_replacement = True

    def load(self, config_files, callback=None):
        """
        Load and merge JSON/YAML files into current dict

        :type callback: callable
        :type config_files: list[str]
        """
        self.log.debug("Configs: %s", config_files)
        for config_file in config_files:
            try:
                configs = []
                with open(config_file) as fds:
                    if self.tab_replacement_spaces:
                        contents = self._replace_tabs(fds.readlines(), config_file)
                    else:
                        contents = fds.read()

                    self._read_yaml_or_json(config_file, configs, contents)

                for config in configs:
                    self.merge(config)

            except KeyboardInterrupt:
                raise
            except InvalidTaurusConfiguration:
                raise
            except BaseException as exc:
                raise TaurusConfigError("Error when reading config file '%s': %s" % (config_file, exc))

            if callback is not None:
                callback(config_file)

    def _read_yaml_or_json(self, config_file, configs, contents):
        try:
            self.log.debug("Reading %s as YAML", config_file)
            yaml_documents = list(yaml.load_all(contents))
            for doc in yaml_documents:
                if doc is None:
                    continue
                if not isinstance(doc, dict):
                    raise InvalidTaurusConfiguration("Configuration %s is invalid" % config_file)
                configs.append(doc)
        except KeyboardInterrupt:
            raise
        except BaseException as yaml_load_exc:
            self.log.debug("Cannot read config file as YAML '%s': %s", config_file, yaml_load_exc)
            if contents.lstrip().startswith('{'):
                self.log.debug("Reading %s as JSON", config_file)
                config_value = json.loads(contents)
                if not isinstance(config_value, dict):
                    raise InvalidTaurusConfiguration("Configuration %s in invalid" % config_file)
                configs.append(config_value)
            else:
                raise

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
        :raise TaurusInternalException:
        """
        if fmt == self.JSON:
            json_s = to_json(self)
            fds.write(json_s.encode('utf-8'))
        elif fmt == self.YAML:
            yml = yaml.dump(self, default_flow_style=False, explicit_start=True, canonical=False, allow_unicode=True,
                            encoding='utf-8', width=float("inf"))
            fds.write(yml)
        else:
            raise TaurusInternalException("Unknown dump format: %s" % fmt)
        fds.write("\n".encode('utf-8'))

    def dump(self, filename=None, fmt=None):
        """
        Dump current state of dict into file. If no filename or format
        specified, defaults are used

        :type filename: str or NoneType
        :type fmt: str or NoneType
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
            BetterDict.traverse(acopy, self.replace_infinities)
            with open(filename, "wb") as fhd:
                self.log.debug("Dumping %s config into %s", fmt, filename)
                acopy.write(fhd, fmt)

    @staticmethod
    def masq_sensitive(value, key, container):
        """
        Remove sensitive data from config
        """
        if isinstance(key, string_types):
            for suffix in ('password', 'secret', 'token',):
                if key.lower().endswith(suffix):
                    if value and isinstance(value, (string_types, text_type)):
                        container[key] = '*' * 8

    @staticmethod
    def replace_infinities(value, key, container):
        """
        Remove non-string JSON values used by default JSON encoder (Infinity, -Infinity, NaN)
        """
        if isinstance(container[key], float):
            if math.isinf(container[key]) or math.isnan(container[key]):
                container[key] = str(container[key])

    def _replace_tabs(self, lines, fname):
        has_tab_indents = re.compile("^( *)(\t+)( *\S*)")
        res = ""
        for num, line in enumerate(lines):
            replaced = has_tab_indents.sub(r"\1" + (" " * self.tab_replacement_spaces) + r"\3", line)
            if replaced != line:
                line = replaced
                if self.warn_on_tab_replacement:
                    self.log.warning("Replaced leading tabs in file %s, line %s", fname, num)
                    self.log.warning("Line content is: %s", replaced.strip())
                    self.log.warning("Please remember that YAML spec does not allow using tabs for indentation")
            res += line
        return res


yaml.add_representer(Configuration, SafeRepresenter.represent_dict)
yaml.add_representer(BetterDict, SafeRepresenter.represent_dict)
if PY2:
    yaml.add_representer(text_type, SafeRepresenter.represent_unicode)
yaml.add_representer(str, str_representer)

# dirty hack from http://stackoverflow.com/questions/1447287/format-floats-with-standard-json-module
encoder.FLOAT_REPR = lambda o: format(o, '.3g')


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
        self.disallow_empty_execution = True

    def prepare(self):
        """
        Preparation in provisioning begins with reading executions list
        and instantiating ScenarioExecutor classes for them
        """
        super(Provisioning, self).prepare()
        esettings = self.engine.config.get(SETTINGS)
        default_executor = esettings.get("default-executor", None)

        exc = TaurusConfigError("No 'execution' is configured. Did you forget to pass config files?")

        if ScenarioExecutor.EXEC not in self.engine.config and self.disallow_empty_execution:
            raise exc

        executions = self.engine.config.get(ScenarioExecutor.EXEC, [])
        if not executions and self.disallow_empty_execution:
            raise exc

        if isinstance(executions, dict):
            executions = [executions]

        for execution in executions:
            executor = execution.get("executor", default_executor)
            if not executor:
                msg = "Cannot determine executor type and no default executor in %s"
                raise TaurusConfigError(msg % execution)
            instance = self.engine.instantiate_module(executor)
            instance.provisioning = self
            instance.execution = execution
            assert isinstance(instance, ScenarioExecutor)
            self.executors.append(instance)


class FileLister(object):
    """
    A mixin to get required files info from executor
    """

    @abstractmethod
    def resource_files(self):
        """
        Get list of resource files

        :rtype: list
        """
        pass


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
    STEPS = "steps"
    LOAD_FMT = namedtuple("LoadSpec", "concurrency throughput ramp_up hold iterations duration steps")

    def __init__(self):
        super(ScenarioExecutor, self).__init__()
        self.provisioning = None
        self.execution = BetterDict()
        self.__scenario = None
        self.label = None
        self.widget = None
        self.reader = None
        self.delay = None
        self.start_time = None
        self.env = None
        self.preprocess_args = lambda x: None

    def has_results(self):
        if self.reader and self.reader.buffer:
            return True
        else:
            return False

    def get_script_path(self, scenario=None):
        """
        :type scenario: Scenario
        """
        if scenario is None:
            scenario = self.get_scenario()
        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            return self.engine.find_file(scenario.get(Scenario.SCRIPT))
        else:
            return None

    def get_scenario(self, name=None, cache_scenario=True):
        """
        Returns scenario dict, extract if scenario is inlined

        :return: DictOfDicts
        """
        if name is None and self.__scenario is not None:
            return self.__scenario

        scenarios = self.engine.config.get("scenarios")

        if name is None:  # get current scenario
            exc = TaurusConfigError("Scenario is not found in execution: %s" % self.execution)
            label = self.execution.get('scenario', exc)

            is_script = isinstance(label, string_types) and label not in scenarios and \
                        os.path.exists(self.engine.find_file(label))
            if isinstance(label, list):
                msg = "Invalid content of scenario, list type instead of dict or string: %s"
                raise TaurusConfigError(msg % label)
            if isinstance(label, dict) or is_script:
                self.log.debug("Extract %s into scenarios" % label)
                if isinstance(label, string_types):
                    scenario = BetterDict()
                    scenario.merge({Scenario.SCRIPT: label})
                else:
                    scenario = label

                path = self.get_script_path(Scenario(self.engine, scenario))
                if path is not None:
                    label = os.path.basename(path)
                if path is None or label in scenarios:
                    hash_str = str(hashlib.md5(to_json(scenario).encode()).hexdigest())
                    label = 'autogenerated_' + hash_str[-10:]

                scenarios[label] = scenario
                self.execution['scenario'] = label

            self.label = label
        else:  # get scenario by name
            label = name

        exc = TaurusConfigError("Scenario '%s' not found in scenarios: %s" % (label, scenarios.keys()))
        scenario = scenarios.get(label, exc)
        scenario_obj = Scenario(self.engine, scenario)

        if name is None and cache_scenario:
            self.__scenario = scenario_obj

        return scenario_obj

    def get_load(self):
        """
        Helper method to read load specification
        """
        prov_type = self.engine.config.get(Provisioning.PROV)

        ensure_is_dict(self.execution, ScenarioExecutor.THRPT, prov_type)
        throughput = self.execution[ScenarioExecutor.THRPT].get(prov_type, 0)

        ensure_is_dict(self.execution, ScenarioExecutor.CONCURR, prov_type)
        concurrency = self.execution[ScenarioExecutor.CONCURR].get(prov_type, 0)

        iterations = self.execution.get("iterations", None)

        ramp_up = self.execution.get(ScenarioExecutor.RAMP_UP, None)
        steps = self.execution.get(ScenarioExecutor.STEPS, None)
        hold = dehumanize_time(self.execution.get(ScenarioExecutor.HOLD_FOR, 0))
        if ramp_up is None:
            duration = hold
        else:
            ramp_up = dehumanize_time(ramp_up)
            duration = hold + ramp_up

        if duration and not iterations:
            iterations = 0  # which means infinite

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

    def execute(self, args, cwd=None, stdout=PIPE, stderr=PIPE, stdin=PIPE, shell=False):
        self.preprocess_args(args)

        return self.engine.start_subprocess(args=args, cwd=cwd, stdout=stdout,
                                            stderr=stderr, stdin=stdin, shell=shell, env=self.env)


class Reporter(EngineModule):
    """
    This type of modules is responsible for
    in-test and post-test results analysis
    """

    REP = "reporting"


class Service(EngineModule):
    """
    This type of modules is responsible for
    in-test and post-test results analysis
    """

    SERV = "services"

    def should_run(self):
        prov = self.engine.config.get(Provisioning.PROV)
        runat = self.parameters.get("run-at", "local")  # weird to have "local" hardcoded here
        if prov != runat:
            self.log.debug("Should not run because of non-matching prov: %s != %s", prov, runat)
            return False
        return True


class Aggregator(EngineModule):
    def __init__(self, is_functional):
        super(Aggregator, self).__init__()
        self.is_functional = is_functional


class Scenario(UserDict, object):
    """
    Test scenario entity
    """

    SCRIPT = "script"
    COOKIES = "cookies"
    FIELD_RESP_CODE = "http-code"
    FIELD_HEADERS = "headers"
    FIELD_BODY = "body"
    FIELD_DATA_SOURCES = 'data-sources'

    def __init__(self, engine, scenario=None):
        super(Scenario, self).__init__()
        self.engine = engine
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
        for item in self.data:
            yield item

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
        headers = scenario.get("headers", {})
        if headers is None:
            headers = {}
        return headers

    def get_requests(self, require_url=True):
        """
        Generator object to read requests

        :type require_url: bool
        :rtype: list[bzt.requests_model.Request]
        """
        requests_parser = RequestsParser(self, self.engine)
        return requests_parser.extract_requests(require_url=require_url)

    def get_data_sources(self):
        data_sources = self.get(self.FIELD_DATA_SOURCES, [])
        if not isinstance(data_sources, list):
            raise TaurusConfigError("data-sources '%s' is not a list" % data_sources)
        for index, _ in enumerate(data_sources):
            ensure_is_dict(data_sources, index, "path")
        return self.get(self.FIELD_DATA_SOURCES, [])


class HavingInstallableTools(object):
    @abstractmethod
    def install_required_tools(self):
        pass


class Singletone(object):
    pass


class SelfDiagnosable(object):
    @abstractmethod
    def get_error_diagnostics(self):
        """

        :rtype: list[str]
        """
        pass
