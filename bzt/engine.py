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
import codecs
import copy
import datetime
import hashlib
import json
import logging
import math
import os
import pkgutil
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
from yaml import SafeDumper
from yaml.representer import SafeRepresenter

import bzt
from bzt import ManualShutdown, get_configs_dir, TaurusConfigError, TaurusInternalException, InvalidTaurusConfiguration
from bzt import ToolError
from bzt.requests_model import RequestParser
from bzt.six import numeric_types, string_types, text_type, PY2, UserDict, parse, reraise
from bzt.utils import PIPE, shell_exec, get_full_path, ExceptionalDownloader, get_uniq_name, HTTPClient
from bzt.utils import load_class, to_json, BetterDict, ensure_is_dict, dehumanize_time, is_windows, is_linux
from bzt.utils import str_representer, Environment, RequiredTool

TAURUS_ARTIFACTS_DIR = "TAURUS_ARTIFACTS_DIR"

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

        self.env = Environment(self.log)  # backward compatibility
        self.shared_env = Environment(self.log)  # backward compatibility

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

        self._http_client = None

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
        self.get_http_client()

        if self.config.get(SETTINGS).get("check-updates", True):
            install_id = self.config.get("install-id", self._generate_id())

            def wrapper():
                return self._check_updates(install_id)

            thread = threading.Thread(target=wrapper)  # intentionally non-daemon thread
            thread.start()

        return merged_config

    def unify_config(self):
        executions = self.config.get(ScenarioExecutor.EXEC, [])
        if isinstance(executions, dict):
            executions = [executions]
            self.config[ScenarioExecutor.EXEC] = executions

        settings = self.config.get(SETTINGS)
        default_executor = settings.get("default-executor", None)

        prov_type = self.config.get(Provisioning.PROV)

        for execution in executions:
            executor = execution.get("executor", default_executor, force_set=True)
            if not executor:
                msg = "Cannot determine executor type and no default executor in %s"
                raise TaurusConfigError(msg % execution)

        reporting = self.config.get(Reporter.REP, [])
        for index in range(len(reporting)):
            ensure_is_dict(reporting, index, "module")

        services = self.config.get(Service.SERV, [])
        for index in range(len(services)):
            ensure_is_dict(services, index, "module")

        modules = self.config.get("modules")
        for module in modules:
            ensure_is_dict(modules, module, "class")

    @staticmethod
    def _generate_id():
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
        self.unify_config()
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

    def start_subprocess(self, args, env, cwd=None, **kwargs):
        if cwd is None:
            cwd = self.default_cwd

        return shell_exec(args, cwd=cwd, env=env.get(), **kwargs)

    def run(self):
        """
        Run the job. Calls `startup`, does periodic `check`,
        calls `shutdown` in any case
        """
        self.log.info("Starting...")
        exc_info = exc_value = None
        try:
            self._startup()
            self.logging_level_down()
            self._wait()
        except BaseException as exc:
            self.log.debug("%s:\n%s", exc, traceback.format_exc())
            if not self.stopping_reason:
                self.stopping_reason = exc
            exc_value = exc
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
                if not exc_value:
                    exc_value = exc
                    exc_info = sys.exc_info()

        if exc_value:
            reraise(exc_info, exc_value)

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
        self.log.debug("Current stop reason: %s", self.stopping_reason)
        exc_info = exc_value = None
        modules = [self.provisioning, self.aggregator] + self.reporters + self.services  # order matters
        for module in modules:
            try:
                if module in self.started:
                    module.shutdown()
            except BaseException as exc:
                self.log.debug("%s:\n%s", exc, traceback.format_exc())
                if not self.stopping_reason:
                    self.stopping_reason = exc
                if not exc_value:
                    exc_value = exc
                    exc_info = sys.exc_info()

        self.config.dump()
        if exc_value:
            reraise(exc_info, exc_value)

    def post_process(self):
        """
        Do post-run analysis and processing for the results.
        """
        self.log.info("Post-processing...")
        # :type exception: BaseException
        exc_info = exc_value = None
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
                    if not exc_value:
                        exc_value = exc
                        exc_info = sys.exc_info()
        self.config.dump()

        if exc_info:
            reraise(exc_info, exc_value)

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
            artifacts_dir = self.config.get(SETTINGS, force_set=True).get("artifacts-dir", self.ARTIFACTS_DIR)
            self.artifacts_dir = datetime.datetime.now().strftime(artifacts_dir)

        self.artifacts_dir = get_full_path(self.artifacts_dir)

        self.log.info("Artifacts dir: %s", self.artifacts_dir)
        os.environ[TAURUS_ARTIFACTS_DIR] = self.artifacts_dir

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

        err = TaurusConfigError("Class name for alias '%s' is not found in module settings: %s" % (alias, settings))
        clsname = settings.get('class', err)

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
        Return path is full and mustn't treat with abspath/etc.
        :param filename: file basename to find
        :type filename: str
        """
        if not filename:
            return filename

        if filename.lower().startswith("http://") or filename.lower().startswith("https://"):
            parsed_url = parse.urlparse(filename)
            downloader = ExceptionalDownloader(self.get_http_client())
            self.log.info("Downloading %s", filename)
            tmp_f_name, headers = downloader.get(filename)
            cd_header = headers.get('Content-Disposition', '')
            dest = cd_header.split('filename=')[-1] if cd_header and 'filename=' in cd_header else ''
            if dest.startswith('"') and dest.endswith('"') or dest.startswith("'") and dest.endswith("'"):
                dest = dest[1:-1]
            elif not dest:
                dest = os.path.basename(parsed_url.path)
            fname, ext = os.path.splitext(dest) if dest else (parsed_url.hostname.replace(".", "_"), '.file')
            dest = self.create_artifact(fname, ext)
            self.log.debug("Moving %s to %s", tmp_f_name, dest)
            shutil.move(tmp_f_name, dest)
            return dest
        else:
            filename = os.path.expanduser(filename)  # expanding of '~' is required for check of existence

            # check filename 'as is' and all combinations of file_search_path/filename
            for dirname in [""] + self.file_search_paths:
                location = os.path.join(dirname, filename)
                if os.path.exists(location):
                    if dirname:
                        self.log.warning("Guessed location from search paths for %s: %s", filename, location)
                    return get_full_path(location)

        self.log.warning("Could not find location at path: %s", filename)
        return filename

    def _load_base_configs(self):
        configs = []
        try:
            sys.path.insert(0, os.path.curdir)  # necessary for development mode (running bzt from curdir)
            configs.extend(self._scan_system_configs())
            configs.extend(self._scan_package_configs())
        finally:
            sys.path.pop(0)
        configs.sort(key=os.path.basename)
        self.log.debug("Base configs list: %s", configs)
        if not configs:
            self.log.warning("No base configs were discovered")
        self.config.load(configs)

    def _scan_package_configs(self):
        configs = []
        for importer, modname, ispkg in pkgutil.iter_modules(path=None):
            try:
                if not ispkg:
                    continue

                package_path = getattr(importer, 'path', None)
                if package_path is None:
                    continue

                index_path = os.path.join(package_path, modname, 'bzt-configs.json')
                if not os.path.exists(index_path):
                    continue

                try:
                    with codecs.open(index_path, 'rb', encoding='utf-8') as fds:
                        index_configs = json.load(fds)
                except (OSError, IOError, ValueError) as exc:
                    self.log.debug("Can't load package-specific bzt config %s: %s", index_path, exc)
                    continue

                if not isinstance(index_configs, list):
                    self.log.debug("Error: value of bzt-configs.json should be a list (%s)" % index_path)
                    continue

                for config_name in index_configs:
                    configs.append(os.path.join(importer.path, modname, config_name))
            except BaseException as exc:
                self.log.warning("Can't look for package configs in package %r: %s", modname, str(exc))
                self.log.debug("Traceback: %s", traceback.format_exc())
        return configs

    def _scan_system_configs(self):
        configs = []
        machine_dir = get_configs_dir()  # can't refactor machine_dir out - see setup.py
        if os.path.isdir(machine_dir):
            self.log.debug("Reading system configs from: %s", machine_dir)
            for cfile in sorted(os.listdir(machine_dir)):
                fname = os.path.join(machine_dir, cfile)
                if os.path.isfile(fname):
                    configs.append(fname)
        return configs

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
        err = TaurusConfigError("Please check global config availability or configure provisioning settings")
        cls = self.config.get(Provisioning.PROV, err)
        self.provisioning = self.instantiate_module(cls)
        self.prepared.append(self.provisioning)
        self.provisioning.prepare()

    def __prepare_reporters(self):
        """
        Instantiate reporters, then prepare them in case they would like to interact
        """
        reporting = self.config.get(Reporter.REP, [])
        for index, reporter in enumerate(reporting):
            msg = "reporter 'module' field isn't recognized: %s"
            cls = reporter.get('module', TaurusConfigError(msg % reporter))
            instance = self.instantiate_module(cls)
            instance.parameters = reporter
            if self.__singletone_exists(instance, self.reporters):
                continue
            assert isinstance(instance, Reporter)
            self.reporters.append(instance)

        for reporter in self.reporters[:]:
            if not reporter.should_run():
                self.reporters.remove(reporter)

        # prepare reporters
        for module in self.reporters:
            self.prepared.append(module)
            module.prepare()

    def __prepare_services(self):
        """
        Instantiate service modules, then prepare them
        """
        srv_config = self.config.get(Service.SERV, [])
        services = []
        for index, config in enumerate(srv_config):
            cls = config.get('module', '')
            instance = self.instantiate_module(cls)
            instance.parameters = config
            if self.__singletone_exists(instance, services):
                continue
            assert isinstance(instance, Service)
            services.append(instance)

        for service in services[:]:
            if not service.should_run():
                services.remove(service)

        self.services.extend(services)

        for module in self.services:
            self.prepared.append(module)
            module.prepare()

    def __singletone_exists(self, instance, mods_list):
        """
        :type instance: EngineModule
        :type mods_list: list[EngineModule]
        :rtype: bool
        """
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

    def get_http_client(self):
        if self._http_client is None:
            self._http_client = HTTPClient()
            self._http_client.add_proxy_settings(self.config.get("settings").get("proxy"))
        return self._http_client

    def _check_updates(self, install_id):
        try:
            params = (bzt.VERSION, install_id)
            addr = "http://gettaurus.org/updates/?version=%s&installID=%s" % params
            self.log.debug("Requesting updates info: %s", addr)
            client = self.get_http_client()
            response = client.request('GET', addr, timeout=10)

            data = response.json()
            self.log.debug("Taurus updates info: %s", data)
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

    def eval_env(self):
        """
        Should be done after `configure`
        """
        envs = self.config.get(SETTINGS, force_set=True).get("env", force_set=True)
        envs[TAURUS_ARTIFACTS_DIR] = self.artifacts_dir

        for varname in envs:
            if envs[varname]:
                envs[varname] = str(envs[varname])
                envs[varname] = os.path.expandvars(envs[varname])

        for varname in envs:
            if envs[varname] is None:
                if varname in os.environ:
                    os.environ.pop(varname)
            else:
                os.environ[varname] = str(envs[varname])

        def custom_expandvars(value):
            parts = re.split(r'(\$\{.*?\})', value)
            value = ''
            for item in parts:
                if item and item.startswith("${") and item.endswith("}"):
                    key = item[2:-1]
                    if key in envs:
                        item = envs[key]
                if item is not None:
                    value += text_type(item)
            return value

        def apply_env(value, key, container):
            if isinstance(value, string_types):
                container[key] = custom_expandvars(value)

        BetterDict.traverse(self.config, apply_env)


class Configuration(BetterDict):
    """
    loading both JSONs and YAMLs and .properties-like override
    dump effective config into files
    first config should not contain action prefixes
    """
    JSON = "JSON"
    YAML = "YAML"

    def __init__(self, *args, **kwargs):
        super(Configuration, self).__init__(*args, **kwargs)
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
                with codecs.open(config_file, 'r', encoding='utf-8') as fds:
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
            yaml_documents = list(yaml.safe_load_all(contents))
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
            yml = yaml.safe_dump(self, default_flow_style=False, explicit_start=True, canonical=False,
                                 allow_unicode=True, encoding='utf-8', width=float("inf"))
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
        del value
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


SafeDumper.add_representer(Configuration, SafeRepresenter.represent_dict)
SafeDumper.add_representer(BetterDict, SafeRepresenter.represent_dict)
if PY2:
    SafeDumper.add_representer(text_type, SafeRepresenter.represent_unicode)
SafeDumper.add_representer(str, str_representer)

if PY2:
    # dirty hack from http://stackoverflow.com/questions/1447287/format-floats-with-standard-json-module
    encoder.FLOAT_REPR = lambda o: format(o, '.3g')
else:
    pass  # TODO: how to implement it?


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
        self.executors = []
        self.disallow_empty_execution = True

    def prepare(self):
        """
        Preparation in provisioning begins with reading executions list
        and instantiating ScenarioExecutor classes for them
        """
        super(Provisioning, self).prepare()

        exc = TaurusConfigError("No 'execution' is configured. Did you forget to pass config files?")
        executions = self.engine.config.get(ScenarioExecutor.EXEC, [])
        if not executions and self.disallow_empty_execution:
            raise exc

        for execution in executions:
            instance = self.engine.instantiate_module(execution.get("executor"))
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
        self.env = Environment(log=self.log)
        self.provisioning = None
        self.execution = BetterDict()  # FIXME: why have this field if we have `parameters` from base class?
        self.__scenario = None
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

    def get_scenario(self, name=None, cache_scenario=True):
        """
        Returns scenario dict, extract if scenario is inlined

        :return: DictOfDicts
        """
        if name is None and self.__scenario is not None:
            return self.__scenario

        scenarios = self.engine.config.get("scenarios", force_set=True)

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

        exc = TaurusConfigError("Scenario '%s' not found in scenarios: %s" % (label, scenarios.keys()))
        scenario = scenarios.get(label, exc)
        scenario_obj = Scenario(self.engine, scenario)

        if name is None and cache_scenario:
            self.__scenario = scenario_obj

        return scenario_obj

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

        if duration and not iterations:
            iterations = 0  # infinite

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

    def execute(self, args, **kwargs):
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

    def get_data_sources(self):
        sources = self.get(self.FIELD_DATA_SOURCES, [])
        if not isinstance(sources, list):
            raise TaurusConfigError("data-sources is not a list: '%s'" % sources)

        for idx, source in enumerate(sources):
            source = ensure_is_dict(sources, idx, "path")
            if not source:
                raise TaurusConfigError("Data source must have valid file path: '%s'" % source)

            yield source

    def get_requests(self, parser=RequestParser, require_url=True):
        """
        Generator object to read requests

        :type require_url: bool
        :type parser: class
        :rtype: list[bzt.requests_model.Request]
        """
        requests_parser = parser(self, self.engine)
        return requests_parser.extract_requests(require_url=require_url, )


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
