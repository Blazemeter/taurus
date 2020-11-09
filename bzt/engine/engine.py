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
import json
import logging
import os
import pkgutil
import shutil
import sys
import threading
import time
import traceback
import uuid
from distutils.version import LooseVersion
from urllib import parse

from bzt import ManualShutdown, get_configs_dir, TaurusConfigError, TaurusInternalException
from bzt.utils import reraise, load_class, BetterDict, ensure_is_dict, dehumanize_time, is_windows, is_linux
from bzt.utils import shell_exec, get_full_path, ExceptionalDownloader, get_uniq_name, HTTPClient, Environment
from .dicts import Configuration
from .modules import Provisioning, Reporter, Service, Aggregator, EngineModule
from .names import EXEC, TAURUS_ARTIFACTS_DIR, SETTINGS
from .templates import Singletone
from ..environment_helpers import expand_variable_with_os, custom_expandvars, expand_envs_with_os

from bzt.resources.version import VERSION


class Engine(object):
    """
    Core entity of the technology, used to coordinate whole process

    :type reporters: list[Reporter]
    :type services: list[Service]EXEC
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
        self.aggregator.engine = self
        self.interrupted = False
        self.check_interval = 1
        self.stopping_reason = None
        self.engine_loop_utilization = 0
        self.prepared = []
        self.started = []

        self.default_cwd = None
        self.logging_level_down = lambda: None
        self.logging_level_up = lambda: None

        self.user_pythonpath = None
        self.temp_pythonpath = None

        self._http_client = None

    def set_pythonpath(self):
        version = sys.version.split(' ')[0]
        path_suffix = os.path.join('python-packages', version)
        self.user_pythonpath = get_full_path(os.path.join("~", ".bzt", path_suffix))
        self.temp_pythonpath = get_full_path(os.path.join(self.artifacts_dir, path_suffix))
        current_pythonpath = os.environ.get('PYTHONPATH', '')
        paths = self.user_pythonpath, self.temp_pythonpath, current_pythonpath

        self.log.debug("Set PYTHONPATH to :\n\tUSER: '{}' +\n\tTEMP: '{}' +\n\tCURRENT: '{}'".format(*paths))
        try:
            user_packages = os.listdir(self.user_pythonpath)
        except:
            user_packages = []

        self.log.debug("Content of user packages dir: {}".format(user_packages))

        os.environ['PYTHONPATH'] = os.pathsep.join(paths)

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

        self.config.merge({"version": VERSION})
        self.get_http_client()

        if self.config.get(SETTINGS).get("check-updates", True):
            install_id = self.config.get("install-id", self._generate_id())

            def wrapper():
                return self._check_updates(install_id)

            thread = threading.Thread(target=wrapper)  # intentionally non-daemon thread
            thread.start()

        return merged_config

    def unify_config(self):
        executions = self.config.get(EXEC, [])
        if isinstance(executions, dict):
            executions = [executions]
            self.config[EXEC] = executions

        settings = self.config.get(SETTINGS)
        default_executor = settings.get("default-executor", None)

        prov_type = self.config.get(Provisioning.PROV)

        for execution in executions:  # type: BetterDict
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

        self.artifacts_dir = self.__expand_artifacts_dir()

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

    def __expand_artifacts_dir(self):
        envs = self.__get_envs_from_config()
        artifacts_dir = custom_expandvars(self.artifacts_dir, envs)
        artifacts_dir = expand_variable_with_os(artifacts_dir)
        artifacts_dir = get_full_path(artifacts_dir)
        return artifacts_dir

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
        return False

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
            params = (VERSION, install_id)
            addr = "https://gettaurus.org/updates/?version=%s&installID=%s" % params
            self.log.debug("Requesting updates info: %s", addr)
            client = self.get_http_client()
            response = client.request('GET', addr, timeout=10)

            data = response.json()
            self.log.debug("Taurus updates info: %s", data)
            mine = LooseVersion(VERSION)
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
        envs = self.__get_envs_from_config()
        envs = expand_envs_with_os(envs)

        def apply_env(value, key, container):
            if isinstance(value, str):
                container[key] = custom_expandvars(value, envs)

        BetterDict.traverse(self.config, apply_env)

        self.__export_variables_to_os()

    def __export_variables_to_os(self):
        """
        Export all user-defined environment variables to the system.
        Example:

        settings:
          env:
            FOO: bbb/ccc
            BAR: aaa
        """
        envs = self.__get_envs_from_config()

        for var_name in envs:
            if envs[var_name] is None:
                if var_name in os.environ:
                    os.environ.pop(var_name)
            else:
                os.environ[var_name] = envs[var_name]
                self.log.debug("OS env: %s=%s", var_name, envs[var_name])

    def __get_envs_from_config(self):
        envs = self.config.get(SETTINGS, force_set=True).get("env", force_set=True)
        envs[TAURUS_ARTIFACTS_DIR] = self.artifacts_dir
        return envs
