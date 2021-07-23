#! /usr/bin/env python
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
import logging
import os
import platform
import shutil
import signal
import sys
import tempfile
import traceback
from logging import Formatter
from optparse import OptionParser, Option
from tempfile import NamedTemporaryFile
from urllib.error import HTTPError

import yaml
from colorlog import ColoredFormatter

from bzt import ManualShutdown, NormalShutdown, RCProvider, TaurusException, AutomatedShutdown
from bzt import TaurusInternalException, TaurusConfigError, TaurusNetworkError, ToolError

from bzt.engine import Engine, Configuration, SETTINGS, EXEC
from bzt.linter import ConfigurationLinter
from bzt.utils import get_stacktrace, is_int, BetterDict, is_url, RESOURCES_DIR
from bzt.resources.version import VERSION, GIT_INFO, BUILD


class CLI(object):
    """
    'cli' means 'tool' in hebrew, did you know that?

    :param options: OptionParser parsed parameters
    """
    console_handler = logging.StreamHandler(sys.stdout)

    CLI_SETTINGS = "cli"

    def __init__(self, options):
        self.signal_count = 0
        self.options = options
        self.setup_logging(options)
        self.log = logging.getLogger('')
        self.log.info("Taurus CLI Tool v%s", VERSION)
        self.log.debug("Build: %s", BUILD)
        self.log.debug("Extended git info: %s", GIT_INFO)
        self.log.debug("Command-line options: %s", self.options)
        self.log.debug("Python: %s %s", platform.python_implementation(), platform.python_version())
        self.log.debug("OS: %s", platform.uname())

        try:
            self.log.debug("Path to interpreter: {}".format(sys.executable))
            self.log.debug("Path to packages: {}".format(sys.path))
            self.log.debug("Default python: {}".format(shutil.which('python' or 'not found')))
            self.log.debug("Default python3: {}".format(shutil.which('python3' or 'not found')))

        except BaseException as exc:
            self.log.warning("Extended python info getting error: {}".format(exc))


        self.engine = Engine(self.log)
        self.exit_code = 0

    @staticmethod
    def setup_logging(options):
        """
        Setting up console and file logging, colored if possible

        :param options: OptionParser parsed options
        """
        colors = {
            'WARNING': 'yellow',
            'ERROR': 'red',
            'CRITICAL': 'bold_red',
        }
        fmt_file = Formatter("[%(asctime)s %(levelname)s %(name)s] %(message)s")
        if sys.stdout and sys.stdout.isatty():
            fmt_verbose = ColoredFormatter("%(log_color)s[%(asctime)s %(levelname)s %(name)s] %(message)s",
                                           log_colors=colors)
            fmt_regular = ColoredFormatter("%(log_color)s%(asctime)s %(levelname)s: %(message)s",
                                           "%H:%M:%S", log_colors=colors)
        else:
            fmt_verbose = Formatter("[%(asctime)s %(levelname)s %(name)s] %(message)s")
            fmt_regular = Formatter("%(asctime)s %(levelname)s: %(message)s", "%H:%M:%S")

        logger = logging.getLogger('')
        logger.setLevel(logging.DEBUG)

        # log everything to file
        if options.log is None:
            tf = tempfile.NamedTemporaryFile(prefix="bzt_", suffix=".log", delete=False)
            tf.close()
            os.chmod(tf.name, 0o644)
            options.log = tf.name

        if options.log:
            file_handler = logging.FileHandler(options.log, encoding="utf-8")
            file_handler.setLevel(logging.DEBUG)
            file_handler.setFormatter(fmt_file)
            logger.addHandler(file_handler)

        # log something to console
        if options.verbose:
            CLI.console_handler.setLevel(logging.DEBUG)
            CLI.console_handler.setFormatter(fmt_verbose)
        elif options.quiet:
            CLI.console_handler.setLevel(logging.WARNING)
            CLI.console_handler.setFormatter(fmt_regular)
        else:
            CLI.console_handler.setLevel(logging.INFO)
            CLI.console_handler.setFormatter(fmt_regular)

        logger.addHandler(CLI.console_handler)

        logging.getLogger("requests").setLevel(logging.WARNING)  # misplaced?

    def close_log(self):
        """
        Close log handlers
        :return:
        """
        if self.options.log:
            # need to finalize the logger before finishing
            for handler in self.log.handlers[:]:
                if issubclass(handler.__class__, logging.FileHandler):
                    self.log.debug("Closing log handler: %s", handler.baseFilename)
                    handler.close()
                    self.log.handlers.remove(handler)

    def __move_log_to_artifacts(self):
        """
        Close log handlers, copy log to artifacts dir, recreate file handlers
        :return:
        """
        if self.options.log:
            for handler in self.log.handlers[:]:
                if issubclass(handler.__class__, logging.FileHandler):
                    self.log.debug("Closing log handler: %s", handler.baseFilename)
                    handler.close()
                    self.log.handlers.remove(handler)

            if os.path.exists(self.options.log):
                self.engine.existing_artifact(self.options.log, move=True, target_filename="bzt.log")
            self.options.log = os.path.join(self.engine.artifacts_dir, "bzt.log")

            file_handler = logging.FileHandler(self.options.log, encoding="utf-8")
            file_handler.setLevel(logging.DEBUG)
            file_handler.setFormatter(Formatter("[%(asctime)s %(levelname)s %(name)s] %(message)s"))

            self.log.addHandler(file_handler)
            self.log.debug("Switched writing logs to %s", self.options.log)

    def __configure(self, configs):
        if self.options.no_system_configs is None:
            self.options.no_system_configs = False

        load_hidden_configs = not self.options.no_system_configs
        if load_hidden_configs:
            bzt_rc = os.path.expanduser(os.path.join('~', ".bzt-rc"))
            if os.path.exists(bzt_rc):
                self.log.debug("Using personal config: %s" % bzt_rc)
            else:
                self.log.debug("Adding personal config: %s", bzt_rc)
                self.log.info("No personal config found, creating one at %s", bzt_rc)
                shutil.copy(os.path.join(RESOURCES_DIR, 'base-bzt-rc.yml'), bzt_rc)

            configs.insert(0, bzt_rc)

        self.log.info("Starting with configs: %s", configs)
        merged_config = self.engine.configure(configs, not self.options.no_system_configs)

        # apply aliases
        for alias in self.options.aliases:
            cli_aliases = self.engine.config.get('cli-aliases')
            keys = sorted(cli_aliases.keys())
            err = TaurusConfigError("'%s' not found in aliases. Available aliases are: %s" % (alias, ", ".join(keys)))
            self.engine.config.merge(cli_aliases.get(alias, err))

        if self.options.option:
            overrider = ConfigOverrider(self.log)
            overrider.apply_overrides(self.options.option, self.engine.config)

        if self.__is_verbose():
            CLI.console_handler.setLevel(logging.DEBUG)
        self.engine.create_artifacts_dir(configs, merged_config)
        self.engine.default_cwd = os.getcwd()
        self.engine.set_pythonpath()
        self.engine.eval_env()  # yacky, I don't like having it here, but how to apply it after aliases and artif dir?

    def __is_verbose(self):
        settings = self.engine.config.get(SETTINGS, force_set=True)
        settings.get('verbose', bool(self.options.verbose))  # respect value from config
        if self.options.verbose:  # force verbosity if cmdline asked for it
            settings['verbose'] = True

        return settings.get('verbose', False)

    def __lint_config(self):
        settings = self.engine.config.get(CLI.CLI_SETTINGS).get("linter")
        self.log.debug("Linting config")
        self.warn_on_unfamiliar_fields = settings.get("warn-on-unfamiliar-fields", True)
        config_copy = copy.deepcopy(self.engine.config)
        ignored_warnings = settings.get("ignored-warnings", [])
        self.linter = ConfigurationLinter(config_copy, ignored_warnings, self.log)
        self.linter.register_checkers()
        self.linter.lint()
        warnings = self.linter.get_warnings()
        for warning in warnings:
            self.log.warning(str(warning))

        if settings.get("lint-and-exit", False):
            if warnings:
                raise TaurusConfigError("Errors were found in the configuration")
            else:
                raise NormalShutdown("Linting has finished, no errors were found")

    def _level_down_logging(self):
        target = logging.DEBUG if self.__is_verbose() else logging.INFO
        for handler in self.log.handlers:
            if issubclass(handler.__class__, logging.FileHandler):
                if handler.level != target:
                    msg = "Leveling down log file verbosity to %s, use -v option to have DEBUG messages enabled"
                    self.log.debug(msg, logging.getLevelName(target))
                    handler.setLevel(target)

    def _level_up_logging(self):
        for handler in self.log.handlers:
            if issubclass(handler.__class__, logging.FileHandler):
                if handler.level != logging.DEBUG:
                    handler.setLevel(logging.DEBUG)
                    self.log.debug("Leveled up log file verbosity")

    def perform(self, configs):
        """
        Run the tool

        :type configs: list
        :return: integer exit code
        """
        url_shorthands = []
        jmx_shorthands = []
        jtl_shorthands = []
        try:
            url_shorthands = self.__get_url_shorthands(configs)
            configs.extend(url_shorthands)

            jmx_shorthands = self.__get_jmx_shorthands(configs)
            configs.extend(jmx_shorthands)

            jtl_shorthands = self.__get_jtl_shorthands(configs)
            configs.extend(jtl_shorthands)

            if not self.engine.config.get(SETTINGS).get('verbose', False, force_set=True):
                self.engine.logging_level_down = self._level_down_logging
                self.engine.logging_level_up = self._level_up_logging

            self.__configure(configs)
            self.__move_log_to_artifacts()
            self.__lint_config()

            self.engine.prepare()
            self.engine.run()
        except BaseException as exc:
            self.handle_exception(exc)
        finally:
            try:
                for fname in url_shorthands + jmx_shorthands + jtl_shorthands:
                    os.remove(fname)
                self.engine.post_process()
            except BaseException as exc:
                self.handle_exception(exc)

        if self.options.verbose:
            for module_name in sys.modules:
                version = str(getattr(sys.modules[module_name], '__version__', ""))
                file = getattr(sys.modules[module_name], '__file__', "")
                if version:
                    module_name = "-".join((module_name, version))
                    self.log.debug("\t{}\t{}".format(module_name, file))

        self.log.info("Artifacts dir: %s", self.engine.artifacts_dir)
        if self.engine.artifacts_dir is None:
            self.log.info("Log file: %s", self.options.log)

        if self.exit_code:
            self.log.warning("Done performing with code: %s", self.exit_code)
        else:
            self.log.info("Done performing with code: %s", self.exit_code)

        self.close_log()

        return self.exit_code

    def handle_exception(self, exc):
        log_level = {'info': logging.DEBUG, 'http': logging.DEBUG, 'default': logging.DEBUG}
        if not self.exit_code:  # only fist exception goes to the screen
            log_level['info'] = logging.WARNING
            log_level['http'] = logging.ERROR
            log_level['default'] = logging.ERROR
            if isinstance(exc, RCProvider):
                self.exit_code = exc.get_rc()
            else:
                self.exit_code = 1

        if isinstance(exc, KeyboardInterrupt):
            self.__handle_keyboard_interrupt(exc, log_level)
            log_level['default'] = logging.DEBUG
        elif isinstance(exc, TaurusException):
            self.__handle_taurus_exception(exc, log_level['default'])
            log_level['default'] = logging.DEBUG
        elif isinstance(exc, HTTPError):
            msg = "Response from %s: [%s] %s %s" % (exc.geturl(), exc.code, exc.reason, exc.read())
            self.log.log(log_level['http'], msg)
            log_level['default'] = logging.DEBUG

        self.log.log(log_level['default'], "%s: %s\n%s", type(exc).__name__, exc, get_stacktrace(exc))

    def __handle_keyboard_interrupt(self, exc, log_level):
        if isinstance(exc, ManualShutdown):
            self.log.log(log_level['info'], "Interrupted by user")
        elif isinstance(exc, AutomatedShutdown):
            self.log.log(log_level['info'], "Automated shutdown")
        elif isinstance(exc, NormalShutdown):
            self.log.log(logging.DEBUG, "Shutting down by request from code")
        elif isinstance(exc, KeyboardInterrupt):
            self.log.log(log_level['info'], "Keyboard interrupt")
        else:
            msg = "Non-KeyboardInterrupt exception %s: %s\n%s"
            raise ValueError(msg % (type(exc), exc, get_stacktrace(exc)))

    def __handle_taurus_exception(self, exc, log_level):
        if isinstance(exc, TaurusConfigError):
            self.log.log(log_level, "Config Error: %s", exc)
        elif isinstance(exc, TaurusInternalException):
            self.log.log(log_level, "Internal Error: %s", exc)
        elif isinstance(exc, ToolError):
            self.log.log(log_level, "Child Process Error: %s", exc)
            if exc.diagnostics is not None:
                for line in exc.diagnostics:
                    self.log.log(log_level, line)
        elif isinstance(exc, TaurusNetworkError):
            self.log.log(log_level, "Network Error: %s", exc)
        else:
            self.log.log(log_level, "Generic Taurus Error: %s", exc)

    def __get_jmx_shorthands(self, configs):
        """
        Generate json file with execution, executor and scenario settings
        :type configs: list
        :return: list
        """
        jmxes = []
        for filename in configs[:]:
            if filename.lower().endswith(".jmx"):
                jmxes.append(filename)
                configs.remove(filename)

        if jmxes:
            self.log.debug("Adding JMX shorthand config for: %s", jmxes)
            fds = NamedTemporaryFile(prefix="jmx_", suffix=".json")
            fname = fds.name
            fds.close()

            config = Configuration()

            for jmx_file in jmxes:
                piece = BetterDict.from_dict({"executor": "jmeter", "scenario": {"script": jmx_file}})
                config.get(EXEC, [], force_set=True).append(piece)  # Does it brake single execution?

            config.dump(fname, Configuration.JSON)

            return [fname]
        else:
            return []

    def __get_jtl_shorthands(self, configs):
        """
        Generate json file with execution, executor and scenario settings
        :type configs: list
        :return: list
        """
        jtls = []
        for filename in configs[:]:
            if filename.lower().endswith(".jtl"):
                jtls.append(filename)
                configs.remove(filename)

        if jtls:
            self.log.debug("Adding JTL shorthand config for: %s", jtls)
            fds = NamedTemporaryFile(prefix="jtl_", suffix=".json")
            fname = fds.name
            fds.close()

            config = Configuration()

            for jtl in jtls:
                piece = BetterDict.from_dict({"executor": "external-results-loader", "data-file": jtl})
                config.get(EXEC, [], force_set=True).append(piece)

            config.dump(fname, Configuration.JSON)

            return [fname]
        else:
            return []

    def __get_url_shorthands(self, configs):
        """
        :type configs: list
        :return: list
        """
        urls = []
        for candidate in configs[:]:
            if is_url(candidate):
                urls.append(candidate)
                configs.remove(candidate)

        if urls:
            self.log.debug("Adding HTTP shorthand config for: %s", urls)
            config_fds = NamedTemporaryFile(prefix="http_", suffix=".yml")
            fname = config_fds.name
            config_fds.close()

            config = Configuration.from_dict({
                "execution": [{
                    "concurrency": "${__tstFeedback(Throughput_Limiter,1,${__P(concurrencyCap,1)},2)}",
                    "hold-for": "2m",
                    "throughput": "${__P(throughput,600)}",
                    "scenario": "linear-growth",
                }],
                "scenarios": {
                    "linear-growth": {
                        "retrieve-resources": False,
                        "timeout": "5s",
                        "keepalive": False,
                        "requests": [{
                            "action": "pause",
                            "pause-duration": 0,
                            "jsr223": [{
                                "language": "javascript",
                                "execute": "before",
                                "script-text": """
var startTime = parseInt(props.get("startTime"));
if (!startTime) {
    startTime = Math.floor((new Date()).getTime() / 1000);
    props.put("startTime", startTime);
} else {
    var now = Math.floor((new Date()).getTime() / 1000);
    var offset = now - startTime;
    if (offset < 60) {
        var targetOffset = Math.max(offset * 10, 10);
        props.put("throughput", targetOffset.toString());
    }
}"""
                            }]
                        }] + urls,
                    }
                },
                "modules": {
                    "jmeter": {
                        "properties": {
                            "throughput": 1,
                            "concurrencyCap": 500,
                        },
                    }
                }
            })
            config.dump(fname, Configuration.JSON)
            return [fname]
        else:
            return []


class ConfigOverrider(object):
    def __init__(self, logger):
        """
        :type logger: logging.Logger
        """
        super(ConfigOverrider, self).__init__()
        self.log = logger.getChild(self.__class__.__name__)

    def apply_overrides(self, options, dest):
        """
        Apply overrides
        :type options: list[str]
        :type dest: Configuration
        """
        for option in options:
            name = option[:option.index('=')]
            value = option[option.index('=') + 1:]
            try:
                self.__apply_single_override(dest, name, value)
            except BaseException:
                self.log.debug("Failed override: %s", traceback.format_exc())
                self.log.error("Failed to apply override %s=%s", name, value)
                raise

        dest.dump()

    def __apply_mult_override(self, obj, key, replace_value):
        if isinstance(obj, list):
            for i in obj:
                if isinstance(i, dict):
                    i = self.__apply_mult_override(i, key, replace_value)
        if isinstance(obj, dict):            
            for k, v in obj.items():
                obj[k] = self.__apply_mult_override(v, key, replace_value)
        if isinstance(obj, dict) and key in obj:
            obj[key] = replace_value
        if isinstance(obj, list) and key in obj:
            obj[obj.index(key)] = replace_value
        return obj

    def __apply_single_override(self, dest, name, value):
        """
        Apply single override
        :type name: str
        :type value: str
        """
        self.log.debug("Applying %s=%s", name, value)
        parts = [(int(x) if is_int(x) else x) for x in name.split(".")]
        pointer = dest
        for index, part in enumerate(parts[:-1]):
            self.__ensure_list_capacity(pointer, part, parts[index + 1])

            if isinstance(part, int):
                if part < 0:
                    if isinstance(parts[index + 1], int):
                        pointer.append([])
                    else:
                        pointer.append(BetterDict())
                    pointer = pointer[-1]
                else:
                    pointer = pointer[part]
            elif isinstance(parts[index + 1], int) and isinstance(pointer, dict):
                pointer = pointer.get(part, [], force_set=True)
            else:
                pointer = pointer.get(part, force_set=True)
        self.__ensure_list_capacity(pointer, parts[-1])
        self.log.debug("Applying: [%s]=%s", parts[-1], value)
        if isinstance(parts[-1], str) and parts[-1][0] == '*':
            return self.__apply_mult_override(pointer, parts[-1][1:], value)

        if isinstance(parts[-1], str) and parts[-1][0] == '^':
            item = parts[-1][1:]

            if isinstance(pointer, list):
                item = int(item)
                if -len(pointer) <= item < len(pointer):
                    del pointer[item]
                else:
                    self.log.debug("No value to delete: %s", item)
            elif isinstance(pointer, dict):
                if item in pointer:
                    del pointer[item]
                else:
                    self.log.debug("No value to delete: %s", item)
            else:
                raise ValueError("Cannot handle override %s in non-iterable type %s" % (item, pointer))

        else:
            parsed_value = self.__parse_override_value(value)
            self.log.debug("Parsed override value: %r -> %r (%s)", value, parsed_value, type(parsed_value))
            if isinstance(parsed_value, dict):
                parsed_value = BetterDict.from_dict(parsed_value)
            if isinstance(pointer, list) and parts[-1] < 0:
                pointer.append(parsed_value)
            else:
                pointer[parts[-1]] = parsed_value

    @staticmethod
    def __parse_override_value(override):
        try:
            return yaml.safe_load(override)
        except BaseException:
            return override

    def __ensure_list_capacity(self, pointer, part, next_part=None):
        """
        Extend pointer list to hold additional item
        :type pointer: list
        :type part: int
        """
        if isinstance(pointer, list) and isinstance(part, int):
            while len(pointer) <= part:
                self.log.debug("Len %s less than %s", len(pointer), part)
                if isinstance(next_part, int):
                    pointer.append([])
                else:
                    pointer.append(BetterDict())


class OptionParserWithAliases(OptionParser, object):
    """
    Decorator that processes short opts as aliases
    """

    def __init__(self,
                 usage=None,
                 option_list=None,
                 option_class=Option,
                 version=None,
                 conflict_handler="error",
                 description=None,
                 formatter=None,
                 add_help_option=True,
                 prog=None,
                 epilog=None):
        super(OptionParserWithAliases, self).__init__(
            usage=usage, option_list=option_list,
            option_class=option_class, version=version,
            conflict_handler=conflict_handler, description=description, formatter=formatter,
            add_help_option=add_help_option, prog=prog, epilog=epilog)
        self.aliases = []

    def _process_short_opts(self, rargs, values):
        if rargs[0].startswith('-') and len(rargs[0]) > 2:
            self.aliases.append(rargs.pop(0)[1:])
        else:
            return OptionParser._process_short_opts(self, rargs, values)

    def parse_args(self, args=None, values=None):
        res = OptionParser.parse_args(self, args, values)
        res[0].aliases = self.aliases
        return res


def get_option_parser():
    usage = "Usage: bzt [options] [configs] [-aliases]"
    dsc = "BlazeMeter Taurus Tool v%s, the configuration-driven test running engine" % VERSION
    parser = OptionParserWithAliases(usage=usage, description=dsc, prog="bzt")
    parser.add_option('-l', '--log', action='store', default=None,
                      help="Log file location")
    parser.add_option('-o', '--option', action='append',
                      help="Override option in config")
    parser.add_option('-q', '--quiet', action='store_true',
                      help="Only errors and warnings printed to console")
    parser.add_option('-v', '--verbose', action='store_true',
                      help="Prints all logging messages to console")
    parser.add_option('-n', '--no-system-configs', action='store_true',
                      help="Skip system and user config files")
    return parser


def signal_handler(sig, frame):
    """
    required for non-tty python runs to interrupt
    :param frame:
    :param sig:
    """
    del sig, frame
    raise ManualShutdown()


def main():
    """
    This function is used as entrypoint by setuptools
    """
    parser = get_option_parser()

    parsed_options, parsed_configs = parser.parse_args()

    executor = CLI(parsed_options)

    try:
        code = executor.perform(parsed_configs)
    except BaseException as exc_top:
        logging.error("%s: %s", type(exc_top).__name__, exc_top)
        logging.debug("Exception: %s", traceback.format_exc())
        code = 1

    sys.exit(code)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    main()
