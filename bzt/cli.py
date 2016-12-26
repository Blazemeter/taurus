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
import logging
import os
import platform
import signal
import sys
import traceback
from logging import Formatter
from optparse import OptionParser, Option
from tempfile import NamedTemporaryFile

import yaml
from colorlog import ColoredFormatter

import bzt
from bzt import TaurusException, ToolError
from bzt import TaurusInternalException, TaurusConfigError, TaurusNetworkError
from bzt import ManualShutdown, NormalShutdown, RCProvider, AutomatedShutdown
from bzt.engine import Engine, Configuration, ScenarioExecutor
from bzt.six import HTTPError, string_types, b, get_stacktrace
from bzt.utils import run_once, is_int, BetterDict, is_windows, is_piped


class CLI(object):
    """
    'cli' means 'tool' in hebrew, did you know that?

    :param options: OptionParser parsed parameters
    """

    def __init__(self, options):
        self.signal_count = 0
        self.options = options
        self.setup_logging(options)
        self.log = logging.getLogger('')
        self.log.info("Taurus CLI Tool v%s", bzt.VERSION)
        self.log.debug("Command-line options: %s", self.options)
        self.log.debug("Python: %s %s", platform.python_implementation(), platform.python_version())
        self.log.debug("OS: %s", platform.uname())
        self.engine = Engine(self.log)
        self.exit_code = 0

    @staticmethod
    @run_once
    def setup_logging(options):
        """
        Setting up console and file loggind, colored if possible

        :param options: OptionParser parsed options
        """
        colors = {
            'WARNING': 'yellow',
            'ERROR': 'red',
            'CRITICAL': 'bold_red',
        }
        fmt_file = Formatter("[%(asctime)s %(levelname)s %(name)s] %(message)s")
        if sys.stdout.isatty():
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
        if options.log:
            file_handler = logging.FileHandler(options.log)
            file_handler.setLevel(logging.DEBUG)
            file_handler.setFormatter(fmt_file)
            logger.addHandler(file_handler)

        # log something to console
        console_handler = logging.StreamHandler(sys.stdout)

        if options.verbose:
            console_handler.setLevel(logging.DEBUG)
            console_handler.setFormatter(fmt_verbose)
        elif options.quiet:
            console_handler.setLevel(logging.WARNING)
            console_handler.setFormatter(fmt_regular)
        else:
            console_handler.setLevel(logging.INFO)
            console_handler.setFormatter(fmt_regular)

        logger.addHandler(console_handler)

    def __close_log(self):
        """
        Close log handlers, move log to artifacts dir
        :return:
        """
        if self.options.log:
            if is_windows():
                # need to finalize the logger before moving file
                for handler in self.log.handlers:
                    if issubclass(handler.__class__, logging.FileHandler):
                        self.log.debug("Closing log handler: %s", handler.baseFilename)
                        handler.close()
                        self.log.handlers.remove(handler)
                if os.path.exists(self.options.log):
                    self.engine.existing_artifact(self.options.log)
                    os.remove(self.options.log)
            else:
                self.engine.existing_artifact(self.options.log, True)

    def __configure(self, configs):
        self.log.info("Starting with configs: %s", configs)

        if self.options.no_system_configs is None:
            self.options.no_system_configs = False

        merged_config = self.engine.configure(configs, not self.options.no_system_configs)

        # apply aliases
        for alias in self.options.aliases:
            cli_aliases = self.engine.config.get('cli-aliases')
            al_config = cli_aliases.get(alias, None)
            if al_config is None:
                raise TaurusConfigError("'%s' not found in aliases: %s" % (alias, cli_aliases.keys()))
            self.engine.config.merge(al_config)

        if self.options.option:
            overrider = ConfigOverrider(self.log)
            overrider.apply_overrides(self.options.option, self.engine.config)

        self.engine.create_artifacts_dir(configs, merged_config)
        self.engine.default_cwd = os.getcwd()

    def perform(self, configs):
        """
        Run the tool

        :type configs: list
        :return: integer exit code
        """
        jmx_shorthands = []
        try:
            jmx_shorthands = self.__get_jmx_shorthands(configs)
            configs.extend(jmx_shorthands)

            self.__configure(configs)
            self.engine.prepare()
            self.engine.run()
        except BaseException as exc:
            self.handle_exception(exc)
        finally:
            try:
                for fname in jmx_shorthands:
                    os.remove(fname)
                self.engine.post_process()
            except BaseException as exc:
                self.handle_exception(exc)

        self.log.info("Artifacts dir: %s", self.engine.artifacts_dir)

        if self.exit_code:
            self.log.warning("Done performing with code: %s", self.exit_code)
        else:
            self.log.info("Done performing with code: %s", self.exit_code)

        self.__close_log()

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
        elif isinstance(exc, TaurusNetworkError):
            self.log.log(log_level, "Network Error: %s", exc)
        else:
            msg = "Unknown Taurus exception %s: %s\n%s"
            raise ValueError(msg % (type(exc), exc, get_stacktrace(exc)))

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
                config.get(ScenarioExecutor.EXEC, []).append({"executor": "jmeter", "scenario": {"script": jmx_file}})

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
        :type dest: BetterDict
        """
        for option in options:
            name = option[:option.index('=')]
            value = option[option.index('=') + 1:]
            try:
                self.__apply_single_override(dest, name, value)
            except:
                self.log.debug("Failed override: %s", traceback.format_exc())
                self.log.error("Failed to apply override %s=%s", name, value)
                raise

        dest.dump()

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
                pointer = pointer.get(part, [])
            else:
                pointer = pointer.get(part)
        self.__ensure_list_capacity(pointer, parts[-1])
        self.log.debug("Applying: [%s]=%s", parts[-1], value)
        if isinstance(parts[-1], string_types) and parts[-1][0] == '^':
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
                dict_value = BetterDict()
                dict_value.merge(parsed_value)
                parsed_value = dict_value
            if isinstance(pointer, list) and parts[-1] < 0:
                pointer.append(parsed_value)
            else:
                pointer[parts[-1]] = parsed_value

    @staticmethod
    def __parse_override_value(override):
        try:
            return yaml.load(override)
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


def main():
    """
    This function is used as entrypoint by setuptools
    """
    usage = "Usage: bzt [options] [configs] [-aliases]"
    dsc = "BlazeMeter Taurus Tool v%s, the configuration-driven test running engine" % bzt.VERSION
    parser = OptionParserWithAliases(usage=usage, description=dsc, prog="bzt")
    parser.add_option('-l', '--log', action='store', default="bzt.log",
                      help="Log file location")
    parser.add_option('-o', '--option', action='append',
                      help="Override option in config")
    parser.add_option('-q', '--quiet', action='store_true',
                      help="Only errors and warnings printed to console")
    parser.add_option('-v', '--verbose', action='store_true',
                      help="Prints all logging messages to console")
    parser.add_option('-n', '--no-system-configs', action='store_true',
                      help="Skip system and user config files")

    parsed_options, parsed_configs = parser.parse_args()

    executor = CLI(parsed_options)

    if is_piped(sys.stdin):
        stdin = sys.stdin.read()
        if stdin:
            with NamedTemporaryFile(prefix="stdin_", suffix=".config", delete=False) as fhd:
                fhd.write(b(stdin))
                parsed_configs.append(fhd.name)

    try:
        code = executor.perform(parsed_configs)
    except BaseException as exc_top:
        logging.error("%s: %s", type(exc_top).__name__, exc_top)
        logging.debug("Exception: %s", traceback.format_exc())
        code = 1

    exit(code)


def signal_handler(sig, frame):
    """
    required for non-tty python runs to interrupt
    :param frame:
    :param sig:
    """
    del sig, frame
    raise ManualShutdown()


if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    main()
