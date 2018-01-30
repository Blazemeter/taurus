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
import shutil
import signal
import sys
import tempfile
import traceback
from logging import Formatter
from optparse import OptionParser, Option
import optparse
import textwrap
from tempfile import NamedTemporaryFile

import yaml
from colorlog import ColoredFormatter

import bzt
from bzt import ManualShutdown, NormalShutdown, RCProvider, AutomatedShutdown
from bzt import TaurusException, ToolError
from bzt import TaurusInternalException, TaurusConfigError, TaurusNetworkError
from bzt.engine import Engine, Configuration, ScenarioExecutor
from bzt.engine import SETTINGS
from bzt.commands import Commands
from bzt.six import HTTPError, string_types, get_stacktrace
from bzt.utils import run_once, is_int, BetterDict, get_full_path, is_url


class CLI(object):
    """
    'cli' means 'tool' in hebrew, did you know that?

    :param options: OptionParser parsed parameters
    """
    console_handler = logging.StreamHandler(sys.stdout)

    def __init__(self, options, from_command=False):
        self.signal_count = 0
        self.options = options
        self.setup_logging(options, from_command=from_command)
        self.log = logging.getLogger('')
        if not from_command:
            self.log.info("Taurus CLI Tool v%s", bzt.VERSION)
            self.log.debug("Command-line options: %s", self.options)
            self.log.debug("Python: %s %s", platform.python_implementation(), platform.python_version())
            self.log.debug("OS: %s", platform.uname())
        self.engine = Engine(self.log)
        self.exit_code = 0

    @staticmethod
    @run_once
    def setup_logging(options, from_command=False):
        """
        Setting up console and file logging, colored if possible

        :param options: OptionParser parsed options
        :param from_command: When the invocation is from command
        """
        colors = {
            'WARNING': 'yellow',
            'ERROR': 'red',
            'CRITICAL': 'bold_red',
        }
        fmt_file = Formatter("[%(asctime)s %(levelname)s %(name)s] %(message)s")

        if from_command:
            fmt_verbose = Formatter("%(message)s")
            fmt_regular = Formatter("%(message)s")
        else:
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
        if options.log is None:
            tf = tempfile.NamedTemporaryFile(prefix="bzt_", suffix=".log", delete=False)
            tf.close()
            os.chmod(tf.name, 0o644)
            options.log = tf.name

        if options.log:
            file_handler = logging.FileHandler(options.log)
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

    def __close_log(self):
        """
        Close log handlers
        :return:
        """
        if self.options.log:
            # need to finalize the logger before finishing
            for handler in self.log.handlers:
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
            for handler in self.log.handlers:
                if issubclass(handler.__class__, logging.FileHandler):
                    self.log.debug("Closing log handler: %s", handler.baseFilename)
                    handler.close()
                    self.log.handlers.remove(handler)

            if os.path.exists(self.options.log):
                self.engine.existing_artifact(self.options.log, move=True, target_filename="bzt.log")
            self.options.log = os.path.join(self.engine.artifacts_dir, "bzt.log")

            file_handler = logging.FileHandler(self.options.log)
            file_handler.setLevel(logging.DEBUG)
            file_handler.setFormatter(Formatter("[%(asctime)s %(levelname)s %(name)s] %(message)s"))

            self.log.addHandler(file_handler)
            self.log.debug("Switched writing logs to %s", self.options.log)

    def __configure(self, configs):
        self.log.info("Starting with configs: %s", configs)

        if self.options.no_system_configs is None:
            self.options.no_system_configs = False

        bzt_rc = os.path.expanduser(os.path.join('~', ".bzt-rc"))
        if os.path.exists(bzt_rc):
            self.log.debug("Using personal config: %s" % bzt_rc)
        else:
            self.log.debug("Adding personal config: %s", bzt_rc)
            self.log.info("No personal config found, creating one at %s", bzt_rc)
            shutil.copy(os.path.join(get_full_path(__file__, step_up=1), 'resources', 'base-bzt-rc.yml'), bzt_rc)

        merged_config = self.engine.configure([bzt_rc] + configs, not self.options.no_system_configs)

        # apply aliases
        for alias in self.options.aliases:
            cli_aliases = self.engine.config.get('cli-aliases')
            keys = sorted(cli_aliases.keys())
            err = TaurusConfigError("'%s' not found in aliases. Available aliases are: %s" % (alias, ", ".join(keys)))
            self.engine.config.merge(cli_aliases.get(alias, err))

        if self.options.option:
            overrider = ConfigOverrider(self.log)
            overrider.apply_overrides(self.options.option, self.engine.config)

        settings = self.engine.config.get(SETTINGS)
        settings.get('verbose', bool(self.options.verbose))  # respect value from config
        if self.options.verbose:  # force verbosity if cmdline asked for it
            settings['verbose'] = True

        if settings.get('verbose'):
            CLI.console_handler.setLevel(logging.DEBUG)
        self.engine.create_artifacts_dir(configs, merged_config)
        self.engine.default_cwd = os.getcwd()

    def _level_down_logging(self):
        self.log.debug("Leveling down log file verbosity, use -v option to have DEBUG messages enabled")
        for handler in self.log.handlers:
            if issubclass(handler.__class__, logging.FileHandler):
                handler.setLevel(logging.INFO)

    def _level_up_logging(self):
        for handler in self.log.handlers:
            if issubclass(handler.__class__, logging.FileHandler):
                handler.setLevel(logging.DEBUG)
        self.log.debug("Leveled up log file verbosity")

    def perform(self, configs, sub_args=None, extra_args=None):
        """
        Run the tool

        :type configs: list
        :return: integer exit code
        """
        if isinstance(configs, SubCmdOptionParser):
            self.evaluate_command(configs, sub_args, extra_args)
        else:
            url_shorthands = []
            jmx_shorthands = []
            try:
                url_shorthands = self.__get_url_shorthands(configs)
                configs.extend(url_shorthands)

                jmx_shorthands = self.__get_jmx_shorthands(configs)
                configs.extend(jmx_shorthands)

                if not self.engine.config.get(SETTINGS).get('verbose', False):
                    self.engine.logging_level_down = self._level_down_logging
                    self.engine.logging_level_up = self._level_up_logging

                self.__configure(configs)
                self.__move_log_to_artifacts()

                self.engine.prepare()
                self.engine.run()
            except BaseException as exc:
                self.handle_exception(exc)
            finally:
                try:
                    for fname in url_shorthands + jmx_shorthands:
                        os.remove(fname)
                    self.engine.post_process()
                except BaseException as exc:
                    self.handle_exception(exc)

            self.log.info("Artifacts dir: %s", self.engine.artifacts_dir)
            if self.engine.artifacts_dir is None:
                self.log.info("Log file: %s", self.options.log)

            if self.exit_code:
                self.log.warning("Done performing with code: %s", self.exit_code)
            else:
                self.log.info("Done performing with code: %s", self.exit_code)

        self.__close_log()

        return self.exit_code

    def evaluate_command(self, configs, sub_args, extra_args):

        commands = Commands(self.log)
        if configs.name == "remote":
            if isinstance(sub_args, SubCmdOptionParser):
                if sub_args.name == "on":
                    commands.remote_on()
                elif sub_args.name == "off":
                    commands.remote_off()
                elif sub_args.name == "catalog":
                    commands.remote_catalog()
                elif sub_args.name == "attach":
                    if len(extra_args) == 0:
                        self.log.error("Specify service_id argument, one or more separated by space")
                        self.exit_code = 1
                    else:
                        service_ids = extra_args
                        commands.remote_attach(service_ids)
                elif sub_args.name == "detach":
                    if len(extra_args) == 0:
                        self.log.error("Specify service_id argument, one or more " +
                                       "separated by space or use the keyword '*all'")
                        self.exit_code = 1
                    else:
                        attach_ids = extra_args
                        commands.remote_detach(attach_ids)
                elif sub_args.name == "inspect":
                    if len(extra_args) == 0:
                        self.log.error("Specify service_id argument, one or more separated by space")
                        self.exit_code = 1
                    else:
                        attach_ids = extra_args
                        commands.remote_detach(attach_ids)
                elif sub_args.name == "list":
                    commands.remote_list()
                else:
                    self.log.info("Unparsed sub-command:%s" % sub_args.name)
            else:
                self.log.info("Unknown Sub Args type")
        else:
            self.log.info("Unparsed command:%s" % configs.name)

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
                config.get(ScenarioExecutor.EXEC, []).append({"executor": "jmeter", "scenario": {"script": jmx_file}})

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

            config = Configuration()
            config.merge({
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


class SubCmdOptionParser(object):
    """A subcommand of a root command-line application that may be
    invoked by a SubcommandOptionParser.
    """

    def __init__(self, name, parser=None, help='', aliases=()):
        """Creates a new subcommand. name is the primary way to invoke
        the subcommand; aliases are alternate names. parser is an
        OptionParser responsible for parsing the subcommand's options.
        help is a short description of the command. If no parser is
        given, it defaults to a new, empty OptionParser.
        """
        self.class_name = str(self.__class__)
        self.name = name
        self.parser = parser or OptionParserWithAliases()
        self.aliases = aliases
        self.help = help


class SubCmdsOptionParser(OptionParserWithAliases):
    """A variant of OptionParser that parses subcommands and their
    arguments.
    """

    # A singleton command used to give help on other subcommands.
    _HelpSubcommand = SubCmdOptionParser('help', optparse.OptionParser(),
                                         help='Give detailed help on a specific command',
                                         )

    def __init__(self, *args, **kwargs):
        """Create a new subcommand-aware option parser. All of the
        options to OptionParser.__init__ are supported in addition
        to subcommands, a sequence of Subcommand objects.
        """
        # The sub_command array, with the help command included.
        self.sub_commands = list(kwargs.pop('sub_commands', []))
        self.sub_commands.append(self._HelpSubcommand)

        # A more helpful default usage.
        if 'usage' not in kwargs:
            kwargs['usage'] = """
  %prog COMMAND [ARGS...]
  %prog help COMMAND"""

        # Super constructor.
        OptionParserWithAliases.__init__(self, *args, **kwargs)

        # Adjust the help-visible name of each subcommand.
        for sub_command in self.sub_commands:
            sub_command.parser.prog = '%s %s' % \
                                      (self.get_prog_name(), sub_command.name)

        # Our root parser needs to stop on the first unrecognized argument.
        self.disable_interspersed_args()

    def add_sub_command(self, cmd):
        """Adds a Subcommand object to the parser's list of commands.
        """
        self.sub_commands.append(cmd)

    # Add the list of subcommands to the help message.
    def format_help(self, formatter=None):
        # Get the original help message, to which we will append.
        out = optparse.OptionParser.format_help(self, formatter)
        if formatter is None:
            formatter = self.formatter

        # Subcommands header.
        result = []
        if len(self.sub_commands) > 1:
            result.append(formatter.format_heading('Commands'))
        formatter.indent()

        # Generate the display names (including aliases).
        # Also determine the help position.
        disp_names = []
        help_position = 0
        for sub_command in self.sub_commands:
            if sub_command.name == "help" and len(self.sub_commands) == 1:
                continue
            name = sub_command.name
            if sub_command.aliases:
                name += ' (%s)' % ', '.join(sub_command.aliases)
            disp_names.append(name)

            # Set the help position based on the max width.
            proposed_help_position = len(name) + formatter.current_indent + 2
            if proposed_help_position <= formatter.max_help_position:
                help_position = max(help_position, proposed_help_position)

                # Add each subcommand to the output.
        for sub_command, name in zip(self.sub_commands, disp_names):
            # Lifted directly from optparse.py.
            name_width = help_position - formatter.current_indent - 2
            if len(name) > name_width:
                name = "%*s%s\n" % (formatter.current_indent, "", name)
                indent_first = help_position
            else:
                name = "%*s%-*s  " % (formatter.current_indent, "",
                                      name_width, name)
                indent_first = 0
            result.append(name)
            help_width = formatter.width - help_position
            help_lines = textwrap.wrap(sub_command.help, help_width)
            result.append("%*s%s\n" % (indent_first, "", help_lines[0]))
            result.extend(["%*s%s\n" % (help_position, "", line)
                           for line in help_lines[1:]])
        formatter.dedent()

        # Concatenate the original help message with the subcommand
        # list.
        return out + "".join(result)

    def _sub_command_for_name(self, name):
        """Return the subcommand in self.subcommands matching the
        given name. The name may either be the name of a subcommand or
        an alias. If no subcommand matches, returns None.
        """
        for sub_command in self.sub_commands:
            if name == sub_command.name or \
                    name in sub_command.aliases:
                return sub_command
        return None

    def parse_args(self, args=None, values=None):
        """Like OptionParser.parse_args, but returns these four items:
        - options: the options passed to the root parser
        - subcommand: the Subcommand object that was invoked
        - suboptions: the options passed to the subcommand parser
        - subargs: the positional arguments passed to the subcommand
        """

        options, args = super(SubCmdsOptionParser, self).parse_args(args, values)

        sub_command = None
        sub_options = None
        sub_args = None
        sub_sub_args = None

        if not args:
            return options, args, None, None, None

        if args:
            cmd_name = args[0]
            sub_command = self._sub_command_for_name(cmd_name)

            if not sub_command:
                return options, args, None, None, None

            if isinstance(sub_command.parser, SubCmdsOptionParser):
                sub_options, sub_args, sub_sub_options, sub_sub_args, extra_sub_args = \
                    sub_command.parser.parse_args(args[1:])
            else:
                sub_options, sub_args = sub_command.parser.parse_args(args[1:])
                sub_sub_options = None
                sub_sub_args = None
                extra_sub_args = None

            if extra_sub_args:  # Remove the warnig from Codacy
                pass

            if sub_command is self._HelpSubcommand:
                if sub_args:
                    cmd_name = sub_args[0]
                    help_command = self._sub_command_for_name(cmd_name)
                    if help_command:
                        help_command.parser.print_help()
                        self.exit()
                    else:
                        self.error('Unknown command ' + cmd_name)
                else:
                    self.print_help()
                    self.exit()
            else:
                if len(sub_command.parser.sub_commands) > 1:
                    if not sub_sub_options:
                        sub_command.parser.print_help()
                        self.exit()

        return options, sub_command, sub_options, sub_args, sub_sub_args


def get_option_parser():
    usage = "Usage: bzt [options] [configs] [-aliases]"
    dsc = "BlazeMeter Taurus Tool v%s, the configuration-driven test running engine" % bzt.VERSION

    sub_commands = list()

    # sub_commands.append(SubCmdOptionParser('on',
    #                                       SubCmdsOptionParser(
    #                                           usage="bzt remote on",
    #                                           description="Turn on the remote provisioning mode",
    #                                           add_help_option=False
    #                                       ),
    #                                       help='Turn ON the remote provisioning mode',
    #                                       )
    #                    )

    # sub_commands.append(SubCmdOptionParser('off',
    #                                       SubCmdsOptionParser(
    #                                           usage="bzt remote off",
    #                                           description="Turn off provisioning mode and release reserved resources",
    #                                           add_help_option=False
    #                                       ),
    #                                       help='Turn OFF the remote provisioning mode',
    #                                       )
    #                    )

    sub_commands.append(SubCmdOptionParser('catalog',
                                           SubCmdsOptionParser(
                                               usage="bzt remote catalog",
                                               description="List the available services to be attached",
                                               add_help_option=False
                                           ),
                                           help='List the available services to be attached',
                                           )
                        )

    sub_commands.append(SubCmdOptionParser('attach',
                                           SubCmdsOptionParser(
                                               usage="bzt remote attach service_id | service_id1 service_id2 ...",
                                               description="Attach a service to Taurus",
                                               add_help_option=False
                                           ),
                                           help='Attach a service to Taurus',
                                           )
                        )

    sub_commands.append(SubCmdOptionParser('detach',
                                           SubCmdsOptionParser(
                                               usage="bzt remote detach attach_id | attach_id1 attach_id2 ... | *all",
                                               description="Detach an attached service",
                                               add_help_option=False
                                           ),
                                           help='Detach an attached service',
                                           )
                        )

    sub_commands.append(SubCmdOptionParser('list',
                                           SubCmdsOptionParser(
                                               usage="bzt remote list",
                                               description="List services attached to Taurus",
                                               add_help_option=False
                                           ),
                                           help='List services attached to Taurus',
                                           )
                        )

    sub_commands.append(SubCmdOptionParser('inspect',
                                           SubCmdsOptionParser(
                                               usage="bzt remote inspect attach_id",
                                               description="Inspect attached service, display detailed information",
                                               add_help_option=False
                                           ),
                                           help='Inspect attached service',
                                           )
                        )

    remote_opts = SubCmdsOptionParser(
        usage="bzt remote [command] [options]",
        description="Provisioning through Remote Services for Selenium and Appium",
        sub_commands=sub_commands,
        add_help_option=False
    )

    remote_cmd = SubCmdOptionParser('remote',
                                    remote_opts,
                                    help='Provisioning through Remote Services for Selenium and Appium',
                                    )

    parser = SubCmdsOptionParser(usage=usage, description=dsc, prog="bzt",
                                 sub_commands=(remote_cmd,))

    # parser = OptionParserWithAliases(usage=usage, description=dsc, prog="bzt")

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

    parsed_options, parsed_configs, parsed_suboptions, parsed_subargs, parsed_extra_args = parser.parse_args()

    if parsed_suboptions:  # Remove the warnig from Codacy
        pass

    from_command = False
    if isinstance(parsed_configs, SubCmdOptionParser):
        from_command = True

    executor = CLI(parsed_options, from_command=from_command)

    try:
        code = executor.perform(parsed_configs, parsed_subargs, parsed_extra_args)
    except BaseException as exc_top:
        logging.error("%s: %s", type(exc_top).__name__, exc_top)
        logging.debug("Exception: %s", traceback.format_exc())
        code = 1

    exit(code)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    main()
