#! /usr/bin/env python
"""
CLI tool wrapper to run Engine using command-line interface
"""
from logging import Formatter
from optparse import OptionParser
import logging
import os
import sys
import tempfile
import traceback

from colorlog import ColoredFormatter

from bzt import Engine, ManualShutdown, NormalShutdown, version
from bzt.utils import run_once


class CLI(object):
    """
    'cli' means 'tool' in hebrew, did you know?

    :param options: OptionParser parsed parameters
    """

    def __init__(self, options):
        self.signal_count = 0
        self.options = options
        self.setup_logging(options)
        self.log = logging.getLogger('')
        self.log.info("Taurus CLI Tool v%s", version)
        logging.debug("Command-line options: %s", self.options)
        self.engine = Engine(self.log)
        self.engine.artifacts_base_dir = self.options.datadir
        self.engine.file_search_path = os.getcwd()

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
            fmt_verbose = ColoredFormatter(
                "%(log_color)s[%(asctime)s %(levelname)s %(name)s] %(message)s",
                log_colors=colors)
            fmt_regular = ColoredFormatter(
                "%(log_color)s%(asctime)s %(levelname)s: %(message)s",
                "%H:%M:%S", log_colors=colors)
        else:
            fmt_verbose = Formatter(
                "[%(asctime)s %(levelname)s %(name)s] %(message)s")
            fmt_regular = Formatter(
                "%(asctime)s %(levelname)s: %(message)s", "%H:%M:%S")

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

    def perform(self, configs):
        """
        Run the tool

        :type configs: list
        :return: integer exit code
        """
        user_file = os.path.expanduser('~') + os.path.sep + ".bzt-rc"
        if os.path.isfile(user_file):
            self.log.debug("Adding personal config: %s", user_file)
            configs.insert(0, user_file)

        overrides = []
        try:
            overrides = self.__get_config_overrides()
            configs.extend(overrides)
            logging.info("Starting with configs: %s", configs)
            self.engine.prepare(configs)
            self.engine.run()
            exit_code = 0
        except BaseException, exc:
            self.log.debug("Caught exception in try: %s", traceback.format_exc(exc))
            if isinstance(exc, ManualShutdown):
                self.log.info("Interrupted by user: %s", exc)
            elif isinstance(exc, NormalShutdown):
                self.log.info("Normal shutdown")
            else:
                self.log.error("Exception: %s", exc)
            self.log.warn("Please wait for graceful shutdown...")
            exit_code = 1
        finally:
            try:
                for fname in overrides:
                    os.remove(fname)
                self.engine.post_process()
            except BaseException, exc:
                self.log.debug("Caught exception in finally: %s", traceback.format_exc(exc))
                self.log.error("Exception: %s", exc)
                exit_code = 1

        self.log.info("Artifacts dir: %s", self.engine.artifacts_dir)
        self.log.info("Done performing with code: %s", exit_code)
        if self.options.log:
            self.engine.existing_artifact(self.options.log, True)
        return exit_code

    def __get_config_overrides(self):
        if self.options.option:
            self.log.debug("Adding overrides: %s", self.options.option)
            fds, fname = tempfile.mkstemp(".ini", "overrides_",
                                          dir=self.engine.artifacts_base_dir)
            os.close(fds)
            with open(fname, 'w') as fds:
                fds.write("[DEFAULT]\n")
                for option in self.options.option:
                    fds.write(option + "\n")
            return [fname]
        else:
            return []


def main():
    """
    This function is used as entrypoint by setuptools
    """
    parser = OptionParser()
    parser.add_option('-d', '--datadir', action='store', default=".",
                      help="Artifacts base dir")
    parser.add_option('-l', '--log', action='store', default="bzt.log",
                      help="Log file location")
    parser.add_option('-o', '--option', action='append',
                      help="Override option in config")
    parser.add_option('-q', '--quiet', action='store_true',
                      help="Only errors and warnings printed to console")
    parser.add_option('-v', '--verbose', action='store_true',
                      help="Prints all logging messages to console")

    parsed_options, parsed_configs = parser.parse_args()

    executor = CLI(parsed_options)

    try:
        code = executor.perform(parsed_configs)
    except BaseException, exc_top:
        logging.error("Exception: %s", exc_top)
        logging.debug("Exception: %s", traceback.format_exc(exc_top))
        code = 1

    exit(code)


if __name__ == "__main__":
    main()