"""
Copyright 2017 BlazeMeter Inc.

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
import sys
import traceback
from optparse import OptionParser

from bzt import TaurusInternalException
from bzt.cli import CLI
from bzt.engine import Configuration
from bzt.utils import SoapUIScriptConverter, iteritems


class SoapUI2YAML(object):
    def __init__(self, options, file_name):
        self.log = logging.getLogger(self.__class__.__name__)
        self.options = options
        self.setup_logging()
        self.converter = None
        self.file_to_convert = file_name

    def setup_logging(self):
        CLI.setup_logging(self.options)
        if self.options.quiet:
            logging.disable(logging.WARNING)

    def process(self):
        output_format = Configuration.JSON if self.options.json else Configuration.YAML

        self.log.info('Loading SoapUI project %s', self.file_to_convert)
        self.file_to_convert = os.path.abspath(os.path.expanduser(self.file_to_convert))
        if not os.path.exists(self.file_to_convert):
            raise TaurusInternalException("File does not exist: %s" % self.file_to_convert)
        self.converter = SoapUIScriptConverter(self.log)
        try:
            converted_config = self.converter.convert_script(self.file_to_convert, self.options.test_case)
        except BaseException:
            self.log.error("Error while processing SoapUI project: %s", self.file_to_convert)
            raise

        self._cleanup_config(converted_config)

        exporter = Configuration.from_dict(converted_config)

        if self.options.file_name:
            file_name = self.options.file_name
        else:
            file_name = self.file_to_convert + "." + output_format.lower()

        exporter.dump(file_name, output_format)

        self.log.info("Done processing, result saved in %s", file_name)

    @staticmethod
    def _cleanup_config(config):
        for _, scenario in iteritems(config.get("scenarios")):
            if "test-case" in scenario:
                scenario.pop("test-case")
            if "test-suite" in scenario:
                scenario.pop("test-suite")


def process(parsed_options, args):
    tool = SoapUI2YAML(parsed_options, args[0])
    tool.process()


def main():
    usage = "Usage: soapui2yaml [input SoapUI file] [options]"
    parser = OptionParser(usage=usage, prog="soapui2yaml")
    parser.add_option('-t', '--test-case', action='store', default=None,
                      help="Extract only one test case given its name")
    parser.add_option('-v', '--verbose', action='store_true', default=False,
                      help="Prints all logging messages to console")
    parser.add_option('-o', '--out', dest="file_name",
                      help="Set output .yml file name, by default input file name + .yml is used")
    parser.add_option('-q', '--quiet', action='store_true', default=False, dest='quiet',
                      help="Do not display any log messages")
    parser.add_option('-j', '--json', action='store_true', default=False, dest='json',
                      help="Use JSON format")
    parser.add_option('-l', '--log', action='store', default=False, help="Log file location")
    parsed_options, args = parser.parse_args()
    if len(args) > 0:
        try:
            process(parsed_options, args)
        except BaseException as exc:
            logging.error("Exception: %s", exc)
            logging.debug("Exception: %s", traceback.format_exc())
            sys.exit(1)
        sys.exit(0)
    else:
        sys.stdout.write(usage + "\n")


if __name__ == "__main__":
    main()
