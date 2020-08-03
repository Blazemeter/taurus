"""
BZT structures

Copyright 2019 BlazeMeter Inc.

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
import json
import logging
import re
from collections import defaultdict
from json import encoder

import math
import yaml
from yaml import SafeDumper
from yaml.representer import SafeRepresenter

from bzt import TaurusConfigError, TaurusInternalException, InvalidTaurusConfiguration
from bzt.requests_model import RequestParser
from collections import UserDict
from bzt.utils import str_representer, parse_think_time
from bzt.utils import to_json, ensure_is_dict, BetterDict


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

    def get_think_time(self, full=False):
        think_time = self.get('think-time')
        if think_time:
            return parse_think_time(think_time=think_time, full=full)

    def get_data_sources(self):
        sources = self.get(self.FIELD_DATA_SOURCES, [])
        if not isinstance(sources, list):
            raise TaurusConfigError("data-sources is not a list: '%s'" % sources)

        for idx, source in enumerate(sources):
            source = ensure_is_dict(sources, idx, "path")
            if not source:
                raise TaurusConfigError("Data source must have valid file path: '%s'" % source)
            source.get("loop", not self.engine.is_functional_mode(), force_set=True)
            delimiter = source.get("delimiter")
            if delimiter:
                source['delimiter'] = delimiter.replace('\\t', '\t')
                if delimiter.lower() == 'tab':
                    source['delimiter'] = '\t'

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
        if isinstance(key, str):
            for suffix in ('password', 'secret', 'token',):
                if key.lower().endswith(suffix):
                    if value and isinstance(value, str):
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
SafeDumper.add_representer(str, str_representer)

def replace_in_config(config, samples, substitutes, log=None):
    def file_replacer(value, key, container):
        if value in samples:
            container[key] = substitutes[samples.index(value)]
            if container[key] != value and log:
                log.debug("Replaced %s with %s", value, container[key])

    BetterDict.traverse(config, file_replacer)
