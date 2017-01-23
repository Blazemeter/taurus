"""
Swagger module for Taurus

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
import json
import os
from collections import OrderedDict

import yaml

from bzt import TaurusConfigError
from bzt.six import iteritems


def yaml_ordered_load(stream, Loader=yaml.Loader, object_pairs_hook=OrderedDict):
    class OrderedLoader(Loader):
        pass
    def construct_mapping(loader, node):
        loader.flatten_mapping(node)
        return object_pairs_hook(loader.construct_pairs(node))
    OrderedLoader.add_constructor(
        yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
        construct_mapping)
    return yaml.load(stream, OrderedLoader)


class SwaggerConverter(object):
    def __init__(self, parent_log):
        self.swagger = None
        self.log = parent_log.getChild(self.__class__.__name__)

    def _load(self, path):
        with open(path) as fds:
            swagger_content = fds.read()
        try:
            self.swagger = json.loads(swagger_content, object_pairs_hook=OrderedDict)
            self.log.info("Loaded %s as JSON", path)
        except ValueError:
            try:
                self.swagger = yaml_ordered_load(swagger_content, yaml.SafeLoader)
                self.log.info("Loaded %s as YAML", path)
            except IOError as exc:
                raise TaurusConfigError("Error when parsing Swagger file '%s': %s" % (path, exc))

    def convert(self, swagger_path):
        if not os.path.exists(swagger_path):
            raise ValueError("Swagger file %s doesn't exist")

        self._load(swagger_path)

        info = self.swagger.get("info", {})
        title = info.get("title", "Swagger")
        host = self.swagger.get("host")
        base_path = self.swagger.get("basePath")
        schemes = self.swagger.get("schemes", ["http"])

        paths = self.swagger.get("paths")

        scenario_name = title.replace(' ', '-')
        requests = []

        for path, path_obj in iteritems(paths):
            for method, request_obj in iteritems(path_obj):
                requests.append({"url": base_path + path,
                                 "method": method.upper()})

        return {
            "scenarios": {
                scenario_name: {
                    "default-address": host,
                    "requests": requests
                }
            },
            "execution": [{
                "concurrency": 1,
                "scenario": scenario_name,
            }]
        }
