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
import os
from collections import OrderedDict, namedtuple

import logging
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
        self.log = parent_log.getChild(self.__class__.__name__)
        self.swagger = Swagger(self.log)

    def convert(self, swagger_path):
        if not os.path.exists(swagger_path):
            raise ValueError("Swagger file %s doesn't exist")

        self.swagger.parse(swagger_path)

        info = self.swagger.get_info()
        title = info.get("title", "Swagger")
        host = self.swagger.get_host()
        base_path = self.swagger.get_base_path()
        paths = self.swagger.get_paths()

        scenario_name = title.replace(' ', '-')
        requests = []

        for path, path_obj in iteritems(paths):
            for method in Swagger._methods:
                if getattr(path_obj, method) is not None:
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


class Swagger(object):
    _methods = ["get", "put", "post", "delete", "options", "head", "patch"]

    Definition = namedtuple("Definition", "name, schema")
    Parameter = namedtuple("Parameter", "name, location, description, required, schema, type, format")
    Response = namedtuple("Response", "name, description, schema, headers")
    Path = namedtuple("Path", "ref, get, put, post, delete, options, head, patch, parameters")
    Operation = namedtuple("Operation", "summary, description, operation_id, consumes, produces, parameters, responses")

    def __init__(self, parent_log=None):
        self.log = (parent_log or logging.getLogger('')).getChild(self.__class__.__name__)
        self.swagger = None
        self.info = None
        self.definitions = {}
        self.parameters = {}
        self.responses = {}
        self.paths = OrderedDict()

    def _load(self, swagger_spec_path):
        with open(swagger_spec_path) as fds:
            swagger_content = fds.read()
        try:
            self.swagger = yaml_ordered_load(swagger_content, yaml.SafeLoader)
            self.log.info("Loaded %s as YAML", swagger_spec_path)
        except IOError as exc:
            raise TaurusConfigError("Error when parsing Swagger file '%s': %s" % (swagger_spec_path, exc))

    def parse(self, swagger_spec_path):
        self._load(swagger_spec_path)

        self.info = self.swagger.get("info", {})

        for name, schema in iteritems(self.swagger.get("definitions", {})):
            self.definitions[name] = Swagger.Definition(name=name, schema=schema)

        for name, response in iteritems(self.swagger.get("responses", {})):
            self.responses[name] = Swagger.Response(name=name,
                                                    description=response["description"],
                                                    schema=response.get("schema"),
                                                    headers=response.get("headers"))

        for name, parameter in iteritems(self.swagger.get("parameters", {})):
            self.parameters[name] = Swagger.Parameter(name=name,
                                                      location=parameter.get("in"),
                                                      description=parameter.get("description"),
                                                      required=parameter.get("required"),
                                                      schema=parameter.get("schema"),
                                                      type=parameter.get("type"),
                                                      format=parameter.get("format"))

        for name, path_item in iteritems(self.swagger["paths"]):
            path = {"ref": None, "get": None, "put": None, "post": None, "delete": None, "options": None, "head": None,
                    "patch": None, "parameters": None}
            for method in Swagger._methods:
                if method in path_item:
                    op = path_item[method]
                    parameters = {param["name"]: Swagger.Parameter(name=param["name"],
                                                                   location=param.get("in"),
                                                                   description=param.get("description"),
                                                                   required=param.get("required"),
                                                                   schema=param.get("schema"),
                                                                   type=param.get("type"),
                                                                   format=param.get("format"))
                                  for param in op.get("parameters", [])}
                    # TODO: parse location-specific and schema stuff ('type', 'schema', etc)

                    responses = {name: Swagger.Response(name=name,
                                                        description=resp["description"],
                                                        schema=resp.get("schema"),
                                                        headers=resp.get("headers"))
                                 for name, resp in iteritems(op.get("responses", {}))}

                    operation = Swagger.Operation(summary=op.get("summary"),
                                                  description=op.get("description"),
                                                  operation_id=op.get("operationId"),
                                                  consumes=op.get("consumes"),
                                                  produces=op.get("produces"),
                                                  parameters=parameters,
                                                  responses=responses)

                    path[method] = operation
            path["parameters"] = {name: Swagger.Parameter(name=name,
                                                          location=param.get("in"),
                                                          description=param.get("description"),
                                                          required=param.get("required"),
                                                          schema=param.get("schema"),
                                                          type=param.get("type"),
                                                          format=param.get("format"))
                                  for name, param in iteritems(path_item.get("parameters", {}))}
            self.paths[name] = Swagger.Path(**path)

    def get_definitions(self):
        return self.definitions

    def get_responses(self):
        return self.responses

    def get_parameters(self):
        return self.parameters

    def get_paths(self):
        return self.paths

    def get_info(self):
        return self.info

    def get_host(self):
        return self.swagger.get("host")

    def get_base_path(self):
        return self.swagger.get("basePath")