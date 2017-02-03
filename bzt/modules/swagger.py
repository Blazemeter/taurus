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
import logging
import os
from collections import namedtuple, OrderedDict

import copy
import yaml

from bzt import TaurusConfigError
from bzt.six import iteritems, parse, urlencode


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
    def __init__(self, settings, parent_log):
        self.settings = settings
        self.log = parent_log.getChild(self.__class__.__name__)
        self.swagger = Swagger(self.log)

    def _extract_request(self, path, method, operation):
        request = {}

        if self.settings.get("get-only", True) and method != "get":
            return None

        if method != "get":
            request["method"] = method.upper()

        if operation.operation_id is not None:
            request["label"] = operation.operation_id

        query_params = {}
        form_data = {}
        headers = {}
        for _, param in iteritems(operation.parameters):
            if param.location == "header":
                name = param.name
                value = Swagger.get_data_for_type(param.type, param.format)
                headers[name] = value
            elif param.location == "query" and param.required:
                name = param.name
                value = Swagger.get_data_for_type(param.type, param.format)
                query_params[name] = value
            elif param.location == "formData" and param.required:
                name = param.name
                value = Swagger.get_data_for_type(param.type, param.format)
                form_data[name] = value
            elif param.location == "body":
                # TODO:
                pass

        if headers:
            request["headers"] = headers

        if form_data:
            request["body"] = form_data

        if query_params:
            parts = parse.urlparse(path)
            query = urlencode(query_params)
            replaced = parts._replace(query=query)
            url = parse.urlunparse(replaced)
        else:
            url = path

        request["url"] = url

        return request

    def _extract_requests_from_paths(self, paths):
        base_path = self.swagger.get_base_path()
        requests = []
        for path, path_obj in iteritems(paths):
            self.log.debug("Handling path %s", path)
            for method in Swagger.METHODS:
                operation = getattr(path_obj, method)
                if operation is not None:
                    self.log.debug("Handling method %s", method.upper())
                    request = self._extract_request(base_path + path, method, operation)
                    # TODO: Swagger responses -> JMeter assertions?

                    if request is not None:
                        requests.append(request)

        return requests

    def convert(self, swagger_path):
        if not os.path.exists(swagger_path):
            raise ValueError("Swagger file %s doesn't exist")

        self.swagger.parse(swagger_path)

        info = self.swagger.get_info()
        title = info.get("title", "Swagger")
        host = self.swagger.get_host()
        paths = self.swagger.get_interpolated_paths()
        schemes = self.swagger.swagger.get("schemes", ["http"])
        scheme = schemes[0]
        default_address = scheme + "://" + host
        scenario_name = title.replace(' ', '-')
        requests = self._extract_requests_from_paths(paths)

        return {
            "scenarios": {
                scenario_name: {
                    "default-address": default_address,
                    "requests": requests
                }
            },
            "execution": [{
                "concurrency": 1,
                "scenario": scenario_name,
            }]
        }


class Swagger(object):
    METHODS = ["get", "put", "post", "delete", "options", "head", "patch"]

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

        swagger_version = self.swagger.get("swagger")
        if swagger_version != "2.0":
            raise ValueError("Only Swagger 2.0 specs are supported, got %s" % swagger_version)

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
            for method in Swagger.METHODS:
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

    def get_interpolated_paths(self):
        paths = OrderedDict()
        for path, path_obj in iteritems(self.paths):
            new_path = path
            for method in Swagger.METHODS:
                operation = getattr(path_obj, method)
                if operation is not None:
                    for _, param in iteritems(operation.parameters):
                        if param.location == "path":
                            name = param.name
                            value = str(Swagger.get_data_for_type(param.type, param.format))
                            pattern = "{" + name + "}"
                            new_path = new_path.replace(pattern, value)
                        # TODO: what if param.location == "body" or something else?
                        # TODO: convert 'schema' from JSON Schema to concrete value?
            for _, param in iteritems(path_obj.parameters):
                if param.location == "path":
                    name = param.name
                    value = str(Swagger.get_data_for_type(param.type, param.format))
                    pattern = "{" + name + "}"
                    new_path = path.replace(pattern, value)
            path_obj = copy.deepcopy(path_obj)
            paths[new_path] = path_obj
        return paths

    def get_info(self):
        return copy.deepcopy(self.info)

    def get_host(self):
        return self.swagger.get("host")

    def get_base_path(self):
        return self.swagger.get("basePath")

    @staticmethod
    def get_data_for_type(data_type, _):
        if data_type == "string":
            return "string"
        elif data_type == "number":
            return 42
        elif data_type == "integer":
            return 13
        elif data_type == "boolean":
            return True
        elif data_type == "array":
            return [42]
        else:
            raise ValueError("Can't fake data for type %s" % data_type)
