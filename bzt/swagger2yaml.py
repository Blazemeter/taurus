"""
Swagger to YAML converter for Taurus

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
import copy
import logging
import os
import sys
import traceback
from collections import namedtuple, OrderedDict
from optparse import OptionParser

import yaml

from bzt import TaurusInternalException, TaurusConfigError
from bzt.cli import CLI
from bzt.engine import Configuration
from bzt.six import iteritems, parse, urlencode
from bzt.utils import BetterDict


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

    def _load(self, swagger_spec_fd):
        try:
            self.swagger = yaml_ordered_load(swagger_spec_fd, yaml.SafeLoader)
            self.log.info("Loaded Swagger spec %s", swagger_spec_fd)
        except IOError as exc:
            raise TaurusConfigError("Error when parsing Swagger file '%s': %s" % (swagger_spec_fd, exc))

    def _validate_swagger_version(self):
        swagger_version = self.swagger.get("swagger", self.swagger.get("openapi"))
        if swagger_version != "2.0":
            raise ValueError("Only Swagger 2.0 specs are supported, got %s" % swagger_version)

    def _extract_toplevel_definitions(self):
        self.info = self.swagger.get("info", {})

        for name, schema in iteritems(self.swagger.get("definitions", {})):
            self.definitions[name] = Swagger.Definition(name=name, schema=schema)

        for name, response in iteritems(self.swagger.get("responses", {})):
            self.responses[name] = Swagger.Response(name=name, description=response["description"],
                                                    schema=response.get("schema"), headers=response.get("headers"))

        for name, param in iteritems(self.swagger.get("parameters", {})):
            parameter = Swagger.Parameter(name=name, location=param.get("in"), description=param.get("description"),
                                          required=param.get("required"), schema=param.get("schema"),
                                          type=param.get("type"), format=param.get("format"))
            self.parameters[name] = parameter

    def _lookup_reference(self, reference):
        if not reference.startswith("#/"):
            return
        path = reference[2:].split('/')
        pointer = self.swagger
        for component in path:
            if component not in pointer:
                raise IndexError("Can't find location by reference %r at part %r" % (reference, component))
            pointer = pointer[component]
        self.log.debug("Found by reference %r: %r", reference, pointer)
        return pointer

    def _extract_operation(self, operation):
        parameters = OrderedDict()
        for param in operation.get("parameters", []):
            if "$ref" in param:
                param = self._lookup_reference(param["$ref"])
            param_name = param["name"]
            parameter = Swagger.Parameter(name=param_name, location=param.get("in"),
                                          description=param.get("description"), required=param.get("required"),
                                          schema=param.get("schema"), type=param.get("type"),
                                          format=param.get("format"))
            parameters[param_name] = parameter

        responses = OrderedDict()
        for name, resp in iteritems(operation.get("responses", {})):
            response = Swagger.Response(name=name, description=resp["description"], schema=resp.get("schema"),
                                        headers=resp.get("headers"))
            responses[name] = response

        return Swagger.Operation(summary=operation.get("summary"), description=operation.get("description"),
                                 operation_id=operation.get("operationId"), consumes=operation.get("consumes"),
                                 produces=operation.get("produces"), parameters=parameters, responses=responses)

    def _extract_paths(self):
        for name, path_item in iteritems(self.swagger["paths"]):
            path = {"ref": None, "get": None, "put": None, "post": None, "delete": None, "options": None, "head": None,
                    "patch": None, "parameters": {}}
            for method in Swagger.METHODS:
                if method in path_item:
                    operation = path_item[method]
                    path[method] = self._extract_operation(operation)

            for param in path_item.get("parameters", []):
                if "$ref" in param:
                    param = self._lookup_reference(param["$ref"])
                param_name = param["name"]
                parameter = Swagger.Parameter(name=param_name, location=param.get("in"),
                                              description=param.get("description"), required=param.get("required"),
                                              schema=param.get("schema"), type=param.get("type"),
                                              format=param.get("format"))
                path["parameters"][param_name] = parameter
            self.paths[name] = Swagger.Path(**path)

    def parse(self, swagger_spec_fd):
        self._load(swagger_spec_fd)
        self._validate_swagger_version()
        self._extract_toplevel_definitions()
        self._extract_paths()

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
    def get_data_for_type(data_type, data_format):
        del data_format
        if data_type == "string":
            return "some_string"
        elif data_type == "number":
            return 1
        elif data_type == "integer":
            return 1
        elif data_type == "boolean":
            return True
        elif data_type == "array":
            return [1, 2, 3]
        else:
            raise ValueError("Can't generate dummy data for type %s" % data_type)

    @staticmethod
    def get_data_for_schema(schema):
        del schema
        # TODO: generate dummy data from JSONSchema
        return None


class SwaggerConverter(object):
    def __init__(
            self,
            parent_log,
            scenarios_from_paths=False
    ):
        self.scenarios_from_paths = scenarios_from_paths
        self.log = parent_log.getChild(self.__class__.__name__)
        self.swagger = Swagger(self.log)

    def _handle_parameters(self, parameters):
        query_params = OrderedDict()
        form_data = {}
        request_body = None
        headers = {}
        for _, param in iteritems(parameters):
            if not param.required:
                continue
            if param.location == "header":
                name = param.name
                value = Swagger.get_data_for_type(param.type, param.format)
                headers[name] = value
            elif param.location == "query":
                name = param.name
                value = Swagger.get_data_for_type(param.type, param.format)
                query_params[name] = value
            elif param.location == "formData":
                name = param.name
                value = Swagger.get_data_for_type(param.type, param.format)
                form_data[name] = value
            elif param.location == "body":
                request_body = Swagger.get_data_for_schema(param.schema)
            elif param.location == "path":
                pass  # path parameters are resolved at a different level
            else:
                self.log.warning("Unsupported parameter location (%s). Skipping", param.location)
        return query_params, form_data, request_body, headers

    def _embed_query_in_path(self, path, query_dict):
        self.log.debug("Query dict: %s", query_dict)
        parts = parse.urlparse(path)
        query = urlencode(query_dict)
        replaced = parts._replace(query=query)
        return parse.urlunparse(replaced)

    def _extract_request(self, path, path_obj, method, operation):
        request = {}

        if method != "get":
            request["method"] = method.upper()

        if operation.operation_id is not None:
            request["label"] = operation.operation_id

        parameters = BetterDict()
        if path_obj.parameters:
            parameters.merge(path_obj.parameters)
        if operation.parameters:
            parameters.merge(operation.parameters)
        query_params, form_data, request_body, headers = self._handle_parameters(parameters)

        if headers:
            request["headers"] = headers

        if form_data and request_body:
            self.log.warning("Both form data and request body are specified. Omitting form data")

        if request_body:
            request["body"] = request_body
        elif form_data:
            request["body"] = form_data

        if query_params:
            url = self._embed_query_in_path(path, query_params)
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
                    request = self._extract_request(base_path + path, path_obj, method, operation)
                    # TODO: Swagger responses -> JMeter assertions?

                    if request is not None:
                        requests.append(request)

        return requests

    def _extract_scenarios_from_paths(self, paths, default_address):
        base_path = self.swagger.get_base_path()
        scenarios = OrderedDict()
        for path, path_obj in iteritems(paths):
            scenario_name = path
            self.log.info("Handling path %s", path)
            requests = []
            for method in Swagger.METHODS:
                operation = getattr(path_obj, method)
                if operation is not None:
                    if base_path:
                        path = self.join_relative_url(base_path, path)
                    self.log.debug("Handling method %s", method.upper())
                    request = self._extract_request(path, path_obj, method, operation)
                    requests.append(request)
                    # TODO: Swagger responses -> assertions?

            if requests:
                scenarios[scenario_name] = {
                    "default-address": default_address,
                    "requests": requests,
                }

        return scenarios

    @staticmethod
    def join_relative_url(*path):
        return '/'.join(s.strip('/') for s in (('',) + path))

    def convert_path(self, swagger_path):
        if not os.path.exists(swagger_path):
            raise ValueError("Swagger file %s doesn't exist" % swagger_path)
        with open(swagger_path) as swagger_fd:
            return self.convert(swagger_fd)

    def convert(self, swagger_fd):
        self.swagger.parse(swagger_fd)

        info = self.swagger.get_info()
        title = info.get("title", "Swagger")
        host = self.swagger.get_host()
        paths = self.swagger.get_interpolated_paths()
        schemes = self.swagger.swagger.get("schemes", ["http"])
        scheme = schemes[0]
        default_address = scheme + "://" + host
        scenario_name = title.replace(' ', '-')
        if self.scenarios_from_paths:
            scenarios = self._extract_scenarios_from_paths(paths, default_address)
            return {
                "scenarios": scenarios,
                "execution": [{
                    "concurrency": 1,
                    "scenario": scenario_name,
                    "hold-for": "1m",
                } for scenario_name, scenario in iteritems(scenarios)]
            }
        else:
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
                    "hold-for": "1m",
                }]
            }


class Swagger2YAML(object):
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

        self.log.info('Loading Swagger spec %s', self.file_to_convert)
        self.file_to_convert = os.path.abspath(os.path.expanduser(self.file_to_convert))
        if not os.path.exists(self.file_to_convert):
            raise TaurusInternalException("File does not exist: %s" % self.file_to_convert)
        self.converter = SwaggerConverter(
            self.log,
            scenarios_from_paths=self.options.scenarios_from_paths
        )
        try:
            converted_config = self.converter.convert_path(self.file_to_convert)
        except BaseException:
            self.log.error("Error while processing Swagger spec: %s", self.file_to_convert)
            raise

        exporter = Configuration()
        exporter.merge(converted_config)

        if self.options.file_name:
            file_name = self.options.file_name
        else:
            file_name = self.file_to_convert + "." + output_format.lower()

        exporter.dump(file_name, output_format)

        self.log.info("Done processing, result saved in %s", file_name)


def process(parsed_options, args):
    tool = Swagger2YAML(parsed_options, args[0])
    tool.process()


def main():
    usage = "Usage: swagger2yaml [input Swagger spec] [options]"
    parser = OptionParser(usage=usage, prog="swagger2yaml")
    parser.add_option('-v', '--verbose', action='store_true', default=False,
                      help="Prints all logging messages to console")
    parser.add_option('-o', '--out', dest="file_name",
                      help="Set output .yml file name, by default input file name + .yml is used")
    parser.add_option('-q', '--quiet', action='store_true', default=False, dest='quiet',
                      help="Do not display any log messages")
    parser.add_option('-j', '--json', action='store_true', default=False, dest='json',
                      help="Use JSON format for results")
    parser.add_option('-l', '--log', action='store', default=False, help="Log file location")
    parser.add_option('--scenarios-from-paths', action='store_true', default=False,
                      help="Generate one scenario per path (disabled by default)")
    parsed_options, args = parser.parse_args()
    if len(args) > 0:
        try:
            process(parsed_options, args)
        except BaseException as exc:
            logging.error("Exception during conversion: %s: %s", type(exc).__name__, str(exc))
            if not parsed_options.verbose:
                logging.error("Rerun with --verbose to see the stack trace")
            logging.debug("Exception: %s", traceback.format_exc())
            sys.exit(1)
        sys.exit(0)
    else:
        sys.stdout.write(usage + "\n")


if __name__ == "__main__":
    main()
