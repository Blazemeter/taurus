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
import json
import logging
import os
import re
import sys
import traceback
from collections import namedtuple, OrderedDict
from optparse import OptionParser
from urllib import parse
from urllib.parse import urlencode

import yaml

from bzt import TaurusInternalException, TaurusConfigError
from bzt.cli import CLI
from bzt.engine import Configuration
from bzt.utils import iteritems, BetterDict


def yaml_ordered_load(stream, Loader=yaml.SafeLoader, object_pairs_hook=OrderedDict):
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

    INTERPOLATE_WITH_VALUES = 'values'
    INTERPOLATE_WITH_JMETER_VARS = 'variables'
    INTERPOLATE_DISABLE = 'none'

    Definition = namedtuple("Definition", "name, schema")
    Parameter = namedtuple("Parameter", "name, location, description, required, schema, type, format")
    Response = namedtuple("Response", "name, description, schema, headers")
    Path = namedtuple("Path", "ref, get, put, post, delete, options, head, patch, parameters")
    Operation = namedtuple("Operation",
                           "summary, description, operation_id, consumes, produces, parameters, responses, security")
    SecurityDef = namedtuple("SecurityDef", "type, description, name, location")

    def __init__(self, parent_log=None):
        self.log = (parent_log or logging.getLogger('')).getChild(self.__class__.__name__)
        self.swagger = None
        self.info = None
        self.definitions = {}
        self.parameters = {}
        self.responses = {}
        self.paths = OrderedDict()
        self.security_defs = {}
        self.default_security = []

    def _load(self, swagger_spec_fd):
        content = swagger_spec_fd.read()
        try:
            self.log.debug("Loading Swagger spec as YAML")
            self.swagger = yaml_ordered_load(content, yaml.SafeLoader)
            self.log.info("Loaded Swagger spec %s", swagger_spec_fd)
        except BaseException as exc:
            self.log.debug("Can't parse Swagger spec as YAML")
            try:
                self.log.debug("Loading Swagger spec as JSON")
                self.swagger = json.loads(content)
                self.log.info("Loaded Swagger spec %s", swagger_spec_fd)
            except BaseException:
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
            self.responses[name] = Swagger.Response(name=name, description=response.get("description"),
                                                    schema=response.get("schema"), headers=response.get("headers"))

        for name, param in iteritems(self.swagger.get("parameters", {})):
            parameter = Swagger.Parameter(name=name, location=param.get("in"), description=param.get("description"),
                                          required=param.get("required"), schema=param.get("schema"),
                                          type=param.get("type"), format=param.get("format"))
            self.parameters[name] = parameter

        for name, secdef in iteritems(self.swagger.get("securityDefinitions", {})):
            self.security_defs[name] = Swagger.SecurityDef(type=secdef.get('type'),
                                                           description=secdef.get('description'),
                                                           name=secdef.get('name'),
                                                           location=secdef.get('in'))

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
            response = Swagger.Response(name=name, description=resp.get("description"), schema=resp.get("schema"),
                                        headers=resp.get("headers"))
            responses[name] = response

        return Swagger.Operation(summary=operation.get("summary"), description=operation.get("description"),
                                 operation_id=operation.get("operationId"), consumes=operation.get("consumes"),
                                 produces=operation.get("produces"), parameters=parameters, responses=responses,
                                 security=operation.get("security"))

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

    def get_interpolated_paths(self, parameter_interpolation=INTERPOLATE_WITH_VALUES):
        paths = OrderedDict()
        replacer_regex = lambda name: r'(?<!\$)(\{' + name + r'\})'  # replace '{name}', but skip '${name}'
        for path, path_obj in iteritems(self.paths):
            new_path = path
            for method in Swagger.METHODS:
                operation = getattr(path_obj, method)
                if operation is not None:
                    for _, param in iteritems(operation.parameters):
                        if param.location == "path":
                            name = param.name
                            if parameter_interpolation == Swagger.INTERPOLATE_WITH_VALUES:
                                value = str(Swagger.get_data_for_type(param.type, param.format))
                            elif parameter_interpolation == Swagger.INTERPOLATE_WITH_JMETER_VARS:
                                value = "${" + param.name + "}"
                            else:
                                value = None
                            if value is not None:
                                new_path = re.sub(replacer_regex(name), value, new_path)
            for _, param in iteritems(path_obj.parameters):
                if param.location == "path":
                    name = param.name
                    if parameter_interpolation == Swagger.INTERPOLATE_WITH_VALUES:
                        value = str(Swagger.get_data_for_type(param.type, param.format))
                    elif parameter_interpolation == Swagger.INTERPOLATE_WITH_JMETER_VARS:
                        value = "${" + param.name + "}"
                    else:
                        value = None
                    if value is not None:
                        new_path = re.sub(replacer_regex(name), value, new_path)
            path_obj = copy.deepcopy(path_obj)
            paths[new_path] = path_obj
        return paths

    def get_info(self):
        return copy.deepcopy(self.info)

    def get_host(self):
        host = self.swagger.get("host", "")
        if not host:
            self.log.warning("Warning: no `host` declared, using HOST placeholder")
            host = "HOST"
        return host

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
            scenarios_from_paths=False,
            parameter_interpolation=Swagger.INTERPOLATE_WITH_VALUES,
    ):
        self.scenarios_from_paths = scenarios_from_paths
        self.parameter_interpolation = parameter_interpolation
        self.log = parent_log.getChild(self.__class__.__name__)
        self.swagger = Swagger(self.log)

    def _interpolate_parameter(self, param):
        if self.parameter_interpolation == Swagger.INTERPOLATE_WITH_VALUES:
            return Swagger.get_data_for_type(param.type, param.format)
        elif self.parameter_interpolation == Swagger.INTERPOLATE_WITH_JMETER_VARS:
            return '${' + param.name + '}'
        else:
            return None

    def _interpolate_body(self, param):
        if self.parameter_interpolation == Swagger.INTERPOLATE_WITH_VALUES:
            return Swagger.get_data_for_schema(param.schema)
        elif self.parameter_interpolation == Swagger.INTERPOLATE_WITH_JMETER_VARS:
            return '${body}'
        else:
            return None

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
                value = self._interpolate_parameter(param)
                headers[name] = value
            elif param.location == "query":
                name = param.name
                value = self._interpolate_parameter(param)
                query_params[name] = value
            elif param.location == "formData":
                name = param.name
                value = self._interpolate_parameter(param)
                form_data[name] = value
            elif param.location == "body":
                request_body = self._interpolate_body(param)
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

    def _extract_requests_from_paths(self, paths, scenario_name, default_address, global_security):
        base_path = self.swagger.get_base_path()
        requests = []
        scenario = {
            "default-address": "${default-address}",
            "variables": {},
        }
        global_vars = {
            "default-address": default_address,
        }
        if base_path:
            global_vars["default-path"] = base_path

        if global_security:
            self._add_global_security(scenario, global_security, global_vars)

        for path, path_obj in iteritems(paths):
            self.log.debug("Handling path %s", path)
            for method in Swagger.METHODS:
                operation = getattr(path_obj, method)
                if operation is not None:
                    self.log.debug("Handling method %s", method.upper())
                    if base_path:
                        route = "${default-path}" + path
                    else:
                        route = path
                    request = self._extract_request(route, path_obj, method, operation)
                    # TODO: Swagger responses -> JMeter assertions?

                    if request is not None:
                        if operation.security:
                            self._add_local_security(request, operation.security, scenario)
                        elif global_security:
                            self._add_local_security(request, global_security, scenario, disable_basic=True)

                        requests.append(request)

        if not scenario["variables"]:
            scenario.pop("variables")
        scenario["requests"] = requests

        config = {
            "scenarios": {
                scenario_name: scenario
            },
            "execution": [{
                "concurrency": 1,
                "scenario": scenario_name,
                "hold-for": "1m",
            }]
        }
        if global_vars:
            config["settings"] = {"env": global_vars}
        return config

    def _extract_scenarios_from_paths(self, paths, default_address, global_security):
        base_path = self.swagger.get_base_path()
        scenarios = OrderedDict()
        global_vars = {
            "default-address": default_address
        }
        if base_path:
            global_vars["default-path"] = base_path

        for path, path_obj in iteritems(paths):
            self.log.info("Handling path %s", path)

            scenario_name = path
            scenario = {
                "default-address": "${default-address}",
                "variables": {},
            }

            if base_path:
                route = "${default-path}" + path
            else:
                route = path

            requests = []
            for method in Swagger.METHODS:
                operation = getattr(path_obj, method)
                if operation is not None:
                    self.log.debug("Handling method %s", method.upper())
                    request = self._extract_request(route, path_obj, method, operation)

                    if operation.security:
                        self._add_local_security(request, operation.security, scenario)
                    elif global_security:
                        self._add_local_security(request, global_security, scenario)

                    requests.append(request)
                    # TODO: Swagger responses -> assertions?

            if not requests:
                continue

            scenario["requests"] = requests

            if global_security:
                self._add_global_security(scenario, global_security, global_vars)

            if not scenario["variables"]:
                scenario.pop("variables")

            scenarios[scenario_name] = scenario

        config = {
            "scenarios": scenarios,
            "execution": [{
                "concurrency": 1,
                "scenario": scenario_name,
                "hold-for": "1m",
            } for scenario_name, scenario in iteritems(scenarios)]
        }
        if global_vars:
            config["settings"] = {"env": global_vars}
        return config

    def _insert_global_basic_auth(self, scenario, global_vars):
        headers = scenario.get('headers', {})

        headers['Authorization'] = 'Basic ${__base64Encode(${auth})}'
        global_vars['auth'] = 'USER:PASSWORD'

        scenario['headers'] = headers

    def _insert_local_basic_auth(self, request, scenario):
        headers = request.get('headers', {})
        variables = scenario.get('variables', {})

        headers['Authorization'] = 'Basic ${__base64Encode(${auth})}'
        variables['auth'] = 'USER:PASSWORD'

        request['headers'] = headers
        scenario['variables'] = variables

    def _insert_global_apikey_auth(self, scenario, sec_name, param_name, location, global_vars):
        # location == 'query' is deliberately ignored
        if location == 'header':
            header_name = sec_name
            var_name = param_name

            headers = scenario.get('headers', {})

            headers[header_name] = '${' + var_name + '}'
            global_vars[var_name] = 'TOKEN'

            scenario['headers'] = headers

    def _insert_local_apikey_auth(self, request, scenario, sec_name, param_name, location):
        # location == 'header' is deliberately ignored
        if location == 'query':
            query_name = sec_name
            var_name = param_name

            body = request.get('body', {})
            variables = scenario.get('variables', {})

            body[query_name] = '${' + var_name + '}'
            variables[var_name] = 'TOKEN'

            request['body'] = body
            scenario['variables'] = variables

    def _add_global_security(self, scenario, global_security, global_vars):
        if not global_security:
            return

        security = global_security[0]
        for sec_name, _ in iteritems(security):
            secdef = self.swagger.security_defs.get(sec_name)
            if not secdef:
                self.log.warning("Security definition %r not found, skipping" % sec_name)
                continue

            if secdef.type == 'basic':
                self._insert_global_basic_auth(scenario, global_vars)
            elif secdef.type == 'apiKey':
                if secdef.name is None:
                    self.log.warning("apiKey security definition has no header name, skipping")
                    continue
                if secdef.location is None:
                    self.log.warning("apiKey location (`in`) is not given, assuming header")
                    secdef.location = 'header'

                self._insert_global_apikey_auth(scenario, secdef.name, sec_name, secdef.location, global_vars)

            elif secdef.type == 'oauth2':
                self.log.warning("OAuth2 security is not yet supported, skipping")
                continue

    def _add_local_security(self, request, securities, scenario, disable_basic=False):
        if not securities:
            return  # TODO: disable global security for request

        security = securities[0]
        for sec_name, _ in iteritems(security):
            secdef = self.swagger.security_defs.get(sec_name)
            if not secdef:
                self.log.warning("Security definition %r not found, skipping" % sec_name)
                continue

            if secdef.type == 'basic':
                if not disable_basic:
                    self._insert_local_basic_auth(request, scenario)
            elif secdef.type == 'apiKey':
                if secdef.name is None:
                    self.log.warning("apiKey security definition has no header name, skipping")
                    continue
                if secdef.location is None:
                    self.log.warning("apiKey location (`in`) is not given, assuming header")
                    secdef.location = 'header'

                self._insert_local_apikey_auth(request, scenario, secdef.name, sec_name, secdef.location)

            elif secdef.type == 'oauth2':
                self.log.warning("OAuth2 security is not yet supported, skipping")
                continue

    @staticmethod
    def join_base_with_endpoint_url(*path):
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
        paths = self.swagger.get_interpolated_paths(self.parameter_interpolation)
        schemes = self.swagger.swagger.get("schemes", ["http"])
        scheme = schemes[0]
        security = self.swagger.swagger.get("security", [])
        default_address = scheme + "://" + host
        scenario_name = title.replace(' ', '-')
        if self.scenarios_from_paths:
            config = self._extract_scenarios_from_paths(paths, default_address, security)
        else:
            config = self._extract_requests_from_paths(paths, scenario_name, default_address, security)
        return config


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
            scenarios_from_paths=self.options.scenarios_from_paths,
            parameter_interpolation=self.options.parameter_interpolation,
        )
        try:
            converted_config = self.converter.convert_path(self.file_to_convert)
        except BaseException:
            self.log.error("Error while processing Swagger spec: %s", self.file_to_convert)
            raise

        exporter = Configuration.from_dict(converted_config)

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
    parser.add_option('--parameter-interpolation', action='store', default='values',
                      help="Templated parameters interpolation. Valid values are 'variables', 'values', 'none'")
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
