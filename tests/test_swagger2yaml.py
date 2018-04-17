import logging

import yaml

from bzt.six import iteritems
from bzt.swagger2yaml import SwaggerConverter, Swagger, Swagger2YAML, process
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul


class FakeOptions(object):
    def __init__(self, verbose=True, file_name=None, quiet=False, json=False, log=False, scenarios_from_paths=False):
        self.verbose = verbose
        self.file_name = file_name
        self.quiet = quiet
        self.json = json
        self.log = log
        self.scenarios_from_paths = scenarios_from_paths


class TestSwagger2YAML(BZTestCase):
    def setUp(self):
        super(TestSwagger2YAML, self).setUp()
        self.engine = EngineEmul()

    def _get_swagger2yaml(self, path, file_name=None):
        return Swagger2YAML(FakeOptions(file_name=file_name), RESOURCES_DIR + path)

    def _get_tmp(self, prefix='test', suffix='.yml'):
        return self.engine.create_artifact(prefix, suffix)

    def test_convert(self):
        self.maxDiff = None
        source = RESOURCES_DIR + "/swagger/petstore.json"
        result = self._get_tmp()
        options = FakeOptions(file_name=result)
        process(options, [source])
        actual = yaml.load(open(result).read())
        expected = yaml.load(open(RESOURCES_DIR + "/swagger/petstore-converted.yaml").read())
        self.assertEqual(actual, expected)

    def test_convert_scenarios_from_paths(self):
        self.maxDiff = None
        source = RESOURCES_DIR + "/swagger/bzm-api.json"
        result = self._get_tmp()
        options = FakeOptions(file_name=result, scenarios_from_paths=True)
        process(options, [source])
        actual = yaml.load(open(result).read())
        expected = yaml.load(open(RESOURCES_DIR + "/swagger/bzm-api-converted.yaml").read())
        self.assertEqual(actual, expected)


class TestSwaggerConverter(BZTestCase):
    def test_minimal_json(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert_path(RESOURCES_DIR + "/swagger/petstore.json")
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.get("execution"))
        self.assertIsNotNone(config.get("scenarios"))

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual("http://petstore.swagger.io", scenario["default-address"])
        self.assertEqual(20, len(scenario["requests"]))

    def test_minimal_yaml(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert_path(RESOURCES_DIR + "/swagger/petstore.yaml")
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.get("execution"))
        self.assertIsNotNone(config.get("scenarios"))

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual("http://petstore.swagger.io", scenario["default-address"])
        self.assertEqual(6, len(scenario["requests"]))

    def test_interpolated_paths(self):
        swagger = Swagger()
        swagger.parse(open(RESOURCES_DIR + "/swagger/petstore.yaml"))
        paths = list(swagger.get_paths().keys())
        self.assertEqual(paths, ["/pets", "/pets/{petId}", "/owners"])
        inter_paths = list(swagger.get_interpolated_paths().keys())
        self.assertEqual(inter_paths, ["/pets", "/pets/some_string", "/owners"])

    def test_query(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert_path(RESOURCES_DIR + "/swagger/petstore.yaml")

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual(6, len(scenario["requests"]))
        requests = scenario["requests"]
        self.assertEqual(requests[0]["url"], "/v1/pets")
        self.assertEqual(requests[1]["url"], "/v1/pets")
        self.assertEqual(requests[2]["url"], "/v1/pets/some_string")
        self.assertEqual(requests[3]["url"], "/v1/owners?limit=1")

    def test_headers(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert_path(RESOURCES_DIR + "/swagger/petstore.yaml")

        scenario = config["scenarios"].get("Swagger-Petstore")
        requests = scenario["requests"]
        for request in requests[3:6]:
            self.assertIn("headers", request)
            self.assertIn("some_string", request["headers"].get("token"))

    def test_form_data(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert_path(RESOURCES_DIR + "/swagger/petstore.yaml")

        requests = config["scenarios"]["Swagger-Petstore"]["requests"]
        request = requests[5]
        self.assertIn("body", request)
        self.assertEqual(request["body"].get("name"), "some_string")

    def test_referenced_parameters(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert_path(RESOURCES_DIR + "/swagger/bzm-api.json")

    def test_scenarios_from_paths(self):
        obj = SwaggerConverter(logging.getLogger(''), scenarios_from_paths=True)
        config = obj.convert_path(RESOURCES_DIR + "/swagger/bzm-api.json")
        self.assertEqual(len(config["scenarios"]), 5)

        scenario_names = set(key for key, _ in iteritems(config["scenarios"]))
        self.assertEqual({"/reports", "/reports/1", "/tests", "/tests/1", "/tests/1/start"}, scenario_names)

        self.assertEqual(len(config["execution"]), 5)

        for scenario_name, scenario in iteritems(config["scenarios"]):
            self.assertEqual(scenario["default-address"], "https://a.blazemeter.com")
            scenario_requests = scenario["requests"]
            self.assertGreater(len(scenario_requests), 0)
            for scenario_request in scenario_requests:
                self.assertRegexpMatches(scenario_request["url"], "\/api\/v4\/.*")

        self.assertEqual(len(config["scenarios"]["/reports"]["requests"]), 1)
        self.assertEqual(len(config["scenarios"]["/reports/1"]["requests"]), 1)
        self.assertEqual(len(config["scenarios"]["/tests"]["requests"]), 2)
        self.assertEqual(len(config["scenarios"]["/tests/1"]["requests"]), 4)
        self.assertEqual(len(config["scenarios"]["/tests/1/start"]["requests"]), 1)

