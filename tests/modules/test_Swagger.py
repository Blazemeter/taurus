import logging

from bzt.modules.swagger import SwaggerConverter, Swagger
from tests import BZTestCase, __dir__


class TestSwaggerConverter(BZTestCase):
    def test_minimal_json(self):
        obj = SwaggerConverter({}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.json")
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.get("execution"))
        self.assertIsNotNone(config.get("scenarios"))

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual("http://petstore.swagger.io", scenario["default-address"])
        self.assertEqual(8, len(scenario["requests"]))
        # TODO: check body

    def test_minimal_yaml(self):
        obj = SwaggerConverter({}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.yaml")
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.get("execution"))
        self.assertIsNotNone(config.get("scenarios"))

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual("http://petstore.swagger.io", scenario["default-address"])
        self.assertEqual(3, len(scenario["requests"]))

    def test_interpolated_paths(self):
        swagger = Swagger()
        swagger.parse(__dir__() + "/../swagger/petstore.yaml")
        paths = list(swagger.get_paths().keys())
        self.assertEqual(paths, ["/pets", "/pets/{petId}", "/owners"])
        inter_paths = list(swagger.get_interpolated_paths().keys())
        self.assertEqual(inter_paths, ["/pets", "/pets/string", "/owners"])

    def test_query(self):
        obj = SwaggerConverter({}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.yaml")

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual(3, len(scenario["requests"]))
        requests = scenario["requests"]
        self.assertEqual(requests[0]["url"], "/v1/pets")
        self.assertEqual(requests[1]["url"], "/v1/pets/string")
        self.assertEqual(requests[2]["url"], "/v1/owners?limit=13")

    def test_headers(self):
        obj = SwaggerConverter({"get-only": False}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.yaml")

        scenario = config["scenarios"].get("Swagger-Petstore")
        requests = scenario["requests"]
        for request in requests[3:6]:
            self.assertIn("headers", request)
            self.assertIn("string", request["headers"].get("token"))

    def test_form_data(self):
        obj = SwaggerConverter({"get-only": False}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.yaml")

        requests = config["scenarios"]["Swagger-Petstore"]["requests"]
        request = requests[5]
        self.assertIn("body", request)
        self.assertEqual(request["body"].get("name"), "string")

    def test_get_only(self):
        obj = SwaggerConverter({}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.json")

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual(8, len(scenario["requests"]))
        self.assertTrue(all(req.get("method", "GET") == "GET" for req in scenario["requests"]))

    def test_get_only_disabled(self):
        obj = SwaggerConverter({"get-only": False}, logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.json")

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual(20, len(scenario["requests"]))
        self.assertFalse(all(req.get("method", "GET") == "GET" for req in scenario["requests"]))
