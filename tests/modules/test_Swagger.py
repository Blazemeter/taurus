import logging

from bzt.modules.swagger import SwaggerConverter
from tests import BZTestCase, __dir__


class TestSwaggerConverter(BZTestCase):
    def test_minimal_json(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.json")
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.get("execution"))
        self.assertIsNotNone(config.get("scenarios"))

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual("petstore.swagger.io", scenario["default-address"])
        self.assertEqual(20, len(scenario["requests"]))

    def test_minimal_yaml(self):
        obj = SwaggerConverter(logging.getLogger(''))
        config = obj.convert(__dir__() + "/../swagger/petstore.yaml")
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.get("execution"))
        self.assertIsNotNone(config.get("scenarios"))

        scenario = config["scenarios"].get("Swagger-Petstore")
        self.assertEqual("petstore.swagger.io", scenario["default-address"])
        self.assertEqual(3, len(scenario["requests"]))
