import logging

from bzt.modules.soapui import SoapUIScriptConverter
from tests import BZTestCase, __dir__


class TestSoapUIConverter(BZTestCase):
    def test_minimal(self):
        obj = SoapUIScriptConverter(logging.getLogger(''))
        config = obj.convert(__dir__() + "/../soapui/project.xml")

        self.assertIn("execution", config)
        self.assertEqual(1, len(config["execution"]))
        execution = config["execution"][0]
        self.assertEqual("TestSuite 1-index", execution.get("scenario"))
        self.assertEqual(60, execution.get("hold-for"))
        self.assertEqual(10, execution.get("concurrency"))

        self.assertIn("scenarios", config)
        self.assertIn("TestSuite 1-index", config["scenarios"])

        scenario = config["scenarios"]["TestSuite 1-index"]
        self.assertIn("requests", scenario)
        self.assertEqual(2, len(scenario["requests"]))

        first_req = scenario["requests"][0]
        self.assertEqual("http://blazedemo.com/reserve.php", first_req["url"])
        self.assertEqual("test index", first_req["label"])
        self.assertIn("headers", first_req)
        self.assertEqual(first_req["headers"].get("X-Custom-Header"), "Value")
        self.assertIn("assert", first_req)
        self.assertEqual(2, len(first_req["assert"]))
        self.assertEqual("BlazeDemo", first_req["assert"][0]["contains"][0])
        self.assertEqual(False, first_req["assert"][0]["not"])
        self.assertEqual("BlazeDemou", first_req["assert"][1]["contains"][0])
        self.assertEqual(True, first_req["assert"][1]["not"])

        second_req = scenario["requests"][1]
        self.assertEqual("http://example.com/body", second_req["url"])
        self.assertEqual("posty", second_req["label"])
        self.assertEqual("POST", second_req["method"])
        self.assertIn("headers", second_req)
        self.assertEqual(second_req["headers"].get("X-Header"), "X-Value")
        self.assertEqual(second_req["headers"].get("X-Header-2"), "X-Value-2")

