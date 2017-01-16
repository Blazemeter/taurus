import logging

from bzt.modules.soapui import SoapUIScriptConverter
from tests import BZTestCase, __dir__


class TestApacheBenchExecutor(BZTestCase):
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
        self.assertEqual("http://blazedemo.com/reserve.php", scenario["requests"][0]["url"])
        self.assertEqual("test index", scenario["requests"][0]["label"])
        self.assertEqual("http://example.com/body", scenario["requests"][1]["url"])
        self.assertEqual("posty", scenario["requests"][1]["label"])
        self.assertEqual("POST", scenario["requests"][1]["method"])
        self.assertIn("headers", scenario["requests"][1])
        self.assertEqual(scenario["requests"][1]["headers"].get("X-Header"), "X-Value")
        self.assertEqual(scenario["requests"][1]["headers"].get("X-Header-2"), "X-Value-2")

