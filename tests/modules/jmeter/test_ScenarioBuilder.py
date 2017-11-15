from . import MockJMeterExecutor
from bzt.modules.jmeter import JMeterScenarioBuilder
from tests import BZTestCase
from tempfile import NamedTemporaryFile
from bzt.six import etree


class TestScenarioBuilder(BZTestCase):
    def setUp(self):
        super(TestScenarioBuilder, self).setUp()
        executor = MockJMeterExecutor({"scenario": "SB"})
        executor.engine.config.merge({"scenarios": {"SB": {}}})

        self.obj = JMeterScenarioBuilder(executor)

        self.jmx_fd = NamedTemporaryFile()
        self.jmx = self.jmx_fd.name

    def tearDown(self):
        self.jmx_fd.close()
        super(TestScenarioBuilder, self).tearDown()

    def configure(self, scenario, version="3.3"):
        self.obj.scenario.data.merge(scenario)
        self.obj.executor.settings["version"] = version

    def test_old_jmeter(self):
        self.configure(scenario={"requests": [{
                "url": "http://blazedemo.com",
                "extract-jsonpath": {"IP": "$.net[0].ip"}}]},
            version="2.13")
        self.obj.save(self.jmx)
        xml_tree = etree.fromstring(open(self.jmx, "rb").read())
        pass
