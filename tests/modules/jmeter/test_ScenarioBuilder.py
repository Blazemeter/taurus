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
        """ versions before 3.0 must use JSON plugin for extracting purposes """
        self.configure(scenario={"requests": [{
                "url": "http://blazedemo.com",
                "extract-jsonpath": {"IP": "$.net[0].ip"}}]},
            version="2.13")
        self.obj.save(self.jmx)
        xml_tree = etree.fromstring(open(self.jmx, "rb").read())
        block_name = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor.JSONPathExtractor"
        blocks = xml_tree.findall(".//%s" % block_name)
        self.assertEqual(1, len(blocks))
        self.assertEqual("IP", blocks[0].find(".//stringProp[@name='VAR']").text)
        self.assertEqual("$.net[0].ip", blocks[0].find(".//stringProp[@name='JSONPATH']").text)
        self.assertEqual("NOT_FOUND", blocks[0].find(".//stringProp[@name='DEFAULT']").text)

    def test_new_extractor(self):
        """ versions after 3.0 use integrated JSON extractor """
        self.configure(scenario={"requests": [{
            "url": "http://blazedemo.com",
            "extract-jsonpath": {"IP": "$.net[0].ip"}}]},
            version="3.3")
        self.obj.save(self.jmx)
        xml_tree = etree.fromstring(open(self.jmx, "rb").read())
        prefix = "JSONPostProcessor"
        blocks = xml_tree.findall(".//%s" % prefix)
        self.assertEqual(1, len(blocks))
        self.assertEqual("IP", blocks[0].find(".//stringProp[@name='%s.referenceNames']" % prefix).text)
        self.assertEqual("$.net[0].ip", blocks[0].find(".//stringProp[@name='%s.jsonPathExprs']" % prefix).text)
        self.assertEqual("NOT_FOUND", blocks[0].find(".//stringProp[@name='%s.defaultValues']" % prefix).text)

