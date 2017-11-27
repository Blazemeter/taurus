from . import MockJMeterExecutor
from bzt.modules.jmeter import JMeterScenarioBuilder
from tests import BZTestCase, RESOURCES_DIR
from tempfile import mkstemp
from bzt.six import etree


class TestScenarioBuilder(BZTestCase):
    def setUp(self):
        super(TestScenarioBuilder, self).setUp()
        executor = MockJMeterExecutor({"scenario": "SB"})
        executor.engine.config.merge({"scenarios": {"SB": {}}})

        self.obj = JMeterScenarioBuilder(executor)

        _, self.jmx = mkstemp()

    def configure(self, scenario, version="3.3"):
        self.obj.scenario.data.merge(scenario)
        self.obj.executor.settings["version"] = version

    @staticmethod
    def get_plugin_json_extractor_config(xml_tree):
        cfg = {}
        block_name = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor.JSONPathExtractor"
        blocks = xml_tree.findall(".//%s" % block_name)
        for block in blocks:
            varname = block.find(".//stringProp[@name='VAR']")
            jsonpath = block.find(".//stringProp[@name='JSONPATH']")
            default = block.find(".//stringProp[@name='DEFAULT']")
            subject = block.find(".//stringProp[@name='SUBJECT']")
            from_variable = block.find(".//stringProp[@name='VARIABLE']")
            varname = varname.text
            jsonpath = jsonpath.text
            if default is not None:
                default = default.text
            if (subject is not None) and subject.text == "VAR" and (from_variable is not None):
                from_variable = from_variable.text
            else:
                from_variable = None
            cfg[varname] = {"jsonpath": jsonpath, "default": default, "from_variable": from_variable}
        return cfg

    @staticmethod
    def get_internal_json_extractor_config(xml_tree):
        cfg = {}
        blocks = xml_tree.findall(".//JSONPostProcessor")
        for block in blocks:
            varname = block.find(".//stringProp[@name='JSONPostProcessor.referenceNames']")
            jsonpath = block.find(".//stringProp[@name='JSONPostProcessor.jsonPathExprs']")
            default = block.find(".//stringProp[@name='JSONPostProcessor.defaultValues']")
            match_no = block.find(".//stringProp[@name='JSONPostProcessor.match_numbers']")
            scope = block.find(".//stringProp[@name='Sample.scope']")
            from_variable = block.find(".//stringProp[@name='Scope.variable']")
            concat = block.find(".//booleanProp[@name='JSONPostProcessor.compute_concat']")
            varname = varname.text
            jsonpath = jsonpath.text
            if default is not None:
                default = default.text
            if match_no is not None:
                match_no = match_no.text

            if (scope is not None) and scope.text == "variable" and (from_variable is not None):
                scope = scope.text
                from_variable = from_variable.text
            else:
                scope = None
                from_variable = None

            cfg[varname] = {"jsonpath": jsonpath, "default": default, "scope": scope,
                            "from_variable": from_variable, "match_no": match_no, "concat": concat}

        return cfg

    def test_plugin_extractor_config_reader(self):
        xml_tree = etree.fromstring(open(RESOURCES_DIR + "jmeter/jmx/json_extractors.jmx", "rb").read())
        target = {'pv1': {'default': 'pd1', 'from_variable': 'pJV', 'jsonpath': 'pe1'},
                  'pv2': {'default': None, 'from_variable': None, 'jsonpath': 'pe2'}}
        self.assertEqual(target, self.get_plugin_json_extractor_config(xml_tree))

    def test_internal_extractor_config_reader(self):
        xml_tree = etree.fromstring(open(RESOURCES_DIR + "jmeter/jmx/json_extractors.jmx", "rb").read())
        target = {'v1': {'from_variable': None, 'default': 'd1',
                         'match_no': 'm1', 'jsonpath': 'e1', 'scope': None, 'concat': None},
                  'v2': {'from_variable': None, 'default': 'd2',
                         'match_no': 'm2', 'jsonpath': 'e2', 'scope': None, 'concat': None},
                  'v3': {'from_variable': None, 'default': 'd3',
                         'match_no': 'm3', 'jsonpath': 'e3', 'scope': None, 'concat': None},
                  'v4': {'from_variable': 'var4', 'default': 'd4',
                         'match_no': '4', 'jsonpath': 'e4', 'scope': 'variable', 'concat': None},
                  'v-empty': {'from_variable': None, 'default': None,
                              'match_no': None, 'jsonpath': 'p1', 'scope': None, 'concat': None}}

        xml = self.get_internal_json_extractor_config(xml_tree)
        self.assertEqual(target, xml)

    def test_old_jmeter(self):
        """ versions before 3.0 must use JSON plugin for extracting purposes """
        self.configure(scenario={"requests": [{
                "url": "http://blazedemo.com",
                "extract-jsonpath": {
                    "IP": "$.net[0].ip",
                    "URL": {
                        "jsonpath": "$.net[1].url",
                        "default": "def",
                        "from-variable": "Jm_VaR"}}}]},
            version="2.13")
        self.obj.save(self.jmx)
        xml_tree = etree.fromstring(open(self.jmx, "rb").read())
        cfg = self.get_plugin_json_extractor_config(xml_tree)
        self.assertEqual(2, len(cfg))
        target = {
            "IP": {"jsonpath": "$.net[0].ip", "default": "NOT_FOUND", "from_variable": None},
            "URL": {"jsonpath": "$.net[1].url", "default": "def", "from_variable": "Jm_VaR"}}
        self.assertEqual(target, cfg)

    def test_new_extractor(self):
        """ versions after 3.0 use integrated JSON extractor """
        self.configure(scenario={"requests": [{
            "url": "http://blazedemo.com",
            "extract-jsonpath": {
                "IP": "$.net[0].ip",
                "URL": {"jsonpath": "$.url[1]", "default": "d1", "scope": "all", "concat": True, "from_variable": "V"},
                "ID": {"jsonpath": "$.net[3].id", "default": "d2", "scope": "children", "concat": True},
                "NuM": {"jsonpath": "$.num", "scope": "variable", "from-variable": "JMVaR", "match_no": 3},
            }}]},
            version=3.3)
        self.obj.save(self.jmx)
        xml_tree = etree.fromstring(open(self.jmx, "rb").read())
        cfg = self.get_internal_json_extractor_config(xml_tree)
        self.assertEqual(4, len(cfg))
        target = {'IP': {'from_variable': None, 'default': 'NOT_FOUND', 'match_no': '-1',
                         'jsonpath': '$.net[0].ip', 'scope': None, 'concat': None},
                  'URL': {'from_variable': None, 'default': 'd1', 'match_no': '-1',
                          'jsonpath': '$.url[1]', 'scope': None, 'concat': None},
                  'ID': {'from_variable': None, 'default': 'd2', 'match_no': '-1',
                         'jsonpath': '$.net[3].id', 'scope': None, 'concat': None},
                  'NuM': {'from_variable': 'JMVaR', 'default': 'NOT_FOUND', 'match_no': '-1',
                          'jsonpath': '$.num', 'scope': 'variable', 'concat': None}}

        self.assertEqual(target, cfg)
