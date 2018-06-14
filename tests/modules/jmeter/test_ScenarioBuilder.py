import json

from bzt import TaurusConfigError
from bzt.jmx import JMX
from bzt.jmx.actions import ActionsHandler
from bzt.jmx.assertions import AssertionsHandler
from bzt.jmx.extractors import ExtractorsHandler
from bzt.jmx.http import HTTPProtocolHandler
from bzt.jmx.logic import LogicControllersHandler
from bzt.jmx.test_actions import TestActionsHandler
from bzt.modules.jmeter import JMeterScenarioBuilder
from bzt.six import etree
from tests import BZTestCase, RESOURCES_DIR
from . import MockJMeterExecutor


class TestScenarioBuilder(BZTestCase):
    def setUp(self):
        super(TestScenarioBuilder, self).setUp()
        executor = MockJMeterExecutor({"scenario": "SB"})
        executor.engine.config.merge({"scenarios": {"SB": {}}})
        executor.settings['protocol-handlers'] = [
            HTTPProtocolHandler.__module__ + '.' + HTTPProtocolHandler.__name__,
            LogicControllersHandler.__module__ + '.' + LogicControllersHandler.__name__,
            ActionsHandler.__module__ + '.' + ActionsHandler.__name__,
            AssertionsHandler.__module__ + '.' + AssertionsHandler.__name__,
            ExtractorsHandler.__module__ + '.' + ExtractorsHandler.__name__,
            TestActionsHandler.__module__ + '.' + TestActionsHandler.__name__
        ]

        self.obj = JMeterScenarioBuilder(executor)

        # _, self.jmx = mkstemp()
        self.jmx = executor.engine.create_artifact('generated', '.jmx')

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
        target = {'IP': {'from_variable': None, 'default': 'NOT_FOUND', 'match_no': '0',
                         'jsonpath': '$.net[0].ip', 'scope': None, 'concat': None},
                  'URL': {'from_variable': None, 'default': 'd1', 'match_no': '0',
                          'jsonpath': '$.url[1]', 'scope': None, 'concat': None},
                  'ID': {'from_variable': None, 'default': 'd2', 'match_no': '0',
                         'jsonpath': '$.net[3].id', 'scope': None, 'concat': None},
                  'NuM': {'from_variable': 'JMVaR', 'default': 'NOT_FOUND', 'match_no': '0',
                          'jsonpath': '$.num', 'scope': 'variable', 'concat': None}}

        self.assertEqual(target, cfg)

    def test_transaction_include_timers(self):
        self.configure(scenario={"requests": [{
            "transaction": "tran",
            "include-timers": True,
            "do": ["http://blazedemo.com/"]}]
        })
        self.obj.save(self.jmx)
        xml_tree = etree.fromstring(open(self.jmx, "rb").read())
        include = xml_tree.find(".//TransactionController/boolProp[@name='TransactionController.includeTimers']")
        self.assertIsNotNone(include)
        self.assertEqual(include.text, 'true')

    def test_single_request(self):
        self.configure(scenario={
            "requests": ["http://blazedemo.com/"]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        domain = xml_tree.findtext(".//HTTPSamplerProxy/stringProp[@name='HTTPSampler.domain']")
        self.assertEqual('blazedemo.com', domain)
        path = xml_tree.findtext(".//HTTPSamplerProxy/stringProp[@name='HTTPSampler.path']")
        self.assertEqual('/', path)

    def test_if_controller(self):
        self.configure(scenario={
            "requests": [{
                "if": "cond",
                "then": [
                    "http://blazedemo.com/?then=true"
                ],
                "else": [
                    "http://blazedemo.com/?else=true"
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        condition = xml_tree.findtext(".//IfController[@testname=\"If 'cond'\"]/stringProp[@name='IfController.condition']")
        self.assertEqual('cond', condition)
        else_condition = xml_tree.findtext(".//IfController[@testname=\"If not 'cond'\"]/stringProp[@name='IfController.condition']")
        self.assertEqual('!(cond)', else_condition)

    def test_loop_controller(self):
        self.configure(scenario={
            "requests": [{
                "loop": 10,
                "do": [
                    "http://blazedemo.com/?loop=true"
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        loops = xml_tree.findtext(".//LoopController/stringProp[@name='LoopController.loops']")
        self.assertEqual('10', loops)

    def test_loop_forever(self):
        self.configure(scenario={
            "requests": [{
                "loop": "forever",
                "do": [
                    "http://blazedemo.com/"
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        controller = xml_tree.find(".//LoopController")
        self.assertIsNotNone(controller)

        fprops = xml_tree.findall(".//boolProp[@name='LoopController.continue_forever']")
        for fprop in fprops:
            pptag = fprop.getparent().getparent().tag
            if pptag == "ThreadGroup":
                self.assertEqual("false", fprop.text)
            elif pptag == "hashTree":
                self.assertEqual("true", fprop.text)
            else:
                self.fail()

        loops = xml_tree.find(".//LoopController/stringProp[@name='LoopController.loops']")
        self.assertEqual(loops.text, "-1")

    def test_loop_invalid(self):
        self.configure(scenario={
            "requests": [{
                "loop": 100
            }]
        })
        self.assertRaises(TaurusConfigError, lambda: self.obj.save(self.jmx))

    def test_while_controller(self):
        self.configure(scenario={
            "requests": [{
                "while": "<cond>",
                "do": [
                    "http://blazedemo.com/"
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        controller = xml_tree.find(".//WhileController")
        self.assertIsNotNone(controller)
        condition = xml_tree.find(".//WhileController/stringProp[@name='WhileController.condition']")
        self.assertIsNotNone(condition)
        self.assertEqual(condition.text, "<cond>")

    def test_while_invalid(self):
        self.configure(scenario={
            "requests": [{
                "while": "<cond>"
            }]
        })
        self.assertRaises(TaurusConfigError, lambda: self.obj.save(self.jmx))

    def test_foreach_controller(self):
        self.configure(scenario={
            "requests": [{
                "foreach": "name in usernames",
                "do": [
                    "http://site.com/users/${name}"
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        self.assertIsNotNone(xml_tree.find(".//ForeachController"))
        input = xml_tree.find(".//ForeachController/stringProp[@name='ForeachController.inputVal']")
        self.assertEqual(input.text, "usernames")
        loop_var = xml_tree.find(".//ForeachController/stringProp[@name='ForeachController.returnVal']")
        self.assertEqual(loop_var.text, "name")

    def test_transaction_controller(self):
        self.configure(scenario={
            "requests": [{
                "transaction": "API",
                "do": [
                    "http://blazedemo.com/",
                    "http://blazedemo.com/reserve.php"
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        controller = xml_tree.find(".//TransactionController")
        self.assertIsNotNone(controller)
        self.assertEqual(controller.get('testname'), "API")

    def test_include_scenario(self):
        self.configure(scenario={
            "requests": [{
                "include-scenario": "login"
            }]
        })
        self.obj.executor.engine.config.merge({'scenarios': {
            'login': {
                'requests': [
                    'http://blazedemo.com/login'
                ]
            }
        }})
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        controller = xml_tree.find(".//GenericController")
        self.assertIsNotNone(controller)
        self.assertEqual(controller.get('testname'), "login")
        ht = controller.getnext()
        sampler = ht.find('HTTPSamplerProxy')
        self.assertIsNotNone(sampler)
        domain = sampler.find('stringProp[@name="HTTPSampler.domain"]')
        self.assertEqual(domain.text, "blazedemo.com")
        path = sampler.find('stringProp[@name="HTTPSampler.path"]')
        self.assertEqual(path.text, "/login")

    def test_action_target(self):
        self.configure(scenario={
            "requests": [{
                "action": "stop",
                "target": "all-threads",
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        block = xml_tree.find(".//TestAction")
        self.assertIsNotNone(block)
        action = block.find('intProp[@name="ActionProcessor.action"]')
        self.assertEqual(action.text, "0")
        target = block.find('intProp[@name="ActionProcessor.target"]')
        self.assertEqual(target.text, "2")

    def test_action_unknown(self):
        self.configure(scenario={
            "requests": [{
                "action": "unknown",
            }]
        })
        self.assertRaises(TaurusConfigError, lambda: self.obj.save(self.jmx))

    def test_set_vars(self):
        self.configure(scenario={
            "requests": [{
                "set-variables": {"foo": "bar"}
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        self.assertIsNotNone(xml_tree.find(".//JSR223PreProcessor"))
        input = xml_tree.find(".//JSR223PreProcessor/stringProp[@name='script']")
        self.assertEqual(input.text, "vars.put('foo', 'bar');")

    def test_regexp_extractors(self):
        self.configure(scenario={
            "requests": [{
                "url": "http://localhost",
                "extract-regexp": {
                    "test_name": "???"
                }
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        self.assertEqual("body", xml_tree.findall(".//stringProp[@name='RegexExtractor.useHeaders']")[0].text)
        self.assertEqual("???", xml_tree.findall(".//stringProp[@name='RegexExtractor.regex']")[0].text)
        self.assertEqual("parent", xml_tree.findall(".//stringProp[@name='Sample.scope']")[0].text)

    def test_boundary_extractors(self):
        self.configure(scenario={
            "requests": [{
                "url": "http://localhost",
                "extract-boundary": {
                    "varname": {"left": "foo", "right": "bar"}
                }
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        self.assertEqual("false", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.useHeaders']")[0].text)
        self.assertEqual("foo", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.lboundary']")[0].text)
        self.assertEqual("bar", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.rboundary']")[0].text)
        self.assertEqual("varname", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.refname']")[0].text)

    def test_boundary_extractors_exc(self):
        self.configure(scenario={
            "requests": [{
                "url": "http://localhost",
                "extract-boundary": {
                    "varname": {"left": "foo"}
                }
            }]
        })  # no "right"
        self.assertRaises(TaurusConfigError, lambda: self.obj.save(self.jmx))

    def test_css_jquery_extractor(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read())["scenarios"]["get-post"])
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        jq_css_extractors = xml_tree.findall(".//HtmlExtractor")
        self.assertEqual(2, len(jq_css_extractors))
        simplified_extractor = xml_tree.find(".//HtmlExtractor[@testname='Get name1']")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.refname']").text, "name1")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.expr']").text,
                         "input[name~=my_input]")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.attribute']").text, None)
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.match_number']").text, "0")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.default']").text, "NOT_FOUND")
        full_form_extractor = xml_tree.find(".//HtmlExtractor[@testname='Get name2']")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.refname']").text, "name2")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.expr']").text,
                         "input[name=JMeter]")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.attribute']").text, "value")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.match_number']").text, "1")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.default']").text, "NV_JMETER")

    def test_xpath_extractor(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read())["scenarios"]["get-post"])
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        xpath_extractors = xml_tree.findall(".//XPathExtractor")
        self.assertEqual(2, len(xpath_extractors))

        simplified = xml_tree.find(".//XPathExtractor[@testname='Get xpath1']")
        self.assertEqual(simplified.find(".//stringProp[@name='XPathExtractor.refname']").text, "xpath1")
        self.assertEqual(simplified.find(".//stringProp[@name='XPathExtractor.xpathQuery']").text,
                         "/html/head/title")
        self.assertEqual(simplified.find(".//stringProp[@name='XPathExtractor.default']").text, "NOT_FOUND")
        self.assertEqual(simplified.find(".//boolProp[@name='XPathExtractor.validate']").text, "false")
        self.assertEqual(simplified.find(".//boolProp[@name='XPathExtractor.whitespace']").text, "true")
        self.assertEqual(simplified.find(".//boolProp[@name='XPathExtractor.tolerant']").text, "false")

        full_form = xml_tree.find(".//XPathExtractor[@testname='Get xpath2']")
        self.assertEqual(full_form.find(".//stringProp[@name='XPathExtractor.refname']").text, "xpath2")
        self.assertEqual(full_form.find(".//stringProp[@name='XPathExtractor.xpathQuery']").text,
                         "/html/head/base")
        self.assertEqual(full_form.find(".//stringProp[@name='XPathExtractor.default']").text, "<no base>")
        self.assertEqual(full_form.find(".//boolProp[@name='XPathExtractor.validate']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPathExtractor.whitespace']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPathExtractor.tolerant']").text, "true")

    def test_xpath_assertion(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read())["scenarios"]["get-post"])
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        assertions = xml_tree.findall(".//XPathAssertion")
        self.assertEqual(2, len(assertions))

        simplified = assertions[0]
        self.assertEqual(simplified.find(".//stringProp[@name='XPath.xpath']").text, "/note/to")
        self.assertEqual(simplified.find(".//boolProp[@name='XPath.validate']").text, "false")
        self.assertEqual(simplified.find(".//boolProp[@name='XPath.whitespace']").text, "true")
        self.assertEqual(simplified.find(".//boolProp[@name='XPath.tolerant']").text, "false")
        self.assertEqual(simplified.find(".//boolProp[@name='XPath.negate']").text, "false")

        full_form = assertions[1]
        self.assertEqual(full_form.find(".//stringProp[@name='XPath.xpath']").text, "/note/from")
        self.assertEqual(full_form.find(".//boolProp[@name='XPath.validate']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPath.whitespace']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPath.tolerant']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPath.negate']").text, "true")

    def test_jsonpath_assertion(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read())["scenarios"]["get-post"])
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        path = ".//com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion.JSONPathAssertion"
        assertions = xml_tree.findall(path)
        self.assertEqual(4, len(assertions))

        vals = [
            {'path': '$.', 'exp_val': None, 'valid': 'false',
             'null': 'false', 'invert': 'false', 'regexp': 'true'},
            {'path': '$.res[0].type', 'exp_val': 'some_value.1', 'valid': 'true',
             'null': 'false', 'invert': 'false', 'regexp': 'true'},
            {'path': '$.res[1].ip', 'exp_val': 'some_value.2', 'valid': 'true',
             'null': 'false', 'invert': 'true', 'regexp': 'false'},
            {'path': '$.res[2].default', 'exp_val': None, 'valid': 'false',
             'null': 'true', 'invert': 'false', 'regexp': 'true'}]
        for num in range(len(assertions)):
            assertion = assertions[num]
            val = vals[num]
            self.assertEqual(val['path'], assertion.find(".//stringProp[@name='JSON_PATH']").text)
            self.assertEqual(val['exp_val'], assertion.find(".//stringProp[@name='EXPECTED_VALUE']").text)
            self.assertEqual(val['valid'], assertion.find(".//boolProp[@name='JSONVALIDATION']").text)
            self.assertEqual(val['null'], assertion.find(".//boolProp[@name='EXPECT_NULL']").text)
            self.assertEqual(val['invert'], assertion.find(".//boolProp[@name='INVERT']").text)
            self.assertEqual(val['regexp'], assertion.find(".//boolProp[@name='ISREGEX']").text)

    def test_transaction_and_requests1(self):
        self.configure(scenario={
            'force-parent-sample': False,
            'requests': [{
                'transaction': 'MY_TRANSACTION',
                'do': [{
                    'url': 'http://blazedemo.com'
                }]
            }]
        })
        self.obj.save(self.jmx)
        jmx = JMX(self.jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 1)
        non_parent = props[0]
        self.assertEqual(non_parent.text, 'false')

    def test_transaction_and_requests2(self):
        self.configure(scenario={
            'requests': [{
                'transaction': 'MY_TRANSACTION',
                'force-parent-sample': False,
                'do': [{
                    'url': 'http://blazedemo.com'
                }]
            }]
        })
        self.obj.save(self.jmx)
        jmx = JMX(self.jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 1)
        non_parent = props[0]
        self.assertEqual(non_parent.text, 'false')

    def test_request_logic_nested_if(self):
        self.configure(scenario={
            "requests": [{
                "if": "<cond1>",
                "then": [
                    "http://blazedemo.com/", {
                        "if": "<cond2>",
                        "then": [
                            "http://demo.blazemeter.com/"
                        ]
                    }
                ]
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        ifs = xml_tree.findall(".//IfController")
        self.assertEqual(2, len(ifs))
        conditions = xml_tree.findall(".//IfController/stringProp[@name='IfController.condition']")
        self.assertEqual(2, len(conditions))
        self.assertEqual(conditions[0].text, "<cond1>")
        self.assertEqual(conditions[1].text, "<cond2>")

    def test_logic_test_action(self):
        self.configure(scenario={
            "requests": [{
                "action": "pause",
                "pause-duration": "1s",
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        block = xml_tree.find(".//TestAction")
        self.assertIsNotNone(block)
        action = block.find('intProp[@name="ActionProcessor.action"]')
        self.assertEqual(action.text, "1")
        target = block.find('intProp[@name="ActionProcessor.target"]')
        self.assertEqual(target.text, "0")
        target = block.find('stringProp[@name="ActionProcessor.duration"]')
        self.assertEqual(target.text, "1000")

    def test_jsr223_block(self):
        script = RESOURCES_DIR + "/jmeter/jsr223_script.js"
        self.configure(scenario={
            "requests": [{
                "url": "http://blazedemo.com/",
                "jsr223": {
                    "language": "javascript",
                    "script-file": script,
                    "parameters": "first second"
                }
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        post_procs = xml_tree.findall(".//JSR223PostProcessor[@testclass='JSR223PostProcessor']")
        self.assertEqual(1, len(post_procs))

        jsr = post_procs[0]
        self.assertEqual(script, jsr.find(".//stringProp[@name='filename']").text)
        self.assertEqual("javascript", jsr.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual("first second", jsr.find(".//stringProp[@name='parameters']").text)

    def test_jsr223_exceptions_2(self):
        self.configure(scenario={
            "requests": [{
                "url": "http://blazedemo.com/",
                "jsr223": {
                    "language": "javascript"
                }
            }]
        })
        self.assertRaises(TaurusConfigError, lambda: self.obj.save(self.jmx))

    def test_jsr223_multiple(self):
        pre_script = RESOURCES_DIR + "/jmeter/jsr223_script.js"
        post_script = RESOURCES_DIR + "/jmeter/bean_script.bhs"
        self.configure(scenario={
            "requests": [{
                "url": "http://blazedemo.com/",
                "jsr223": [{
                    "language": "javascript",
                    "script-file": pre_script,
                    "execute": "before",
                }, {
                    "language": "beanshell",
                    "script-file": post_script,
                    "execute": "after",
                },
                    'vars.put("a", 1)']
            }]
        })
        self.obj.save(self.jmx)
        with open(self.jmx, "rb") as fds:
            xml_tree = etree.fromstring(fds.read())
        pre_procs = xml_tree.findall(".//JSR223PreProcessor[@testclass='JSR223PreProcessor']")
        post_procs = xml_tree.findall(".//JSR223PostProcessor[@testclass='JSR223PostProcessor']")
        self.assertEqual(1, len(pre_procs))
        self.assertEqual(2, len(post_procs))

        pre = pre_procs[0]
        self.assertEqual(pre_script, pre.find(".//stringProp[@name='filename']").text)
        self.assertEqual("javascript", pre.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual(None, pre.find(".//stringProp[@name='parameters']").text)

        pre = post_procs[0]
        self.assertEqual(post_script, pre.find(".//stringProp[@name='filename']").text)
        self.assertEqual("beanshell", pre.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual(None, pre.find(".//stringProp[@name='parameters']").text)

        pre = post_procs[1]
        self.assertEqual(None, pre.find(".//stringProp[@name='filename']").text)
        self.assertEqual("groovy", pre.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual(None, pre.find(".//stringProp[@name='parameters']").text)
        self.assertEqual('vars.put("a", 1)', pre.find(".//stringProp[@name='script']").text)
