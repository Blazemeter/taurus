# coding=utf-8
import logging

from bzt.engine import Engine, ScenarioExecutor, Provisioning
from bzt.jmx import JMX, LoadSettingsProcessor
from tests import BZTestCase, RESOURCES_DIR


class MockReqTool(object):
    def __init__(self, has_ctg):
        self.has_ctg = has_ctg

    def ctg_plugin_installed(self):
        return self.has_ctg


class MockExecutor(ScenarioExecutor):
    def __init__(self, load=None, settings=None, has_ctg=None):
        super(MockExecutor, self).__init__()
        if load is None: load = {}
        if settings is None: settings = {}
        if has_ctg is None: has_ctg = True

        self.engine = Engine(logging.getLogger(''))
        self.execution = load
        self.settings = settings
        self.tool = MockReqTool(has_ctg)


class TestLoadSettingsProcessor(BZTestCase):
    def configure(self, jmx_file=None, load=None, settings=None, has_ctg=None):
        executor = MockExecutor(load, settings, has_ctg)
        executor.engine.config.merge({Provisioning.PROV: 'local'})
        self.obj = LoadSettingsProcessor(executor)
        if jmx_file:
            self.jmx = JMX(jmx_file)

    def get_groupset(self, testname=None):
        groupset = []
        for group in self.obj.tg_handler.groups(self.jmx):
            # 'testname == None' means 'get all groups'
            if not testname or (testname and group.element.attrib['testname'] == testname):
                groupset.append(group)
        return groupset

    def test_keep_original_TG(self):
        self.configure(jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)     # because no duration
        self.sniff_log(self.obj.log)
        self.obj.modify(self.jmx)
        msg = "No iterations/concurrency/duration found, thread group modification is skipped"
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())
        groupset = self.get_groupset()
        groups = [group.gtype for group in groupset]
        self.assertEqual(4, len(set(groups)))   # no one group was modified

    def test_concurrency_TG_cs(self):
        self.configure(load={'concurrency': 70, 'steps': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)     # because no duration
        self.sniff_log(self.obj.log)
        self.obj.modify(self.jmx)
        msg = "Stepping ramp-up isn't supported for regular ThreadGroup"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        result = {}
        for group in self.get_groupset():
            self.assertEqual('ThreadGroup', group.gtype)
            result[group.testname()] = group.concurrency()

        self.assertEqual(result, {'TG.01': 10, 'CTG.02': 15, 'STG.03': 20, 'UTG.04': 25})

    def test_concurrency_CTG_rs(self):
        self.configure(load={'concurrency': 70, 'ramp-up': 100, 'steps': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)
        self.sniff_log(self.obj.log)
        self.obj.modify(self.jmx)

        conc_vals = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("5", group.element.find(".//*[@name='Steps']").text)
            self.assertEqual("100", group.element.find(".//*[@name='RampUp']").text)
            self.assertIn(group.element.find(".//*[@name='Hold']").text, ("", "0"))
            conc_vals[group.testname()] = group.concurrency()

        self.assertEqual(conc_vals, {'TG.01': 10, 'CTG.02': 15, 'STG.03': 20, 'UTG.04': 25})


class TestJMX(BZTestCase):
    def test_jmx_unicode_checkmark(self):
        obj = JMX()
        res = obj._get_http_request("url", "label", "method", 0, {"param": u"✓"}, True)
        prop = res.find(".//stringProp[@name='Argument.value']")
        self.assertNotEqual("BINARY", prop.text)
        self.assertEqual(u"✓", prop.text)

    def test_variable_hostname(self):
        obj = JMX()
        res = obj._get_http_request("http://${hostName}:${Port}/${Path}", "label", "method", 0, {}, True)
        self.assertEqual("/${Path}", res.find(".//stringProp[@name='HTTPSampler.path']").text)
        self.assertEqual("${hostName}", res.find(".//stringProp[@name='HTTPSampler.domain']").text)
        self.assertEqual("${Port}", res.find(".//stringProp[@name='HTTPSampler.port']").text)

    def test_no_port(self):
        obj = JMX()
        res = obj._get_http_request("http://hostname", "label", "method", 0, {}, True)
        self.assertEqual("", res.find(".//stringProp[@name='HTTPSampler.path']").text)
        self.assertEqual("hostname", res.find(".//stringProp[@name='HTTPSampler.domain']").text)
        self.assertEqual("", res.find(".//stringProp[@name='HTTPSampler.port']").text)

    def test_regexp_subject(self):
        res = JMX._get_extractor('test_name', 'baddy', 'regexp', 1, 1, 'error')
        self.assertEqual("body", res.find(".//stringProp[@name='RegexExtractor.useHeaders']").text)
        res = JMX._get_extractor('test_name', 'headers', 'regexp', 1, 1, 'error')
        self.assertEqual("true", res.find(".//stringProp[@name='RegexExtractor.useHeaders']").text)
        res = JMX._get_extractor('test_name', 'http-code', 'regexp', 1, 1, 'error')
        self.assertEqual("code", res.find(".//stringProp[@name='RegexExtractor.useHeaders']").text)
        self.assertEqual("parent", res.find(".//stringProp[@name='Sample.scope']").text)

    def test_int_udv(self):
        res = JMX()
        data = {"varname2": "1", "varname": 1, 2: 3}
        res.add_user_def_vars_elements(data)

    def test_source_ips_single(self):
        obj = JMX()
        res = obj._get_http_request("/", "label", "method", 0, {}, True,
                                    use_random_host_ip=True, host_ips=["192.168.1.1"])
        self.assertEqual("192.168.1.1", res.find(".//stringProp[@name='HTTPSampler.ipSource']").text)

    def test_source_ips_multiple(self):
        obj = JMX()
        res = obj._get_http_request("/", "label", "method", 0, {}, True,
                                    use_random_host_ip=True, host_ips=["192.168.1.1", "192.168.1.2"])
        self.assertEqual("${__chooseRandom(192.168.1.1,192.168.1.2,randomAddr)}",
                         res.find(".//stringProp[@name='HTTPSampler.ipSource']").text)
