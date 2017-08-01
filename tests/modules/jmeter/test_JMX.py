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

    def test_keep_original(self):
        self.configure(jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)     # because no duration
        self.sniff_log(self.obj.log)
        self.obj.modify(self.jmx)
        msg = "No iterations/concurrency/duration found, thread group modification is skipped"
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())
        groupset = self.get_groupset()
        groups = [group.gtype for group in groupset]
        self.assertEqual(4, len(set(groups)))   # no one group was modified

    def test_TG_cs(self):
        """ ThreadGroup: concurrency, steps """
        self.configure(load={'concurrency': 70, 'steps': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)     # because no duration
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = 'Getting of concurrency for UltimateThreadGroup not implemented'
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        msg = "Stepping ramp-up isn't supported for regular ThreadGroup"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual('ThreadGroup', group.gtype)
            self.assertEqual("false", group.element.find(".//*[@name='LoopController.continue_forever']").text)
            self.assertEqual("-1", group.element.find(".//*[@name='LoopController.loops']").text)   # no loop limit
            res_values[group.testname()] = {'conc': group.concurrency(), 'on_error': group.on_error()}

        self.assertEqual(res_values,
                         {'TG.01': {'conc': 14, 'on_error': 'startnextloop'},
                          'CTG.02': {'conc': 21, 'on_error': 'stopthread'},
                          'STG.03': {'conc': 28, 'on_error': 'stoptest'},
                          'UTG.04': {'conc': 7, 'on_error': 'stoptestnow'}})

    def test_CTG_crs(self):
        """ ConcurrencyThreadGroup: concurrency, ramp-up, steps """
        self.configure(load={'concurrency': 70, 'ramp-up': 100, 'steps': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = 'Getting of concurrency for UltimateThreadGroup not implemented'
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("5", group.element.find(".//*[@name='Steps']").text)
            self.assertEqual("100", group.element.find(".//*[@name='RampUp']").text)
            self.assertEqual("S", group.element.find(".//*[@name='Unit']").text)
            self.assertIn(group.element.find(".//*[@name='Hold']").text, ("", "0"))

            res_values[group.testname()] = {'conc': group.concurrency(), 'on_error': group.on_error()}

        self.assertEqual(res_values,
                         {'TG.01': {'conc': 14, 'on_error': 'startnextloop'},
                          'CTG.02': {'conc': 21, 'on_error': 'stopthread'},
                          'STG.03': {'conc': 28, 'on_error': 'stoptest'},
                          'UTG.04': {'conc': 7, 'on_error': 'stoptestnow'}})

    def test_CTG_h(self):
        """ ConcurrencyThreadGroup: hold-for """
        self.configure(load={'hold-for': 70.5}, jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual("70", group.element.find(".//*[@name='Hold']").text)

            res_values[group.testname()] = group.concurrency()

        self.assertEqual(res_values, {'TG.01': 2, 'CTG.02': 3, 'STG.03': 4, 'UTG.04': 1})

    def test_TG_ci(self):
        """ ThreadGroup: concurrency, iterations """
        self.configure(load={'concurrency': 1, 'iterations': 7},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        for group in self.get_groupset():
            self.assertEqual(1, group.concurrency())
            self.assertEqual("false", group.element.find(".//*[@name='ThreadGroup.scheduler']").text)
            self.assertEqual("7", group.element.find(".//*[@name='LoopController.loops']").text)

    def test_TG_hr(self):
        """ ThreadGroup: hold-for, ramp-up, no plugin """
        self.configure(load={'ramp-up': 10, 'hold-for': 20},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx',
                       has_ctg=False)
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual("true", group.element.find(".//*[@name='ThreadGroup.scheduler']").text)
            self.assertEqual("true", group.element.find(".//*[@name='ThreadGroup.scheduler']").text)
            self.assertEqual(str(10 + 20), group.element.find(".//*[@name='ThreadGroup.duration']").text)
            self.assertEqual("-1", group.element.find(".//*[@name='LoopController.loops']").text)

            res_values[group.testname()] = group.concurrency()

        self.assertEqual(res_values, {'TG.01': 2, 'CTG.02': 3, 'STG.03': 4, 'UTG.04': 1})


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
