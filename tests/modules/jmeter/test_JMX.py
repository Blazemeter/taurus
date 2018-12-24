# coding=utf-8
from . import MockJMeterExecutor
from bzt.engine import Provisioning
from bzt.jmx import JMX, LoadSettingsProcessor
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul


def get(group, name, **kwargs):
    if "raw" not in kwargs:
        kwargs["raw"] = True
    return group.get(name, **kwargs)


class TestLoadSettingsProcessor(BZTestCase):
    def configure(self, jmx_file=None, load=None, settings=None, has_ctg=None):
        executor = MockJMeterExecutor(settings, has_ctg)
        executor.engine = EngineEmul()
        executor.configure(load=load)
        executor.engine.config.merge({Provisioning.PROV: 'local'})
        executor.install_required_tools()
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
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because no duration
        self.sniff_log(self.obj.log)
        self.obj.modify(self.jmx)
        msg = "No iterations/concurrency/duration found, thread group modification is skipped"
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())
        groupset = self.get_groupset()
        groups = [group.gtype for group in groupset]
        self.assertEqual(5, len(set(groups)))  # no one group was modified
        self.assertEqual("", self.log_recorder.warn_buff.getvalue())
        res_values = {}
        for group in groupset:
            res_values[group.test_name] = {
                'conc': group.get("concurrency"),
                'rate': group.get("rate"),
                'duration': int(group.get_seconds("duration")),
                'iterations': group.get("iterations")}

        self.assertEqual(res_values,
                         {'TG.01': {'conc': 2, 'duration': 3, 'iterations': 100, 'rate': 1},
                          'CTG.02': {'conc': 3, 'duration': 100, 'iterations': 0, 'rate': 1},
                          'STG.03': {'conc': 4, 'duration': 0, 'iterations': 0, 'rate': 1},
                          'UTG.04': {'conc': 1, 'duration': 0, 'iterations': 0, 'rate': 1},
                          'ATG.05': {'conc': 1, 'duration': 480, 'iterations': 33, 'rate': 2}})

    def test_TG_cs(self):
        """ ThreadGroup: concurrency, steps """
        self.configure(load={'concurrency': 76, 'steps': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because no duration
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = 'Getting of concurrency for UltimateThreadGroup not implemented'
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        msg = "Had to add 1 more threads to maintain thread group proportion"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        msg = "Stepping ramp-up isn't supported for regular ThreadGroup"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual('ThreadGroup', group.gtype)
            self.assertEqual("false", group.element.find(".//*[@name='LoopController.continue_forever']").text)

            res_values[group.test_name] = {
                "conc": group.get("concurrency"),
                "on_error": group.get("on-error"),
                "iterations": group.get("iterations")}

        self.assertEqual(res_values,
                         {"TG.01": {"conc": 14, "on_error": "startnextloop", "iterations": 100},
                          "CTG.02": {"conc": 21, "on_error": "stopthread", "iterations": -1},
                          "STG.03": {"conc": 28, "on_error": "stoptest", "iterations": -1},
                          "UTG.04": {"conc": 7, "on_error": "stoptestnow", "iterations": -1},
                          "ATG.05": {"conc": 7, "on_error": "continue", "iterations": 33}})

    def test_CTG_crs(self):
        """ ConcurrencyThreadGroup: concurrency, ramp-up, steps """
        self.configure(load={'concurrency': 71, 'ramp-up': 103, 'steps': 5, "throughput": 52},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = 'Getting of concurrency for UltimateThreadGroup not implemented'
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        msg = "1 threads left undistributed due to thread group proportion"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("5", get(group, "steps"))
            self.assertEqual("103", get(group, "ramp-up"))

            res_values[group.test_name] = {
                "conc": group.get("concurrency"),
                "on_error": get(group, "on-error"),
                "unit": get(group, "unit"),
                "hold": get(group, "hold")}

        self.assertEqual(res_values,
                         {"TG.01": {"conc": 13, "on_error": "startnextloop", "unit": "S", "hold": "3"},
                          "CTG.02": {"conc": 19, "on_error": "stopthread", "unit": "S", "hold": "0"},
                          "STG.03": {"conc": 26, "on_error": "stoptest", "unit": "S", "hold": "60"},
                          "UTG.04": {"conc": 6, "on_error": "stoptestnow", "unit": "S", "hold": "0"},
                          "ATG.05": {"conc": 6, "on_error": "continue", "unit": "M", "hold": "5"}})

        self.assertListEqual(self._get_tst_schedule(),
                             [['10.4', '10.4', '20'],
                              ['20.8', '20.8', '21'],
                              ['31.2', '31.2', '20'],
                              ['41.6', '41.6', '21'],
                              ['52.0', '52.0', '21']])

    def test_CTG_prop_rs(self):
        """ ConcurrencyThreadGroup: properties in ramp-up, steps """
        self.configure(load={'ramp-up': '${__P(r)}', 'steps': '${__P(s)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("${__P(s)}", get(group, "steps"))
            self.assertEqual("${__P(r)}", get(group, "ramp-up"))

            res_values[group.test_name] = {
                "conc": get(group, "concurrency"), "hold": get(group, "hold"), "unit": get(group, "unit")}

        self.assertEqual(res_values,
                         {"TG.01": {"conc": "2", "hold": "3", "unit": "S"},
                          "CTG.02": {"conc": "3", "hold": "0", "unit": "S"},
                          "STG.03": {"conc": "4", "hold": "60", "unit": "S"},
                          "UTG.04": {"conc": "1", "hold": "0", "unit": "S"},
                          "ATG.05": {"conc": "1", "hold": "5", "unit": "M"}})

    def test_CTG_prop_trh(self):
        """ ConcurrencyThreadGroup: properties in throughput, ramp-up, hold-for """
        self.configure(load={'ramp-up': '${__P(r)}', 'throughput': '${__P(t)}', 'hold-for': '${__P(h)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        self.assertListEqual(self._get_tst_schedule(),
                             [["1.0", "${__P(t)}", "${__P(r)}"], ["${__P(t)}", "${__P(t)}", "${__P(h)}"]], )

    def _get_tst_schedule(self):
        records = []
        shaper_elements = self.jmx.get("kg\.apc\.jmeter\.timers\.VariableThroughputTimer")
        self.assertEqual(1, len(shaper_elements))

        shaper_collection = shaper_elements[0].find(".//collectionProp[@name='load_profile']")
        coll_elements = shaper_collection.findall(".//collectionProp")

        for expected in coll_elements:
            item = []
            strings0 = expected.findall(".//stringProp")
            for rec in strings0:
                item.append(rec.text)
            records.append(item)
        return records

    def test_TG_prop_cih(self):
        """ ThreadGroup: properties in concurrency, hold-for, iterations """
        self.configure(load={'concurrency': '${__P(c)}', 'hold-for': '${__P(h)}', 'iterations': '${__P(i)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("${__P(c)}", group.element.find(".//*[@name='ThreadGroup.num_threads']").text)
            self.assertEqual("${__P(i)}", group.element.find(".//*[@name='LoopController.loops']").text)

            res_values[group.test_name] = group.get("duration", raw=True)

        self.assertEqual(res_values, {
            "TG.01": "${__P(h)}",
            "CTG.02": "${__intSum(100,${__P(h)})}",     # has ramp-up
            "STG.03": "${__P(h)}",
            "UTG.04": "${__P(h)}",
            "ATG.05": "${__intSum(180,${__P(h)})}"})    # has ramp-up

    def test_TG_prop_rh(self):
        """ ThreadGroup: properties in ramp-up, hold-for """
        self.configure(load={'ramp-up': '${__P(r)}', 'hold-for': '${__P(h)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx', has_ctg=False)
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("${__P(r)}", group.element.find(".//*[@name='ThreadGroup.ramp_time']").text)
            self.assertEqual("${__intSum(${__P(r)},${__P(h)})}",
                             group.element.find(".//*[@name='ThreadGroup.duration']").text)
            # self.assertEqual("-1", group.element.find(".//*[@name='LoopController.loops']").text)
            res_values[group.test_name] = group.get("iterations")

        self.assertEqual(res_values, {"TG.01": 100, "CTG.02": -1, "STG.03": -1, "UTG.04": -1, "ATG.05": 33})

    def test_CTG_h(self):
        """ ConcurrencyThreadGroup: hold-for """
        self.configure(load={'hold-for': 70.5}, jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual("70", group.element.find(".//*[@name='Hold']").text)

            res_values[group.test_name] = group.get("concurrency")

        self.assertEqual(res_values, {'TG.01': 2, 'CTG.02': 3, 'STG.03': 4, 'UTG.04': 1, 'ATG.05': 1})

    def test_TG_ci(self):
        """ ThreadGroup: concurrency, iterations """
        self.configure(load={'concurrency': 1, 'iterations': 7},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(1, group.get("concurrency"))
            self.assertEqual("7", group.element.find(".//*[@name='LoopController.loops']").text)

            res_values[group.test_name] = group.element.find(".//*[@name='ThreadGroup.scheduler']").text

        self.assertEqual(res_values,
                         {"TG.01": "true", "CTG.02": "false", "STG.03": "true", "UTG.04": "false", "ATG.05": "true"})

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
            self.assertEqual(str(10 + 20), group.element.find(".//*[@name='ThreadGroup.duration']").text)

            res_values[group.test_name] = {"conc": group.get("concurrency"), "iterations": group.get("iterations")}

        self.assertEqual(res_values, {
            "TG.01": {"conc": 2, "iterations": 100},
            "CTG.02": {"conc": 3, "iterations": -1},
            "STG.03": {"conc": 4, "iterations": -1},
            "UTG.04": {"conc": 1, "iterations": -1},
            "ATG.05": {"conc": 1, "iterations": 33}})


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
        self.assertIsNone(res.find(".//stringProp[@name='Sample.scope']"))

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
