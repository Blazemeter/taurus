# coding=utf-8
import io

from . import MockJMeterExecutor
from bzt.engine import Provisioning
from bzt.utils import BetterDict
from bzt.jmx import JMX, LoadSettingsProcessor
from tests.unit import BZTestCase, RESOURCES_DIR, EngineEmul
from bzt.jmx.tools import JMeterScenarioBuilder
from bzt.jmx.mqtt import MQTTProtocolHandler
from bzt.requests_model import MQTTRequest


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

    def test_empty_concuurency(self):
        self.configure(load={"concurrency": 22}, jmx_file=RESOURCES_DIR + 'jmeter/jmx/empty_concurrency.jmx')
        self.obj.modify(self.jmx)
        self.assertEqual("22", self.get_groupset()[0].get_concurrency(raw=True))

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
            res_values[group.get_testname()] = {
                'conc': group.get_concurrency(),
                'rate': group.get_rate(),
                'duration': group.get_duration(),
                'iterations': group.get_iterations()}

        self.assertEqual(res_values,
                         {'TG.01': {'conc': 2, 'duration': 3, 'iterations': 100, 'rate': 1},
                          'CTG.02': {'conc': 3, 'duration': 100, 'iterations': 10, 'rate': 1},
                          'STG.03': {'conc': 4, 'duration': None, 'iterations': None, 'rate': 1},
                          'UTG.04': {'conc': 1, 'duration': None, 'iterations': None, 'rate': 1},
                          'ATG.05': {'conc': 1, 'duration': 480, 'iterations': 33, 'rate': 2}})

    def test_TG_cs(self):
        """ ThreadGroup: concurrency, steps """
        self.configure(load={'concurrency': 76, 'steps': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because no duration
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = 'UltimateThreadGroup: getting of concurrency is impossible (not implemented)'
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())

        msg = "Had to add 1 more threads to maintain thread group proportion"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        msg = "Stepping ramp-up isn't supported for regular ThreadGroup"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual('ThreadGroup', group.gtype)
            self.assertEqual("false", group.element.find(".//*[@name='LoopController.continue_forever']").text)
            self.assertEqual("1", group.element.find(".//*[@name='LoopController.loops']").text)  # no loop limit

            res_values[group.get_testname()] = {
                'conc': group.get_concurrency(),
                'on_error': group.get_on_error(),
                'delay': group.get_scheduler_delay()}

        self.assertEqual(res_values,
                         {'TG.01': {'conc': 14, 'on_error': 'startnextloop', 'delay': '33'},
                          'CTG.02': {'conc': 21, 'on_error': 'stopthread', 'delay': None},
                          'STG.03': {'conc': 28, 'on_error': 'stoptest', 'delay': None},
                          'UTG.04': {'conc': 7, 'on_error': 'stoptestnow', 'delay': None},
                          'ATG.05': {'conc': 7, 'on_error': 'continue', 'delay': None}})

    def test_CTG_crs(self):
        """ ConcurrencyThreadGroup: concurrency, ramp-up, steps """
        self.configure(load={'concurrency': 71, 'ramp-up': 103, 'steps': 5, "throughput": 52},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = 'UltimateThreadGroup: getting of concurrency is impossible (not implemented)'
        self.assertIn(msg, self.log_recorder.debug_buff.getvalue())

        msg = "1 threads left undistributed due to thread group proportion"
        self.assertIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("5", group.element.find(".//*[@name='Steps']").text)
            self.assertEqual("103", group.element.find(".//*[@name='RampUp']").text)
            self.assertEqual("S", group.element.find(".//*[@name='Unit']").text)
            self.assertIn(group.element.find(".//*[@name='Hold']").text, ("", "0"))

            res_values[group.get_testname()] = {'conc': group.get_concurrency(), 'on_error': group.get_on_error()}

        self.assertEqual(res_values,
                         {'TG.01': {'conc': 13, 'on_error': 'startnextloop'},
                          'CTG.02': {'conc': 19, 'on_error': 'stopthread'},
                          'STG.03': {'conc': 26, 'on_error': 'stoptest'},
                          'UTG.04': {'conc': 6, 'on_error': 'stoptestnow'},
                          'ATG.05': {'conc': 6, 'on_error': 'continue'}})

        self.assertListEqual(self._get_tst_schedule(),
                             [['10.4', '10.4', '20'],
                              ['20.8', '20.8', '21'],
                              ['31.2', '31.2', '20'],
                              ['41.6', '41.6', '21'],
                              ['52.0', '52.0', '21']])

    def test_CTG_null_iterations(self):
        """ ConcurrencyThreadGroup: concurrency, ramp-up, steps """
        self.configure(load={'hold-for': 103},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/null-iterations.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)
        self.sniff_log(self.obj.log)

        self.obj.modify(self.jmx)

        msg = "Parsing iterations 'None' in group 'ConcurrencyThreadGroup' failed"
        self.assertNotIn(msg, self.log_recorder.warn_buff.getvalue())

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("", group.element.find(".//*[@name='Iterations']").text)
            self.assertIn(group.element.find(".//*[@name='Hold']").text, ("103",))

            res_values[group.get_testname()] = {'conc': group.get_concurrency(), 'on_error': group.get_on_error()}

        self.assertEqual({'CTG.02': {'conc': 3, 'on_error': 'stopthread'}}, res_values, )

    def test_CTG_prop_rs(self):
        """ ConcurrencyThreadGroup: properties in ramp-up, steps """
        self.configure(load={'ramp-up': '${__P(r)}', 'steps': '${__P(s)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ConcurrencyThreadGroup")
            self.assertEqual("${__P(s)}", group.element.find(".//*[@name='Steps']").text)
            self.assertEqual("${__P(r)}", group.element.find(".//*[@name='RampUp']").text)
            self.assertIn(group.element.find(".//*[@name='Hold']").text, ("", "0"))

            res_values[group.get_testname()] = group.get_concurrency()

        self.assertEqual(res_values, {'TG.01': 2, 'CTG.02': 3, 'STG.03': 4, 'UTG.04': 1, 'ATG.05': 1})

    def test_CTG_prop_trh(self):
        """ ConcurrencyThreadGroup: properties in throughput, ramp-up, hold-for """
        self.configure(load={'ramp-up': '${__P(r)}', 'throughput': '${__P(t)}', 'hold-for': '${__P(h)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        self.assertListEqual(self._get_tst_schedule(),
                             [["1.0", "${__P(t)}", "${__P(r)}"], ["${__P(t)}", "${__P(t)}", "${__P(h)}"]], )

    def test_TST_low_val(self):
        """ ConcurrencyThreadGroup: properties in throughput, ramp-up, hold-for """
        self.configure(load={'ramp-up': '300', 'throughput': '2', 'hold-for': '6900'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        self.assertListEqual(self._get_tst_schedule(),
                             [["1.0", "2.0", "300"], ["2.0", "2.0", "6900"]], )

    def _get_tst_schedule(self):
        records = []
        shaper_elements = self.jmx.get(r"kg\.apc\.jmeter\.timers\.VariableThroughputTimer")
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

        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("${__P(c)}", group.element.find(".//*[@name='ThreadGroup.num_threads']").text)
            self.assertEqual("${__P(i)}", group.element.find(".//*[@name='LoopController.loops']").text)
            duration_comp_exp = "${__P(h)}"

            self.assertEqual(duration_comp_exp, group.element.find(".//*[@name='ThreadGroup.duration']").text)

    def test_TG_prop_rh(self):
        """ ThreadGroup: properties in ramp-up, hold-for """
        self.configure(load={'ramp-up': '${__P(r)}', 'hold-for': '${__P(h)}'},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx', has_ctg=False)
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")

            delay = group.element.find(".//boolProp[@name='ThreadGroup.delayedStart']")
            self.assertIsNone(delay)

            self.assertEqual("-1", group.element.find(".//*[@name='LoopController.loops']").text)
            self.assertEqual("${__P(r)}", group.element.find(".//*[@name='ThreadGroup.ramp_time']").text)
            self.assertEqual("${__intSum(${__P(r)},${__P(h)})}",
                             group.element.find(".//*[@name='ThreadGroup.duration']").text)

    def test_CTG_h(self):
        """ ConcurrencyThreadGroup: hold-for """
        self.configure(load={'hold-for': 70.5}, jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.CTG, self.obj.tg)

        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            self.assertEqual("70", group.element.find(".//*[@name='Hold']").text)

            res_values[group.get_testname()] = {'conc': group.get_concurrency(), 'iterations': group.get_iterations()}

        self.assertEqual(res_values, {
            'TG.01': {'conc': 2, 'iterations': None},
            'CTG.02': {'conc': 3, 'iterations': 10},
            'STG.03': {'conc': 4, 'iterations': None},
            'UTG.04': {'conc': 1, 'iterations': None},
            'ATG.05': {'conc': 1, 'iterations': None}})

    def test_TG_ci(self):
        """ ThreadGroup: concurrency, iterations """
        self.configure(load={'concurrency': 1, 'iterations': 7},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/threadgroups.jmx')
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)

        self.obj.modify(self.jmx)

        self.assertEqual(5, len(self.get_groupset()))
        for group in self.get_groupset():
            self.assertEqual(1, group.get_concurrency())
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

            res_values[group.get_testname()] = group.get_concurrency()

        self.assertEqual(res_values, {'TG.01': 2, 'CTG.02': 3, 'STG.03': 4, 'UTG.04': 1, 'ATG.05': 1})

    def test_TG_cr(self):
        """ThreadGroup:  concurrency, ramp-up"""
        self.configure(load={'concurrency': 76, 'ramp-up': 4},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/UTG_dummy.jmx',
                       settings={'force-ctg': False})
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because keep-iterations is True
        self.obj.modify(self.jmx)
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("-1", group.element.find(".//*[@name='LoopController.loops']").text)

    def test_TG_iterations_property(self):
        self.configure(load={'hold-for': 1},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/iterations-property.jmx',
                       settings={'force-ctg': False})
        self.sniff_log(self.obj.log)

        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)
        self.obj.modify(self.jmx)

        res_values = {}
        for group in self.get_groupset():
            res_values[group.get_testname()] = {'conc': group.get_concurrency(), 'iter': group.get_iterations()}
        self.assertEqual(res_values, {'TG': {'conc': 2, 'iter': '${__P(LoopCount,5)}'}})

    def test_TG_iterations_from_load(self):
        """ThreadGroup:  concurrency, ramp-up, iterations"""
        self.configure(load={'concurrency': 76, 'ramp-up': 4, 'iterations': 5},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/iterations-TG.jmx',
                       settings={'keep-iterations': False})
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because iterations is not None
        self.obj.modify(self.jmx)
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("5", group.element.find(".//*[@name='LoopController.loops']").text)

    def test_TG_iterations_from_jmx(self):
        """ThreadGroup:  concurrency, ramp-up, iterations"""
        self.configure(load={'concurrency': 76, 'steps': 5, 'throughput': 20},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/iterations-TG.jmx',
                       settings={'force-ctg': False})
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because keep-iterations is True
        self.obj.modify(self.jmx)
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("false", group.element.find(".//*[@name='ThreadGroup.same_user_on_next_iteration']").text)
            self.assertEqual("10", group.element.find(".//*[@name='LoopController.loops']").text)

    def test_ultimate_TG(self):
        """ThreadGroup:  concurrency, ramp-up, iterations"""
        self.configure(load={'concurrency': 76, 'steps': 5, 'throughput': 20},
                       jmx_file=RESOURCES_DIR + 'jmeter/jmx/ultimateGroupTest.jmx',
                       settings={'force-ctg': True})
        self.assertEqual(LoadSettingsProcessor.TG, self.obj.tg)  # because keep-iterations is True
        self.obj.modify(self.jmx)
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            self.assertEqual("false", group.element.find(".//*[@name='ThreadGroup.same_user_on_next_iteration']").text)
            self.assertEqual("1", group.element.find(".//*[@name='LoopController.loops']").text)


    def test_duration_loops_bug(self):
        self.configure(load={"concurrency": 10, "ramp-up": 15, "hold-for": "2m"},
                       jmx_file=RESOURCES_DIR + "/jmeter/jmx/http.jmx",
                       settings={'force-ctg': False})
        self.obj.modify(self.jmx)
        for group in self.get_groupset():
            self.assertEqual(group.gtype, "ThreadGroup")
            loop_ctrl = group.element.find(".//*[@name='ThreadGroup.main_controller']")
            tg_loops = loop_ctrl.find(".//stringProp[@name='LoopController.loops']").text
            tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']").text
            self.assertEqual(tg_loops, "1")
            self.assertEqual(tg_forever, "false")


class TestMQTTSamplers(BZTestCase):
    def test_full_generation(self):
        # check mqtt protocol handling: getting request, parsing of it, generation of jmx
        engine = EngineEmul()
        jmeter = MockJMeterExecutor()
        jmeter.engine = engine
        jmeter.configure({'scenario': 'sc1'})
        scenario = BetterDict.from_dict({
            'protocol': 'mqtt',
            'requests': [
                {'cmd': 'connect', 'addr': 'server.com'},
                {'cmd': 'disconnect'}
            ]})
        jmeter.engine.config.merge({'scenarios': {'sc1': scenario}})
        jmeter.settings.merge({'protocol-handlers': {'mqtt': 'bzt.jmx.mqtt.MQTTProtocolHandler'}})
        builder = JMeterScenarioBuilder(jmeter)
        elements = builder.compile_scenario(jmeter.get_scenario())
        self.assertEqual(4, len(elements))

        # appropriate classes has been generated
        self.assertEqual('net.xmeter.samplers.ConnectSampler', elements[0].attrib['testclass'])
        self.assertEqual('net.xmeter.samplers.DisConnectSampler', elements[2].attrib['testclass'])

    @staticmethod
    def get_mqtt_sample(config):
        request = BetterDict.from_dict(config)
        request = MQTTRequest(request, {})
        engine = EngineEmul()
        sample, _ = MQTTProtocolHandler({}, engine).get_sampler_pair(request)
        return sample, request

    def test_connect_sampler(self):
        config = {'label': 'connector', 'cmd': 'connect', 'addr': 'server.com'}
        sample = self.get_mqtt_sample(config)[0]
        self.assertEqual('net.xmeter.samplers.ConnectSampler', sample.attrib['testclass'])
        self.assertEqual(config['label'], sample.attrib['testname'])
        self.assertEqual(config['addr'], sample.find(".//stringProp[@name='mqtt.server']").text)

    def test_disconnect_sampler(self):
        config = {'label': 'disconnector', 'cmd': 'disconnect'}
        sample = self.get_mqtt_sample(config)[0]
        self.assertEqual('net.xmeter.samplers.DisConnectSampler', sample.attrib['testclass'])
        self.assertEqual(config['label'], sample.attrib['testname'])

    def test_publish_sampler(self):
        config = {'label': 'publisher', 'cmd': 'publish', 'topic': 't1', 'message': 'm1'}
        sample = self.get_mqtt_sample(config)[0]
        self.assertEqual('net.xmeter.samplers.PubSampler', sample.attrib['testclass'])
        self.assertEqual(config['label'], sample.attrib['testname'])
        self.assertEqual(config['topic'], sample.find(".//stringProp[@name='mqtt.topic_name']").text)
        self.assertEqual(config['message'], sample.find(".//stringProp[@name='mqtt.message_to_sent']").text)

    def test_subscribe_sampler(self):
        config = {'label': 'subscriber', 'cmd': 'subscribe', 'topic': 't1', 'time': 3, 'min-count': 10}
        sample, request = self.get_mqtt_sample(config)
        self.assertEqual('net.xmeter.samplers.SubSampler', sample.attrib['testclass'])
        self.assertEqual(config['label'], sample.attrib['testname'])
        self.assertEqual(config['topic'], sample.find(".//stringProp[@name='mqtt.topic_name']").text)
        self.assertEqual("specified elapsed time (ms)",
                         sample.find(".//stringProp[@name='mqtt.sample_condition']").text)
        self.assertEqual(str(config['time'] * 1000),
                         sample.find(".//stringProp[@name='mqtt.sample_condition_value']").text)
        self.assertIn('jsr223', request.config)
        jsr223 = request.config.get('jsr223')[0]
        self.assertEqual('groovy', jsr223['language'])
        self.assertIn('script-file', jsr223)
        self.assertEqual('after', jsr223['execute'])

class TestGRPCSamplers(BZTestCase):
    def test_full_generation(self):
        # check gRPC protocol handling: getting request, parsing of it, generation of jmx
        engine = EngineEmul()
        jmeter = MockJMeterExecutor()
        jmeter.engine = engine
        jmeter.configure({'scenario': 'sc1'})
        scenario = BetterDict.from_dict({
            'protocol': 'grpc',
            'timeout': '5s',
            'grpc-proto-folder': '/path/to/protobuf/folder',
            'grpc-lib-folder': '/path/to/grpc/lib/folder',
            'grpc-max-inbound-message-size': 4194304,
            'grpc-max-inbound-metadata-size': 8192,
            'tls-disable-verification': False,
            'requests': [
                {'url': 'http://api.example.com:8888/com.example.HelloWorldService/sayHello'},
                {
                    'url': 'https://api2.example.com:9999/com.example.AnotherService/fooBar',
                    'label': 'fooBar',
                    'body': '{ "param1": "value1", "param2": value2 }',
                    'metadata': 'key1:value1,key2:value2',

                    'timeout': '10s',
                    'grpc-proto-folder': '/another/proto/path',
                    'grpc-lib-folder': '/another/lib/path',
                    'grpc-max-inbound-message-size': 12345,
                    'grpc-max-inbound-metadata-size': 42,
                    'tls-disable-verification': True
                },
            ]})
        jmeter.engine.config.merge({'scenarios': {'sc1': scenario}})
        jmeter.settings.merge({'protocol-handlers': {'grpc': 'bzt.jmx.grpc.GRPCProtocolHandler'}})
        builder = JMeterScenarioBuilder(jmeter)
        elements = builder.compile_scenario(jmeter.get_scenario())
        self.assertEqual(4, len(elements))

        # appropriate classes has been generated
        element = elements[0]
        self.assertEqual('vn.zalopay.benchmark.GRPCSampler', element.attrib['testclass'])
        # URL-parsed props
        self.assertEqual('false', element.find(".//boolProp[@name='GRPCSampler.tls']").text)
        self.assertEqual('api.example.com', element.find(".//stringProp[@name='GRPCSampler.host']").text)
        self.assertEqual('8888', element.find(".//stringProp[@name='GRPCSampler.port']").text)
        self.assertEqual('com.example.HelloWorldService/sayHello', element.find(".//stringProp[@name='GRPCSampler.fullMethod']").text)
        # Other props
        self.assertEqual('/path/to/protobuf/folder', element.find(".//stringProp[@name='GRPCSampler.protoFolder']").text)
        self.assertEqual('/path/to/grpc/lib/folder', element.find(".//stringProp[@name='GRPCSampler.libFolder']").text)
        self.assertEqual('', element.find(".//stringProp[@name='GRPCSampler.metadata']").text)
        self.assertEqual('false', element.find(".//boolProp[@name='GRPCSampler.tlsDisableVerification']").text)
        self.assertEqual('5000', element.find(".//stringProp[@name='GRPCSampler.deadline']").text)
        self.assertEqual('5000', element.find(".//stringProp[@name='GRPCSampler.channelAwaitTermination']").text)
        self.assertEqual('4194304', element.find(".//stringProp[@name='GRPCSampler.maxInboundMessageSize']").text)
        self.assertEqual('8192', element.find(".//stringProp[@name='GRPCSampler.maxInboundMetadataSize']").text)
        self.assertEqual('', element.find(".//stringProp[@name='GRPCSampler.requestJson']").text)



        element = elements[2]
        self.assertEqual('vn.zalopay.benchmark.GRPCSampler', element.attrib['testclass'])
        # URL-parsed props
        self.assertEqual('true', element.find(".//boolProp[@name='GRPCSampler.tls']").text)
        self.assertEqual('api2.example.com', element.find(".//stringProp[@name='GRPCSampler.host']").text)
        self.assertEqual('9999', element.find(".//stringProp[@name='GRPCSampler.port']").text)
        self.assertEqual('com.example.AnotherService/fooBar', element.find(".//stringProp[@name='GRPCSampler.fullMethod']").text)
        # Other props
        self.assertEqual('/another/proto/path', element.find(".//stringProp[@name='GRPCSampler.protoFolder']").text)
        self.assertEqual('/another/lib/path', element.find(".//stringProp[@name='GRPCSampler.libFolder']").text)
        self.assertEqual('key1:value1,key2:value2', element.find(".//stringProp[@name='GRPCSampler.metadata']").text)
        self.assertEqual('true', element.find(".//boolProp[@name='GRPCSampler.tlsDisableVerification']").text)
        self.assertEqual('10000', element.find(".//stringProp[@name='GRPCSampler.deadline']").text)
        self.assertEqual('10000', element.find(".//stringProp[@name='GRPCSampler.channelAwaitTermination']").text)
        self.assertEqual('12345', element.find(".//stringProp[@name='GRPCSampler.maxInboundMessageSize']").text)
        self.assertEqual('42', element.find(".//stringProp[@name='GRPCSampler.maxInboundMetadataSize']").text)
        self.assertEqual('{ "param1": "value1", "param2": value2 }', element.find(".//stringProp[@name='GRPCSampler.requestJson']").text)


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

    def test_param_url(self):
        obj = JMX()
        res = obj._get_http_request(url="https://example_url.net/Xhtml;jsessionid=XXXX:XXXX?JacadaApplicationName=XXXX",
                                    label="label", method="method", timeout=0, body={}, keepalive=True)
        self.assertEqual("/Xhtml;jsessionid=XXXX:XXXX?JacadaApplicationName=XXXX",
                         res.find(".//stringProp[@name='HTTPSampler.path']").text)

    def test_huge_jmx(self):
        prefix = """<?xml version="1.0" encoding="UTF-8"?>
        <jmeterTestPlan version="1.2" properties="5.0" jmeter="5.5">
          <hashTree>
            <TestPlan guiclass="TestPlanGui" testclass="TestPlan" testname="AAAAA" enabled="true">
              <stringProp name="TestPlan.comments"></stringProp>
              <boolProp name="TestPlan.functional_mode">false</boolProp>
              <boolProp name="TestPlan.tearDown_on_shutdown">true</boolProp>
              <boolProp name="TestPlan.serialize_threadgroups">false</boolProp>
              <elementProp name="TestPlan.user_defined_variables" elementType="Arguments" guiclass="ArgumentsPanel" testclass="Arguments" testname="User Defined Variables" enabled="true">
                <collectionProp name="Arguments.arguments">
                  <elementProp name="BBBB" elementType="Argument">
                    <stringProp name="Argument.name">BBBB</stringProp>
                    <stringProp name="Argument.value">
        """
        suffix = """</stringProp>
                            <stringProp name="Argument.metadata">=</stringProp>
                          </elementProp>
                        </collectionProp>
                      </elementProp>
                      <stringProp name="TestPlan.user_define_classpath"></stringProp>
                    </TestPlan>
                    <hashTree/>
                  </hashTree>
                </jmeterTestPlan>        
                """

        jmxfile = io.BytesIO()
        jmxfile.write(prefix.encode('utf-8'))
        for i in range(1, 30):
            for i in range(1, 1024):
                jmxfile.write("A".encode('utf-8') * 1024)
                jmxfile.write("\n".encode('utf-8'))
        jmxfile.write(suffix.encode('utf-8'))
        jmxfile.seek(0)

        self.sniff_log()
        obj = JMX(original=jmxfile)
        self.assertTrue("huge text node" in self.log_recorder.warn_buff.getvalue())

        self.log_recorder.warn_buff.truncate(0)

        # huge with syntax error
        jmxfile = io.BytesIO()
        jmxfile.write(prefix.encode('utf-8'))
        for i in range(1, 30):
            for i in range(1, 1024):
                jmxfile.write("A".encode('utf-8') * 1024)
                jmxfile.write("\n".encode('utf-8'))
        jmxfile.write("</aaa>".encode('utf-8'))
        jmxfile.write(suffix.encode('utf-8'))
        jmxfile.seek(0)

        try:
            obj = JMX(original=jmxfile)
            self.assertTrue(False, "Exception not raised")
        except BaseException as exc:
            self.assertTrue("huge text node" in self.log_recorder.warn_buff.getvalue())
            self.assertTrue("XML parsing (with huge_tree) failed" in str(exc))

