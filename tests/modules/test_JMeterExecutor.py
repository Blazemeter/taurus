""" test """
import json
import logging
import os
import shutil
import sys
import time
from math import ceil

import yaml

from bzt.engine import Provisioning
from bzt.jmx import JMX
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet
from bzt.modules.jmeter import JMeterExecutor, JTLErrorsReader, JTLReader, JMeter
from bzt.modules.jmeter import JMeterJTLLoaderExecutor, JMeterScenarioBuilder
from bzt.six import etree
from bzt.utils import BetterDict, EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ResultChecker, RecordingHandler


class TestJMeterExecutor(BZTestCase):
    def test_jmx(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/dummy.jmx"}})
        obj.prepare()

    def test_jmx_2tg(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution.merge({
            "concurrency": 1051,
            "ramp-up": 15,
            "iterations": 100,
            "scenario": {"script": __dir__() + "/../jmx/two_tg.jmx"}
        })
        obj.prepare()
        jmx = JMX(obj.modified_jmx)
        selector = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'
        selector += '>stringProp[name=ThreadGroup\.num_threads]'
        thr = jmx.get(selector)
        self.assertEquals('420', thr[0].text)
        self.assertEquals('631', thr[1].text)

    def test_not_jmx(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = {"scenario": {"script": __file__}}
        self.assertRaises(RuntimeError, obj.prepare)

    def test_broken_xml(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()

        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/broken.jmx"}})
        self.assertRaises(RuntimeError, obj.prepare)

    def test_not_jmx_xml(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/not-jmx.xml"}})
        self.assertRaises(RuntimeError, obj.prepare)

    def test_requests(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.log.debug("%s: %s", obj.modified_jmx, open(obj.modified_jmx).read())
        obj.log.debug("%s", json.dumps(obj.execution, indent=True))
        try:
            obj.startup()
            while not obj.check():
                obj.log.debug("Check...")
                time.sleep(1)
            obj.shutdown()
            obj.post_process()
        except:
            pass
        finally:
            if obj.jmeter_log and os.path.exists(obj.jmeter_log):
                obj.log.debug("%s", open(obj.jmeter_log).read())

    def test_issue_no_iterations(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({
            "concurrency": 10,
            "ramp-up": 10,
            "scenario": {
                "script": __dir__() + "/../jmx/issue_no_iterations.jmx"
            }
        })
        obj.prepare()

    def test_install_jmeter(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/jmeter-taurus/bin/jmeter" + EXE_SUFFIX)

        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        self.assertFalse(os.path.exists(path))

        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})

        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"requests": ["http://localhost"]}})

        obj.prepare()
        jars = os.listdir(os.path.abspath(os.path.join(path, '../../lib')))
        old_jars = ['httpcore-4.2.5.jar', 'httpmime-4.2.6.jar', 'xercesImpl-2.9.1.jar', 'commons-jexl-1.1.jar',
                    'httpclient-4.2.6.jar']
        for old_jar in old_jars:
            self.assertNotIn(old_jar, jars)

        self.assertTrue(os.path.exists(path))

        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})

        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"requests": ["http://localhost"]}})

        obj.prepare()

    def test_think_time_bug(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({'execution': {'ramp-up': '1m', 'hold-for': '1m30s', 'concurrency': 10,
                                               'scenario':
                                                   {'think-time': 0.75,
                                                    'requests':
                                                        ['http://blazedemo.com/',
                                                         'http://blazedemo.com/vacation.html']}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        result = open(obj.modified_jmx).read()
        self.assertIn('<stringProp name="ConstantTimer.delay">750</stringProp>', result)

    def test_body_parse(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()

        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        sampler_element = xml_tree.findall(".//HTTPSamplerProxy[@testname='With body params']")
        arguments_element_prop = sampler_element[0][0]
        self.assertEqual(9, len(sampler_element[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop.getchildren()))
        self.assertEqual(2, len(arguments_element_prop[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop[0].findall(".//elementProp[@name='param1']")))
        self.assertEqual(1, len(arguments_element_prop.findall(".//elementProp[@name='param2']")))

    def __check_path_resource_files(self, jmx_file_path, exclude_jtls=False, reverse_check=False):
        xml_tree = etree.fromstring(open(jmx_file_path, "rb").read())
        exclude_elements = ['kg.apc.jmeter.jmxmon.JMXMonCollector', 'JSR223Listener',
                            'kg.apc.jmeter.vizualizers.CorrectedResultCollector',
                            'kg.apc.jmeter.reporters.FlexibleFileWriter', 'BSFListener',
                            'kg.apc.jmeter.dbmon.DbMonCollector', 'BeanShellListener', 'MailerResultCollector',
                            'kg.apc.jmeter.perfmon.PerfMonCollector', 'ResultCollector',
                            'kg.apc.jmeter.vizualizers.CompositeResultCollector',
                            'kg.apc.jmeter.reporters.LoadosophiaUploader']
        search_patterns = ["File.path", "filename", "BeanShellSampler.filename"]
        for pattern in search_patterns:
            resource_elements = xml_tree.findall(".//stringProp[@name='%s']" % pattern)
            for resource_element in resource_elements:
                parent = resource_element.getparent()
                parent_disabled = False
                while parent is not None:
                    if parent.get('enabled') == 'false' or parent.tag in exclude_elements:
                        parent_disabled = True
                        break
                    parent = parent.getparent()
                if resource_element.text and parent_disabled is False:
                    if exclude_jtls:
                        if not resource_element.text.endswith('.jtl'):
                            if not reverse_check:
                                self.assertEqual("", os.path.dirname(resource_element.text))
                            else:
                                self.assertNotEqual("", os.path.dirname(resource_element.text))
                    else:
                        if not reverse_check:
                            self.assertEqual("", os.path.dirname(resource_element.text))
                        else:
                            self.assertNotEqual("", os.path.dirname(resource_element.text))

    def test_resource_files_collection_remote_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/files.jmx"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 5)
        self.assertEqual(len(artifacts), 5)
        target_jmx = os.path.join(obj.engine.artifacts_dir, "files.jmx")
        self.__check_path_resource_files(target_jmx)

    def test_resource_files_collection_local_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/files.jmx"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 7)  # minus jmeter.log
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_files.jmx.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True)

    def test_resource_files_from_requests_remote_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 2)
        self.assertEqual(len(artifacts), 2)

    def test_resource_files_from_requests_local_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 6)  # + system.properties, minus jmeter.log
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_requests.jmx.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True)

    def test_http_request_defaults(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        default_elements = xml_tree.findall(".//ConfigTestElement[@testclass='ConfigTestElement']")
        self.assertEqual(1, len(default_elements))

        default_element = default_elements[0]
        self.assertEqual("www.somehost.com", default_element.find(".//stringProp[@name='HTTPSampler.domain']").text)
        self.assertEqual("884", default_element.find(".//stringProp[@name='HTTPSampler.port']").text)
        self.assertEqual("https", default_element.find(".//stringProp[@name='HTTPSampler.protocol']").text)
        self.assertEqual("true", default_element.find(".//boolProp[@name='HTTPSampler.image_parser']").text)
        self.assertEqual("true", default_element.find(".//boolProp[@name='HTTPSampler.concurrentDwn']").text)
        self.assertEqual("10", default_element.find(".//stringProp[@name='HTTPSampler.concurrentPool']").text)
        # all keepalives in requests are disabled
        requests = xml_tree.findall(".//HTTPSamplerProxy[@testclass='HTTPSamplerProxy']")
        for request in requests:
            self.assertEqual("false", request.find(".//boolProp[@name='HTTPSampler.use_keepalive']").text)

    def test_add_shaper_constant(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m',
                                               'scenario': {'script': __dir__() + '/../jmx/http.jmx'}}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        timer_ = ".//kg.apc.jmeter.timers.VariableThroughputTimer"
        timer_ += "[@testclass='kg.apc.jmeter.timers.VariableThroughputTimer']"
        shaper_elements = xml_tree.findall(timer_)
        self.assertEqual(1, len(shaper_elements))
        shaper_coll_element = shaper_elements[0].find(".//collectionProp[@name='load_profile']")

        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='49']").text)
        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='1567']").text)
        self.assertEqual("60", shaper_coll_element.find(".//stringProp[@name='53']").text)

    def test_add_shaper_ramp_up(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'ramp-up': '1m', 'throughput': 10, 'hold-for': '2m', 'concurrency': 20,
                                               'scenario': {'script': __dir__() + '/../jmx/http.jmx'}}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        timer_ = ".//kg.apc.jmeter.timers.VariableThroughputTimer"
        timer_ += "[@testclass='kg.apc.jmeter.timers.VariableThroughputTimer']"
        shaper_elements = xml_tree.findall(timer_)
        self.assertEqual(1, len(shaper_elements))
        shaper_coll_element = shaper_elements[0].find(".//collectionProp[@name='load_profile']")

        self.assertEqual("1", shaper_coll_element.findall(".//stringProp[@name='49']")[0].text)
        self.assertEqual("10", shaper_coll_element.findall(".//stringProp[@name='1567']")[0].text)
        self.assertEqual("60", shaper_coll_element.findall(".//stringProp[@name='53']")[0].text)

        self.assertEqual("10", shaper_coll_element.findall(".//stringProp[@name='49']")[1].text)
        self.assertEqual("10", shaper_coll_element.findall(".//stringProp[@name='1567']")[1].text)
        self.assertEqual("120", shaper_coll_element.findall(".//stringProp[@name='53']")[1].text)

    def test_user_def_vars_from_requests(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_user_def_vars_override(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m', 'scenario': {
            'variables': {'my_var': 'http://demo.blazemeter.com/api/user', 'myvar2': 'val2'},
            'properties': {'log_level.jmeter': 'DEBUG'}, 'script': __dir__() + '/../jmx/http.jmx'}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_anonstandard_errors_format(self):
        obj = JTLErrorsReader(__dir__() + "/../data/nonstandard-errors.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertNotEquals(values[''][0]['msg'].find('Cannot find function error in object FirefoxDriver'), -1)

    def test_standard_errors_format(self):
        obj = JTLErrorsReader(__dir__() + "/../data/standard-errors.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEquals(3, len(values))

    def test_tranctl_jtl(self):
        obj = JTLReader(__dir__() + "/../data/tranctl.jtl", logging.getLogger(''), None)
        values = [x for x in obj.datapoints(True)]
        self.assertEquals(1, len(values))

    def test_distributed_th_hostnames(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        thread_groups = xml_tree.findall(".//ThreadGroup")
        prepend_str = r"${__machineName()}"
        for thread_group in thread_groups:
            self.assertTrue(thread_group.attrib["testname"].startswith(prepend_str))

        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        thread_groups = xml_tree.findall(".//ThreadGroup")
        prepend_str = r"${__machineName()}"
        for thread_group in thread_groups:
            self.assertFalse(thread_group.attrib["testname"].startswith(prepend_str))

    def test_dns_cache_mgr_scenario(self):
        """
        No system properties
        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        dns_element = xml_tree.findall(".//DNSCacheManager")
        # no dns manager when using jmx, no system.properties file
        self.assertEqual(len(dns_element), 0)
        arts = os.listdir(obj.engine.artifacts_dir)
        self.assertNotIn("system.properties", arts)

    def test_dns_cache_mgr_requests(self):
        """

        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        dns_managers = xml_tree.findall(".//DNSCacheManager")
        # 1 dns_manager
        self.assertEqual(len(dns_managers), 1)
        # check system.properies file contents
        sys_prop = open(os.path.join(obj.engine.artifacts_dir, "system.properties")).read()
        self.assertTrue("any_prop=true" in sys_prop)
        self.assertTrue("sun.net.inetaddr.ttl=0" in sys_prop)

    def test_dns_cache_mgr_script(self):
        """

        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'ramp-up': 10, 'throughput': 2, 'hold-for': 20, 'concurrency': 5,
                                               'scenario': {'think-time': '0.75s',
                                                            'script': __dir__() + '/../jmx/http.jmx'}},
                                 'modules': {'jmeter': {'system-properties': {'any_prop': 'true'},
                                                        'properties': {'log_level.jmeter': 'WARN',
                                                                       'log_level.jmeter.threads': 'DEBUG',
                                                                       'my-hostname': 'www.pre-test.com'}}}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        dns_managers = xml_tree.findall(".//DNSCacheManager")
        # 0 dns_managers
        self.assertEqual(len(dns_managers), 0)
        sys_prop = open(os.path.join(obj.engine.artifacts_dir, "system.properties")).read()
        self.assertTrue("any_prop=true" in sys_prop)
        self.assertFalse("sun.net.inetaddr.ttl=0" in sys_prop)

    def test_stepping_tg_ramp_no_proportion(self):
        """
        Tested without concurrency proportions
        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'steps': 5, 'concurrency': 170,
                                               'scenario': {'script': __dir__() + '/../jmx/stepping_ramp_up.jmx'},
                                               'ramp-up': '1m', 'distributed': ['127.0.0.1'], 'hold-for': '2m'}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        load = obj.get_load()
        orig_xml_tree = etree.fromstring(open(obj.original_jmx, "rb").read())
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        mod_stepping_tgs = modified_xml_tree.findall(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        orig_tgs = orig_xml_tree.findall(".//ThreadGroup")
        self.assertEqual(len(mod_stepping_tgs), len(orig_tgs))
        for orig_th, step_th in zip(orig_tgs, mod_stepping_tgs):
            orig_num_threads = int(orig_th.find(".//stringProp[@name='ThreadGroup.num_threads']").text)
            mod_num_threads = int(step_th.find(".//stringProp[@name='ThreadGroup.num_threads']").text)
            self.assertEqual(orig_num_threads, mod_num_threads)

            self.assertEqual(step_th.find(".//stringProp[@name='Start users period']").text,
                             str(int(load.ramp_up / load.steps)))
            self.assertEqual(step_th.find(".//stringProp[@name='Start users count']").text,
                             str(int(orig_num_threads / load.steps)))

    def test_stepping_tg_ramp_proportion(self):
        """
        Tested with concurrency proportions
        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'steps': 5, 'concurrency': 170,
                                               'scenario': {'script': __dir__() + '/../jmx/stepping_ramp_up.jmx'},
                                               'ramp-up': '1m', 'distributed': ['127.0.0.1'], 'hold-for': '2m'}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution['concurrency'] = 100  # from 170 to 100
        obj.execution['steps'] = 4  # from 5 to 4
        obj.prepare()
        load = obj.get_load()
        orig_xml_tree = etree.fromstring(open(obj.original_jmx, "rb").read())
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        mod_stepping_tgs = modified_xml_tree.findall(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        orig_tgs = orig_xml_tree.findall(".//ThreadGroup")
        self.assertEqual(len(mod_stepping_tgs), len(orig_tgs))
        orig_summ_cnc = sum([int(x.find(".//stringProp[@name='ThreadGroup.num_threads']").text) for x in orig_tgs])
        for orig_th, step_th in zip(orig_tgs, mod_stepping_tgs):
            orig_num_threads = int(orig_th.find(".//stringProp[@name='ThreadGroup.num_threads']").text)
            mod_num_threads = int(step_th.find(".//stringProp[@name='ThreadGroup.num_threads']").text)

            self.assertEqual(round(orig_num_threads * (float(load.concurrency) / orig_summ_cnc)), mod_num_threads)
            self.assertEqual(step_th.find(".//stringProp[@name='Start users period']").text,
                             str(int(load.ramp_up / load.steps)))
            self.assertEqual(step_th.find(".//stringProp[@name='Start users count']").text,
                             str(int(ceil(float(load.concurrency) / orig_summ_cnc * orig_num_threads / load.steps))))

    def test_step_shaper(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'steps': 5, 'concurrency': 170,
                                               'scenario': {'script': __dir__() + '/../jmx/stepping_ramp_up.jmx'},
                                               'ramp-up': '1m', 'distributed': ['127.0.0.1'], 'hold-for': '2m'}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution['throughput'] = 100
        obj.prepare()
        load = obj.get_load()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        timer = modified_xml_tree.findall(".//kg.apc.jmeter.timers.VariableThroughputTimer")
        self.assertEqual(len(timer), 1)
        for num, step_collection in enumerate(timer[0].findall(".//load_profile")):
            step_start_rps = step_collection.find(".//stringProp[@name='49']")
            step_stop_rps = step_collection.find(".//stringProp[@name='1567']")
            self.assertTrue(step_start_rps == step_stop_rps == str(int(round(float(load.throughput) / load.steps))))
            if num + 1 == load.steps:
                self.assertEqual(step_collection.find(".//stringProp[@name='53']"),
                                 load.hold + load.ramp_up / load.steps)
            else:
                self.assertEqual(step_collection.find(".//stringProp[@name='53']"), load.ramp_up / load.steps)

    def test_csv_path_bug_in_distributed_mode(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/files.jmx"}})
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_files.jmx.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True, reverse_check=True)

    def test_duration_loops_bug(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution = BetterDict()
        obj.execution.merge({
            "concurrency": 10,
            "ramp-up": 15,
            "hold-for": "2m",
            "scenario": {"script": __dir__() + "/../jmx/http.jmx"}
        })
        obj.prepare()

        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//intProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "-1")
        self.assertEqual(tg_forever.text, "false")

    def test_iterations_loop_bug(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution = BetterDict()
        obj.execution.merge({"iterations": 10, "scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.prepare()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//stringProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "10")
        self.assertEqual(tg_forever.text, "false")

        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.prepare()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find("*[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "1")  # default value, not disabled
        self.assertEqual(tg_forever.text, "false")

    def test_distributed_jtl(self):
        obj = JMeterJTLLoaderExecutor()
        obj.engine = EngineEmul()
        obj.engine.aggregator = ConsolidatingAggregator()
        self.maxc = 0

        def clb(x):
            self.maxc = max(self.maxc, x[DataPoint.CURRENT][''][KPISet.CONCURRENCY])

        obj.engine.aggregator.add_listener(ResultChecker(clb))
        obj.execution = BetterDict()
        obj.execution.merge({"kpi-jtl": __dir__() + "/../data/distributed.jtl"})
        obj.prepare()
        obj.reader.is_distributed = True

        obj.engine.aggregator.post_process()
        self.assertEquals(23, self.maxc)

    def test_distributed_gui(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open(__dir__() + "/../yaml/distributed_gui.yml").read()))
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        obj.execution = obj.engine.config['execution']
        obj.prepare()

        prop_file_path = os.path.join(obj.engine.artifacts_dir, "jmeter-bzt.properties")
        self.assertTrue(os.path.exists(prop_file_path))
        with open(prop_file_path) as prop_file:
            contents = prop_file.read()
        self.assertIn("remote_hosts=127.0.0.1,127.0.0.2", contents)

    def test_empty_requests(self):  # https://groups.google.com/forum/#!topic/codename-taurus/iaT6O2UhfBE
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge({'execution': {'ramp-up': '10s', 'requests': ['http://blazedemo.com/',
                                                                              'http://blazedemo.com/vacation.html'],
                                               'hold-for': '30s', 'concurrency': 5, 'scenario': {'think-time': 0.75}}})
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        obj.execution = obj.engine.config['execution']

        try:
            obj.prepare()
            self.fail()
        except RuntimeError as exc:
            self.assertEqual(exc.args[0], "Nothing to test, no requests were provided in scenario")

    def test_variable_csv_file(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/variable_csv.jmx"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 3)  # minus jmeter.log
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_variable_csv.jmx.jmx")
        with open(target_jmx) as fds:
            jmx = fds.read()
            self.assertIn('<stringProp name="filename">${root}/csvfile.csv</stringProp>', jmx)

    def test_css_jquery_extractor(self):
        obj = JMeterExecutor()
        handler = RecordingHandler()
        obj.log.addHandler(handler)
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        target_jmx = os.path.join(obj.engine.artifacts_dir, "requests.jmx")
        modified_xml_tree = etree.fromstring(open(target_jmx, "rb").read())
        jq_css_extractors = modified_xml_tree.findall(".//HtmlExtractor")
        self.assertEqual(2, len(jq_css_extractors))
        simplified_extractor = modified_xml_tree.find(".//HtmlExtractor[@testname='Get name1']")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.refname']").text, "name1")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.expr']").text,
                         "input[name~=my_input]")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.attribute']").text, None)
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.match_number']").text, "0")
        self.assertEqual(simplified_extractor.find(".//stringProp[@name='HtmlExtractor.default']").text, "NOT_FOUND")
        full_form_extractor = modified_xml_tree.find(".//HtmlExtractor[@testname='Get name2']")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.refname']").text, "name2")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.expr']").text,
                         "input[name=JMeter]")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.attribute']").text, "value")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.match_number']").text, "1")
        self.assertEqual(full_form_extractor.find(".//stringProp[@name='HtmlExtractor.default']").text, "NV_JMETER")
        obj.log.removeHandler(handler)

    def test_shutdown_soft(self):
        obj = JMeterExecutor()
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/dummy.jmx"}})
        try:
            obj.prepare()
            obj.startup()
            time.sleep(1)
            obj.shutdown()
        except:
            self.fail()
        finally:
            obj.log.removeHandler(log_recorder)
        self.assertIn("JMeter stopped on Shutdown command", log_recorder.debug_buff.getvalue())

    def test_embedded_resources_main_sample_fail_assert(self):
        obj = JTLErrorsReader(__dir__() + "/../data/resource-errors-main-assert.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Test failed")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "Test failed")

    def test_embedded_resources_fail_child_no_assert(self):
        obj = JTLErrorsReader(__dir__() + "/../data/resource-errors-child-no-assert.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "NOT FOUND")

    def test_embedded_resources_fail_child_assert(self):
        obj = JTLErrorsReader(__dir__() + "/../data/resource-errors-child-assert.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "subsample assertion error")
        self.assertEqual(values.get('')[1].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "subsample assertion error")
        self.assertEqual(values.get('HTTP Request')[1].get("msg"), "NOT FOUND")

    def test_resource_tc(self):
        obj = JTLErrorsReader(__dir__() + "/../data/resource_tc.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "message")
        self.assertEqual(values.get('')[1].get("msg"), "FOUND")
        self.assertEqual(values.get('')[2].get("msg"), "second message")
        self.assertEqual(values.get('')[3].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('')[4].get("msg"), "Failed")

        self.assertEqual(values.get('tc1')[0].get("msg"), "FOUND")
        self.assertEqual(values.get('tc3')[0].get("msg"), "message")
        self.assertEqual(values.get('tc3')[1].get("msg"), "second message")
        self.assertEqual(values.get('tc4')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('tc5')[0].get("msg"), "Failed")

    def test_embedded_resources_no_fail(self):
        obj = JTLErrorsReader(__dir__() + "/../data/resource-errors-no-fail.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(len(values.get('HTTP Request')), 1)
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "failed_resource_message")

    def test_fail_on_zero_results(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.aggregator = ConsolidatingAggregator()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/dummy.jmx"}})
        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_jmeter_mirrors(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/jmeter-taurus/bin/jmeter" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        obj = JMeterExecutor()
        objjm = JMeter(path, obj.log, JMeterExecutor.JMETER_VER)
        objjm.install()

    def test_convert_tgroups_no_load(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution = BetterDict()
        obj.execution.merge({
            "scenario": {"script": __dir__() + "/../jmx/SteppingThreadGroup.jmx"}
        })
        obj.prepare()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        st_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        self.assertNotEqual(st_tg, None)
        ul_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.UltimateThreadGroup")
        self.assertNotEqual(ul_tg, None)

    def test_convert_tgroups_load_modifications(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution = BetterDict()
        obj.execution.merge({
            "iterations": 20,
            "ramp-up": 10,
            "hold-for": "2m",
            "scenario": {"script": __dir__() + "/../jmx/SteppingThreadGroup.jmx"}
        })
        obj.prepare()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        st_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        self.assertEqual(st_tg, None)
        ul_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.UltimateThreadGroup")
        self.assertEqual(ul_tg, None)

        converted_st_tg = modified_xml_tree.find(".//ThreadGroup[@testname='stepping tg']")

        loop_ctrl = converted_st_tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//*[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "20")
        self.assertEqual(tg_forever.text, "false")

        st_tg_concurrency = converted_st_tg.find(".//stringProp[@name='ThreadGroup.num_threads']")
        self.assertEqual(st_tg_concurrency.text, "123")

    def test_smart_time(self):
        s_t = JMeterScenarioBuilder.smart_time
        self.assertEqual(s_t('1m'), 60 * 1000.0)
        self.assertEqual(s_t('${VAR}'), '${VAR}')

    def test_json_body_app_str(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": "{\"store_id\": \"${store_id}\", \"display_name\": \"${display_name}\"}"
                }]}})
        obj.prepare()
        jmx = JMX(obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertNotEqual(jmx.get(selector)[0].text.find('store_id'), -1)

    def test_json_body_app_dic(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": {
                        "store_id": "${store_id}",
                        "display_name": "${display_name}"
                    }}]}})
        obj.prepare()
        jmx = JMX(obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertNotEqual(jmx.get(selector)[0].text.find('store_id'), -1)

    def test_json_body_no_app(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/exi"},
                    "body": {
                        "store_id": "${store_id}",
                        "display_name": "${display_name}"
                    }}]}})
        obj.prepare()
        jmx = JMX(obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertEqual(jmx.get(selector)[0].text.find('store_id'), -1)

    def test_a1_jtl_verbose(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "write-xml-jtl": "full",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    }]}})
        obj.prepare()

    def test_jtl_none(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "write-xml-jtl": "bla-bla-bla",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    }]}})
        obj.prepare()
