""" test """
import json
import logging
import time
import os
import shutil
import yaml
import sys

from tests import setup_test_logging, BZTestCase, __dir__
from bzt.engine import Provisioning
from bzt.modules.jmeter import JMeterExecutor, JMX, JTLErrorsReader, JTLReader
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
from math import ceil

try:
    from lxml import etree
except ImportError:
    try:
        import cElementTree as etree
    except ImportError:
        import elementtree.ElementTree as etree

setup_test_logging()


class TestJMeterExecutor(BZTestCase):
    def test_jmx(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": "tests/jmx/dummy.jmx"}})
        obj.prepare()

    def test_jmx_2tg(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution = BetterDict()
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
        try:
            obj.prepare()
            self.fail()
        except RuntimeError:
            pass

    def test_broken_xml(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()

        obj.execution.merge({"scenario": {"script": "tests/jmx/broken.jmx"}})
        try:
            obj.prepare()
            self.fail()
        except RuntimeError:
            pass

    def test_not_jmx_xml(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": "tests/jmx/not-jmx.xml"}})
        try:
            obj.prepare()
            self.fail()
        except RuntimeError:
            pass

    def test_requests(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
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
        path = os.path.abspath(__dir__() + "/../../build/tmp/jmeter-taurus/bin/jmeter")

        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        jmeter_link = JMeterExecutor.JMETER_DOWNLOAD_LINK
        jmeter_ver = JMeterExecutor.JMETER_VER
        plugins_link = JMeterExecutor.PLUGINS_DOWNLOAD_TPL

        JMeterExecutor.JMETER_DOWNLOAD_LINK = "file://" + __dir__() + "/../data/jmeter-dist-{version}.zip"
        JMeterExecutor.PLUGINS_DOWNLOAD_TPL = "file://" + __dir__() + "/../data/jmeter-plugins-{plugin}.zip"
        JMeterExecutor.JMETER_VER = '2.13'

        self.assertFalse(os.path.exists(path))

        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})

        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"requests": []}})

        obj.prepare()
        jars = os.listdir(os.path.abspath(os.path.join(path, '../../lib')))
        old_jars = ['httpcore-4.2.5.jar', 'httpmime-4.2.6.jar', 'xercesImpl-2.9.1.jar', 'commons-jexl-1.1.jar',
                    'httpclient-4.2.6.jar']
        for old_jar in old_jars:
            self.assertNotIn(old_jar, jars)

        self.assertTrue(os.path.exists(path))

        obj.prepare()

        JMeterExecutor.JMETER_DOWNLOAD_LINK = jmeter_link
        JMeterExecutor.PLUGINS_DOWNLOAD_TPL = plugins_link
        JMeterExecutor.JMETER_VER = jmeter_ver

    def test_think_time_bug(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/think-time-bug.yml").read()))
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        result = open(obj.modified_jmx).read()
        self.assertIn('<stringProp name="ConstantTimer.delay">750</stringProp>', result)

    def test_body_parse(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()

        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        sampler_element = xml_tree.findall(".//HTTPSamplerProxy[@testname='With body params']")
        arguments_element_prop = sampler_element[0][0]
        self.assertEqual(6, len(sampler_element[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop.getchildren()))
        self.assertEqual(2, len(arguments_element_prop[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop[0].findall(".//elementProp[@name='param1']")))
        self.assertEqual(1, len(arguments_element_prop.findall(".//elementProp[@name='param2']")))

    def __check_path_resource_files(self, jmx_file_path, exclude_jtls=False):
        xml_tree = etree.fromstring(open(jmx_file_path, "rb").read())
        search_patterns = ["File.path", "filename", "BeanShellSampler.filename"]
        for pattern in search_patterns:
            resource_elements = xml_tree.findall(".//stringProp[@name='%s']" % pattern)
            for resource_element in resource_elements:
                parent = resource_element.getparent()
                parent_disabled = False
                while parent is not None:
                    if parent.get('enabled') == 'false':
                        parent_disabled = True
                        break
                    parent = parent.getparent()
                if resource_element.text and parent_disabled is False:
                    if exclude_jtls:
                        if not resource_element.text.endswith('.jtl'):
                            self.assertEqual("", os.path.dirname(resource_element.text))
                    else:
                        self.assertEqual("", os.path.dirname(resource_element.text))

    def test_resource_files_collection_remote_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/jmx/files.jmx"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 5)
        self.assertEqual(len(artifacts), 5)
        target_jmx = os.path.join(obj.engine.artifacts_dir, "files.jmx")
        self.__check_path_resource_files(target_jmx)

    def test_resource_files_collection_local_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/jmx/files.jmx"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 8)
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_files.jmx.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True)

    def test_resource_files_from_requests_remote_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 2)
        self.assertEqual(len(artifacts), 2)

    def test_resource_files_from_requests_local_prov(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 7)  # + system.properties
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_requests.jmx.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True)

    def test_http_request_defaults(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
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
        obj.engine.config.merge(yaml.load(open("tests/yaml/throughput_constant.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        shaper_elements = xml_tree.findall(
            ".//kg.apc.jmeter.timers.VariableThroughputTimer[@testclass='kg.apc.jmeter.timers.VariableThroughputTimer']")
        self.assertEqual(1, len(shaper_elements))
        shaper_coll_element = shaper_elements[0].find(".//collectionProp[@name='load_profile']")

        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='49']").text)
        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='1567']").text)
        self.assertEqual("60", shaper_coll_element.find(".//stringProp[@name='53']").text)

    def test_add_shaper_ramp_up(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/throughput_ramp_up.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        shaper_elements = xml_tree.findall(
            ".//kg.apc.jmeter.timers.VariableThroughputTimer[@testclass='kg.apc.jmeter.timers.VariableThroughputTimer']")
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
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_user_def_vars_override(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge(yaml.load(open("tests/yaml/user_def_vars.yml").read()))
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_nonstandard_errors_format(self):
        obj = JTLErrorsReader(__dir__() + "/../data/nonstantard-errors.jtl", logging.getLogger(''))
        obj.read_file(True)
        values = obj.get_data(sys.maxsize)
        self.assertEquals(2, len(values))

    def test_standard_errors_format(self):
        obj = JTLErrorsReader(__dir__() + "/../data/standard-errors.jtl", logging.getLogger(''))
        obj.read_file(True)
        values = obj.get_data(sys.maxsize)
        self.assertEquals(3, len(values))

    def test_tranctl_jtl(self):
        obj = JTLReader(__dir__() + "/../data/tranctl.jtl", logging.getLogger(''), None)
        values = [x for x in obj.datapoints(True)]
        self.assertEquals(1, len(values))

    def test_distributed_th_hostnames(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/jmx/http.jmx"}})
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        thread_groups = xml_tree.findall(".//ThreadGroup")
        prepend_str = r"${__machineName()}"
        for thread_group in thread_groups:
            self.assertTrue(thread_group.attrib["testname"].startswith(prepend_str))

    def test_dns_cache_mgr_scenario(self):
        """
        No system properties
        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/jmx/http.jmx"}})
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
        obj.engine.config = json.loads(open(__dir__() + "/../../tests/json/get-post.json").read())
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
        obj.engine.config.merge(yaml.load(open("tests/yaml/dns_mgr_script.yml").read()))
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
        obj.engine.config.merge(yaml.load(open("tests/yaml/stepping_ramp_up.yml").read()))
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
        obj.engine.config.merge(yaml.load(open("tests/yaml/stepping_ramp_up.yml").read()))
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
                             #str(int(ceil(orig_num_threads * (float(load.concurrency) / orig_summ_cnc)) / load.steps)))

    def test_step_shaper(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/stepping_ramp_up.yml").read()))
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
