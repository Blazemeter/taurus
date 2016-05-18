# coding=utf-8
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
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.jmeter import JMeterExecutor, JTLErrorsReader, JTLReader
from bzt.modules.jmeter import JMeterScenarioBuilder
from bzt.six import etree, u
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, RecordingHandler


def get_jmeter():
    dir_name = os.path.dirname(__file__)
    path = dir_name + "/../jmeter/jmeter-loader" + EXE_SUFFIX
    obj = JMeterExecutor()
    obj.engine = EngineEmul()
    obj.settings.merge({'path': path})
    return obj


class TestJMeterExecutor(BZTestCase):
    def setUp(self):
        self.obj = get_jmeter()

    def tearDown(self):
        if self.obj.modified_jmx and os.path.exists(self.obj.modified_jmx):
            os.remove(self.obj.modified_jmx)

    def test_jmx(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/dummy.jmx"}})
        self.obj.engine.create_artifacts_dir()
        self.obj.prepare()

    def test_jmx_2tg(self):
        self.obj.engine.config[Provisioning.PROV] = 'test'
        self.obj.execution.merge({
            "concurrency": 1051,
            "ramp-up": 15,
            "iterations": 100,
            "scenario": {"script": __dir__() + "/../jmeter/jmx/two_tg.jmx"}
        })
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'
        selector += '>stringProp[name=ThreadGroup\.num_threads]'
        thr = jmx.get(selector)
        self.assertEquals('420', thr[0].text)
        self.assertEquals('631', thr[1].text)

    def test_not_jmx(self):
        self.obj.execution = {"scenario": {"script": __file__}}
        self.assertRaises(RuntimeError, self.obj.prepare)

    def test_broken_xml(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/broken.jmx"}})
        self.assertRaises(RuntimeError, self.obj.prepare)

    def test_not_jmx_xml(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/not-jmx.xml"}})
        self.assertRaises(RuntimeError, self.obj.prepare)

    def test_requests(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        self.obj.log.debug("%s: %s", self.obj.modified_jmx, open(self.obj.modified_jmx).read())
        self.obj.log.debug("%s", json.dumps(self.obj.execution, indent=True))
        try:
            self.obj.startup()
            while not self.obj.check():
                self.obj.log.debug("Check...")
                time.sleep(1)
            self.obj.shutdown()
            self.obj.post_process()
        except:
            pass
        finally:
            if self.obj.jmeter_log and os.path.exists(self.obj.jmeter_log):
                self.obj.log.debug("%s", open(self.obj.jmeter_log).read())

    def test_issue_no_iterations(self):
        self.obj.execution.merge({
            "concurrency": 10,
            "ramp-up": 10,
            "scenario": {
                "script": __dir__() + "/../jmeter/jmx/issue_no_iterations.jmx"
            }
        })
        self.obj.prepare()

    def test_datasources_with_delimiter(self):
        self.obj.execution.merge({"scenario":
                                 {"requests": ["http://localhost"],
                                  "data-sources": [
                                      {"path": __dir__() + "/../data/test2.csv",
                                       "delimiter": ","}]}})
        self.obj.prepare()

    def test_datasources_without_delimiter(self):
        self.obj.execution.merge({"scenario":
                                 {"requests": ["http://localhost"],
                                  "data-sources": [
                                      {"path": __dir__() + "/../data/test2.csv"}]}})
        self.obj.prepare()

    def test_path_processing(self):
        class FakeTool(object):
            tool_path = ''
            installed = None

            def set(self, tool_path, installed):
                self.tool_path = tool_path
                self.installed = installed

            def check_if_installed(self):
                return self.installed

        fake = FakeTool()
        end_str = os.path.join('bin', 'jmeter' + EXE_SUFFIX)

        fake.set(__file__, True)  # real file, jmeter works: do nothing
        self.assertEqual(JMeterExecutor._need_to_install(fake), False)

        fake.set(__file__, False)  # real file, jmeter doesn't work: raise
        with self.assertRaises(ValueError):
            JMeterExecutor._need_to_install(fake)

        fake.set(os.path.curdir, True)  # real dir, $dir/bin/jmeter.EXT works: fix path only
        self.assertEqual(JMeterExecutor._need_to_install(fake), False)
        self.assertEqual(fake.tool_path, os.path.join(os.path.curdir, end_str))

        fake.set(os.path.curdir, False)  # real dir, $dir/bin/jmeter.EXT doesn't work: install into $dir
        self.assertEqual(JMeterExecutor._need_to_install(fake), True)
        self.assertEqual(fake.tool_path, os.path.join(os.path.curdir, end_str))

        # not real file/dir, looks like *bin/jmeter.EXT: make two steps up, use as dir, install jmeter into it
        fake.set('*' + end_str, False)
        self.assertEqual(JMeterExecutor._need_to_install(fake), True)
        self.assertEqual(fake.tool_path, '*' + end_str)

        # not real file/dir, doesn't look like *bin/jmeter.EXT: use as dir, install jmeter into it
        fake.set('*', False)
        self.assertEqual(JMeterExecutor._need_to_install(fake), True)
        self.assertEqual(fake.tool_path, os.path.join('*', end_str))

    def test_install_jmeter(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/jmeter-taurus/bin/jmeter" + EXE_SUFFIX)

        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        self.assertFalse(os.path.exists(path))

        jmeter_link = JMeterExecutor.JMETER_DOWNLOAD_LINK
        jmeter_ver = JMeterExecutor.JMETER_VER
        plugins_link = JMeterExecutor.PLUGINS_DOWNLOAD_TPL
        mirrors_link = JMeterExecutor.MIRRORS_SOURCE

        JMeterExecutor.MIRRORS_SOURCE = "file:///" + __dir__() + "/../data/unicode_file"
        JMeterExecutor.JMETER_DOWNLOAD_LINK = "file:///" + __dir__() + "/../data/jmeter-dist-{version}.zip"
        JMeterExecutor.PLUGINS_DOWNLOAD_TPL = "file:///" + __dir__() + "/../data/JMeterPlugins-{plugin}-1.3.0.zip"
        JMeterExecutor.JMETER_VER = '2.13'

        self.obj = get_jmeter()
        self.obj.settings.merge({"path": path})
        self.obj.execution.merge({"scenario": {"requests": ["http://localhost"]}})

        self.obj.prepare()

        jars = os.listdir(os.path.abspath(os.path.join(path, '../../lib')))
        old_jars = ['httpcore-4.2.5.jar', 'httpmime-4.2.6.jar', 'xercesImpl-2.9.1.jar', 'commons-jexl-1.1.jar',
                    'httpclient-4.2.6.jar']
        for old_jar in old_jars:
            self.assertNotIn(old_jar, jars)

        self.assertTrue(os.path.exists(path))

        self.obj = get_jmeter()
        self.obj.settings.merge({"path": path})
        self.obj.execution.merge({"scenario": {"requests": ["http://localhost"]}})

        self.obj.prepare()

        JMeterExecutor.JMETER_DOWNLOAD_LINK = jmeter_link
        JMeterExecutor.PLUGINS_DOWNLOAD_TPL = plugins_link
        JMeterExecutor.JMETER_VER = jmeter_ver
        JMeterExecutor.MIRRORS_SOURCE = mirrors_link

    def test_think_time_bug(self):
        self.obj.engine.config.merge({'execution': {'ramp-up': '1m', 'hold-for': '1m30s', 'concurrency': 10,
                                               'scenario':
                                                   {'think-time': 0.75,
                                                    'requests':
                                                        ['http://blazedemo.com/',
                                                         'http://blazedemo.com/vacation.html']}}})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        result = open(self.obj.modified_jmx).read()
        self.assertIn('<stringProp name="ConstantTimer.delay">750</stringProp>', result)

    def test_body_parse(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()

        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        sampler_element = xml_tree.findall(".//HTTPSamplerProxy[@testname='With body params']")
        arguments_element_prop = sampler_element[0][0]
        self.assertEqual(10, len(sampler_element[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop.getchildren()))
        self.assertEqual(2, len(arguments_element_prop[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop[0].findall(".//elementProp[@name='param1']")))
        self.assertEqual(1, len(arguments_element_prop.findall(".//elementProp[@name='param2']")))

    def test_resource_files_collection_remote_prov(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/files.jmx"}})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 5)

    def test_resource_files_from_requests_remote_prov(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 2)

    def test_resource_files_from_requests_local_prov(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        files = ['jmeter-bzt.properties', 'modified_requests.jmx']
        files += ['requests.jmx', 'system.properties']
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertTrue(all([_file in artifacts for _file in files]))

    def test_resource_files_data_sources_shorthand(self):
        csv_file = __dir__() + '/../data/test1.csv'
        csv_file_uni = u(__dir__() + '/../data/test2.csv')
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    'data-sources': [csv_file, csv_file_uni],
                }
            }
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        resource_files = self.obj.resource_files()
        self.assertIn(csv_file, resource_files)
        self.assertIn(csv_file_uni, resource_files)

    def test_resource_files_data_sources_full_form(self):
        csv_file = __dir__() + '/../data/test1.csv'
        csv_file_uni = u(__dir__() + '/../data/test2.csv')
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    'data-sources': [{
                        'path': csv_file,
                        'loop': False,
                        'quoted': True,
                    }, {
                        'path': csv_file_uni,
                        'loop': False,
                        'quoted': True,
                    }],
                }
            }
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        resource_files = self.obj.resource_files()
        self.assertIn(csv_file, resource_files)
        self.assertIn(csv_file_uni, resource_files)

    def test_http_request_defaults(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
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
        self.obj.engine.config.merge({'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m',
                                               'scenario': {'script': __dir__() + '/../jmeter/jmx/http.jmx'}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        timer_ = ".//kg.apc.jmeter.timers.VariableThroughputTimer"
        timer_ += "[@testclass='kg.apc.jmeter.timers.VariableThroughputTimer']"
        shaper_elements = xml_tree.findall(timer_)
        self.assertEqual(1, len(shaper_elements))
        shaper_coll_element = shaper_elements[0].find(".//collectionProp[@name='load_profile']")

        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='49']").text)
        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='1567']").text)
        self.assertEqual("60", shaper_coll_element.find(".//stringProp[@name='53']").text)

    def test_add_shaper_ramp_up(self):
        self.obj.engine.config.merge({'execution': {'ramp-up': '1m', 'throughput': 10, 'hold-for': '2m', 'concurrency': 20,
                                               'scenario': {'script': __dir__() + '/../jmeter/jmx/http.jmx'}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
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
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_user_def_vars_override(self):
        self.obj.engine.config.merge({'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m', 'scenario': {
            'variables': {'my_var': 'http://demo.blazemeter.com/api/user', 'myvar2': 'val2'},
            'properties': {'log_level.jmeter': 'DEBUG'}, 'script': __dir__() + '/../jmeter/jmx/http.jmx'}}})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_nonstandard_errors_format(self):
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/nonstandard-errors.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertNotEquals(values[''][0]['msg'].find('Cannot find function error in object FirefoxDriver'), -1)

    def test_standard_errors_format(self):
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/standard-errors.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEquals(3, len(values))

    def test_tranctl_jtl(self):
        obj = JTLReader(__dir__() + "/../jmeter/jtl/tranctl.jtl", logging.getLogger(''), None)
        values = [x for x in obj.datapoints(True)]
        self.assertEquals(1, len(values))

    def test_distributed_th_hostnames(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/http.jmx"}})
        self.obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        for writer in writers:
            self.assertEqual('true', writer.find('objProp/value/hostname').text)

    def test_distributed_th_hostnames_complex(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        for writer in writers:
            self.assertEqual('true', writer.find('objProp/value/hostname').text)

    def test_dns_cache_mgr_scenario(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/http.jmx"}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        dns_element = xml_tree.findall(".//DNSCacheManager")
        # no dns manager when using jmx, no system.properties file
        self.assertEqual(len(dns_element), 0)
        arts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertNotIn("system.properties", arts)

    def test_dns_cache_mgr_requests(self):
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        dns_managers = xml_tree.findall(".//DNSCacheManager")
        # 1 dns_manager
        self.assertEqual(len(dns_managers), 1)
        # check system.properies file contents
        sys_prop = open(os.path.join(self.obj.engine.artifacts_dir, "system.properties")).read()
        self.assertTrue("any_prop=true" in sys_prop)
        self.assertTrue("sun.net.inetaddr.ttl=0" in sys_prop)

    def test_dns_cache_mgr_script(self):
        self.obj.engine.config.merge({'execution': {'ramp-up': 10, 'throughput': 2, 'hold-for': 20, 'concurrency': 5,
                                               'scenario': {'think-time': '0.75s',
                                                            'script': __dir__() + '/../jmeter/jmx/http.jmx'}},
                                 'modules': {'jmeter': {'system-properties': {'any_prop': 'true'},
                                                        'properties': {'log_level.jmeter': 'WARN',
                                                                       'log_level.jmeter.threads': 'DEBUG',
                                                                       'my-hostname': 'www.pre-test.com'}}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        dns_managers = xml_tree.findall(".//DNSCacheManager")
        # 0 dns_managers
        self.assertEqual(len(dns_managers), 0)
        sys_prop = open(os.path.join(self.obj.engine.artifacts_dir, "system.properties")).read()
        self.assertTrue("any_prop=true" in sys_prop)
        self.assertFalse("sun.net.inetaddr.ttl=0" in sys_prop)

    def test_stepping_tg_ramp_no_proportion(self):
        """
        Tested without concurrency proportions
        :return:
        """
        self.obj.engine.config.merge({'execution': {'steps': 5, 'concurrency': 170,
                                               'scenario': {
                                                   'script': __dir__() + '/../jmeter/jmx/stepping_ramp_up.jmx'},
                                               'ramp-up': '1m', 'distributed': ['127.0.0.1'], 'hold-for': '2m'}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        load = self.obj.get_load()
        orig_xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
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
        self.obj.engine.config.merge({'execution': {'steps': 5, 'concurrency': 170,
                                               'scenario': {
                                                   'script': __dir__() + '/../jmeter/jmx/stepping_ramp_up.jmx'},
                                               'ramp-up': '1m', 'distributed': ['127.0.0.1'], 'hold-for': '2m'}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.execution['concurrency'] = 100  # from 170 to 100
        self.obj.execution['steps'] = 4  # from 5 to 4
        self.obj.prepare()
        load = self.obj.get_load()
        orig_xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
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
        self.obj.engine.config.merge({'execution': {'steps': 5, 'concurrency': 170,
                                               'scenario': {
                                                   'script': __dir__() + '/../jmeter/jmx/stepping_ramp_up.jmx'},
                                               'ramp-up': '1m', 'distributed': ['127.0.0.1'], 'hold-for': '2m'}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.execution['throughput'] = 100
        self.obj.prepare()
        load = self.obj.get_load()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
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

    def test_duration_loops_bug(self):
        self.obj.engine.config[Provisioning.PROV] = 'test'
        self.obj.execution.merge({
            "concurrency": 10,
            "ramp-up": 15,
            "hold-for": "2m",
            "scenario": {"script": __dir__() + "/../jmeter/jmx/http.jmx"}
        })
        self.obj.prepare()

        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//intProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "-1")
        self.assertEqual(tg_forever.text, "false")

    def test_force_delimiters(self):
        self.obj.execution.merge({"iterations": 10, "scenario": {"script": __dir__() + "/../jmeter/jmx/delimiters.jmx"}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        delimiters = [delimiter.text for delimiter in jmx.get("CSVDataSet>stringProp[name='delimiter']")]
        self.assertEqual(['1', '2', ','], delimiters)

    def test_iterations_loop_bug(self):
        self.obj.engine.config[Provisioning.PROV] = 'test'
        self.obj.execution.merge({"iterations": 10, "scenario": {"script": __dir__() + "/../jmeter/jmx/http.jmx"}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//stringProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "10")
        self.assertEqual(tg_forever.text, "false")

        self.obj = get_jmeter()
        self.obj.engine.config[Provisioning.PROV] = 'test'
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/http.jmx"}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find("*[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "1")  # default value, not disabled
        self.assertEqual(tg_forever.text, "false")

    def test_distributed_gui(self):
        self.obj.engine.config.merge(yaml.load(open(__dir__() + "/../yaml/distributed_gui.yml").read()))
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()

        prop_file_path = os.path.join(self.obj.engine.artifacts_dir, "jmeter-bzt.properties")
        self.assertTrue(os.path.exists(prop_file_path))
        with open(prop_file_path) as prop_file:
            contents = prop_file.read()
        self.assertIn("remote_hosts=127.0.0.1,127.0.0.2", contents)

    def test_empty_requests(self):  # https://groups.google.com/forum/#!topic/codename-taurus/iaT6O2UhfBE
        self.obj.engine.config.merge({
            'execution': {
                'ramp-up': '10s',
                'requests': ['http://blazedemo.com/',
                             'http://blazedemo.com/vacation.html'],
                'hold-for': '30s',
                'concurrency': 5,
                'scenario': {'think-time': 0.75}}})
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.execution = self.obj.engine.config['execution']
        self.assertRaises(ValueError, self.obj.prepare)

    def test_variable_csv_file(self):
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/variable_csv.jmx"}})
        self.obj.prepare()
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 3)  # 2*effective, .properties, jmx
        with open(self.obj.modified_jmx) as fds:
            jmx = fds.read()
            self.assertIn('<stringProp name="filename">${root}/csvfile.csv</stringProp>', jmx)

    def test_css_jquery_extractor(self):
        handler = RecordingHandler()
        self.obj.log.addHandler(handler)

        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        target_jmx = os.path.join(self.obj.engine.artifacts_dir, "requests.jmx")
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
        self.obj.log.removeHandler(handler)

    def test_xpath_extractor(self):
        handler = RecordingHandler()
        self.obj.log.addHandler(handler)
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        target_jmx = os.path.join(self.obj.engine.artifacts_dir, "requests.jmx")
        modified_xml_tree = etree.fromstring(open(target_jmx, "rb").read())
        xpath_extractors = modified_xml_tree.findall(".//XPathExtractor")
        self.assertEqual(2, len(xpath_extractors))

        simplified = modified_xml_tree.find(".//XPathExtractor[@testname='Get xpath1']")
        self.assertEqual(simplified.find(".//stringProp[@name='XPathExtractor.refname']").text, "xpath1")
        self.assertEqual(simplified.find(".//stringProp[@name='XPathExtractor.xpathQuery']").text,
                         "/html/head/title")
        self.assertEqual(simplified.find(".//stringProp[@name='XPathExtractor.default']").text, "NOT_FOUND")
        self.assertEqual(simplified.find(".//boolProp[@name='XPathExtractor.validate']").text, "false")
        self.assertEqual(simplified.find(".//boolProp[@name='XPathExtractor.whitespace']").text, "true")
        self.assertEqual(simplified.find(".//boolProp[@name='XPathExtractor.tolerant']").text, "false")

        full_form = modified_xml_tree.find(".//XPathExtractor[@testname='Get xpath2']")
        self.assertEqual(full_form.find(".//stringProp[@name='XPathExtractor.refname']").text, "xpath2")
        self.assertEqual(full_form.find(".//stringProp[@name='XPathExtractor.xpathQuery']").text,
                         "/html/head/base")
        self.assertEqual(full_form.find(".//stringProp[@name='XPathExtractor.default']").text, "<no base>")
        self.assertEqual(full_form.find(".//boolProp[@name='XPathExtractor.validate']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPathExtractor.whitespace']").text, "true")
        self.assertEqual(full_form.find(".//boolProp[@name='XPathExtractor.tolerant']").text, "true")
        self.obj.log.removeHandler(handler)

    def test_xpath_assertion(self):
        handler = RecordingHandler()
        self.obj.log.addHandler(handler)
        self.obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        target_jmx = os.path.join(self.obj.engine.artifacts_dir, "requests.jmx")
        modified_xml_tree = etree.fromstring(open(target_jmx, "rb").read())
        assertions = modified_xml_tree.findall(".//XPathAssertion")
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
        self.obj.log.removeHandler(handler)

    def test_shutdown_soft(self):
        log_recorder = RecordingHandler()
        self.obj.log.addHandler(log_recorder)
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/dummy.jmx"}})
        try:
            self.obj.prepare()
            self.obj._env['TEST_MODE'] = 'server'
            self.obj.startup()
            time.sleep(1)
            self.obj.management_port = 8089
            self.obj.shutdown()
        except:
            self.fail()
        finally:
            self.obj.log.removeHandler(log_recorder)
        self.assertIn("JMeter stopped on Shutdown command", log_recorder.debug_buff.getvalue())

    def test_embedded_resources_main_sample_fail_assert(self):
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/resource-errors-main-assert.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "Test failed")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "Test failed")

    def test_embedded_resources_fail_child_no_assert(self):
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/resource-errors-child-no-assert.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "NOT FOUND")

    def test_embedded_resources_fail_child_assert(self):
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/resource-errors-child-assert.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(values.get('')[0].get("msg"), "subsample assertion error")
        self.assertEqual(values.get('')[1].get("msg"), "NOT FOUND")
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "subsample assertion error")
        self.assertEqual(values.get('HTTP Request')[1].get("msg"), "NOT FOUND")

    def test_resource_tc(self):
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/resource_tc.jtl", logging.getLogger(''))
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
        obj = JTLErrorsReader(__dir__() + "/../jmeter/jtl/resource-errors-no-fail.jtl", logging.getLogger(''))
        obj.read_file()
        values = obj.get_data(sys.maxsize)
        self.assertEqual(len(values.get('HTTP Request')), 1)
        self.assertEqual(values.get('HTTP Request')[0].get("msg"), "failed_resource_message")

    def test_fail_on_zero_results(self):
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.execution.merge({"scenario": {"script": __dir__() + "/../jmeter/jmx/dummy.jmx"}})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()
        self.assertRaises(RuntimeWarning, self.obj.post_process)

    def test_convert_tgroups_no_load(self):
        self.obj.engine.config[Provisioning.PROV] = 'test'
        self.obj.execution.merge({
            "scenario": {"script": __dir__() + "/../jmeter/jmx/SteppingThreadGroup.jmx"}
        })
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        st_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        self.assertNotEqual(st_tg, None)
        ul_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.UltimateThreadGroup")
        self.assertNotEqual(ul_tg, None)

    def test_convert_tgroups_load_modifications(self):
        self.obj.engine.config[Provisioning.PROV] = 'test'
        self.obj.execution.merge({
            "iterations": 20,
            "ramp-up": 10,
            "hold-for": "2m",
            "scenario": {"script": __dir__() + "/../jmeter/jmx/SteppingThreadGroup.jmx"}
        })
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
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
        self.obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": "{\"store_id\": \"${store_id}\", \"display_name\": \"${display_name}\"}"
                }]}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertNotEqual(jmx.get(selector)[0].text.find('store_id'), -1)

    def test_json_body_app_dic(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": {
                        "store_id": "${store_id}",
                        "display_name": "${display_name}"
                    }}]}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertNotEqual(jmx.get(selector)[0].text.find('store_id'), -1)

    def test_json_body_no_app(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/exi"},
                    "body": {
                        "store_id": "${store_id}",
                        "display_name": "${display_name}"
                    }}]}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertEqual(jmx.get(selector)[0].text.find('"store_id": "${store_id}"'), -1)

    def test_jtl_verbose(self):
        self.obj.execution.merge({
            "write-xml-jtl": "full",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                }]}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertNotEqual(jmx.get('ResultCollector[testname="Trace Writer"]'), [])
        self.assertEqual(jmx.get('ResultCollector[testname="Errors Writer"]'), [])
        
    def test_jtl_errors(self):
        self.obj.execution.merge({
            "write-xml-jtl": "error",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                }]}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertNotEqual(jmx.get('ResultCollector[testname="Errors Writer"]'), [])
        self.assertEqual(jmx.get('ResultCollector[testname="Trace Writer"]'), [])


    def test_jtl_none(self):
        self.obj.execution.merge({
            "write-xml-jtl": "bla-bla-bla",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                }]}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertEqual(jmx.get('ResultCollector[testname="Trace Writer"]'), [])
        self.assertEqual(jmx.get('ResultCollector[testname="Errors Writer"]'), [])

    def test_jmx_modification_unicode(self):
        cfg_selector = ('Home Page>HTTPsampler.Arguments>Arguments.arguments'
                        '>param>Argument.value')

        self.obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../jmeter/jmx/dummy_plan.jmx",
                "modifications": {
                    "set-prop": {
                        cfg_selector: u"✓",
                    }
                }
            }
        })
        selector = ("[testname='Home Page']>[name='HTTPsampler.Arguments']"
                    ">[name='Arguments.arguments']>[name='param']>[name='Argument.value']")
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertEqual(jmx.get(selector)[0].text, u"✓")

    def test_data_source_list(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com/"],
                # note that data-sources should be a list of strings/objects
                "data-sources": {
                    "path": __dir__() + "/../data/test1.csv",
                }
            }
        })
        self.assertRaises(ValueError, self.obj.prepare)

    def test_force_parent_sample(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../jmeter/jmx/transactions.jmx',
                    # 'force-parent-sample' is True by default
                }
            }
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        self.assertTrue(all(prop.text == 'true' for prop in props))

    def test_disable_force_parent_sample(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../jmeter/jmx/transactions.jmx',
                    'force-parent-sample': False,
                }
            }
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        non_parent = props[1]
        self.assertEqual(non_parent.text, 'false')

    def test_jvm_heap_settings(self):
        self.obj.engine.config.merge({'execution': {'iterations': 1,
                                               'scenario': {'script': __dir__() + '/../jmeter/jmx/http.jmx'}},
                                 'modules': {'jmeter': {'memory-xmx': '2G'}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        self.obj._env['TEST_MODE'] = 'heap'
        self.obj.startup()
        stdout, _ = self.obj.process.communicate()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIn("-Xmx2G", str(stdout))

    def test_jvm_heap_default_value(self):
        self.obj.engine.config.merge({'execution': {'iterations': 1,
                                               'scenario': {'script': __dir__() + '/../jmeter/jmx/http.jmx'}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        self.obj._env['TEST_MODE'] = 'heap'
        self.obj.startup()
        stdout, _ = self.obj.process.communicate()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIn("-Xmx", str(stdout))

    def test_data_sources_in_artifacts(self):
        self.obj.engine.config.merge({'execution': {'iterations': 1,
                                               'scenario': {'data-sources': ['test1.csv'],
                                                            'requests': ['http://blazedemo.com/${url}']}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        csv_source = __dir__() + '/../data/test1.csv'
        self.obj.engine.file_search_paths.append(self.obj.engine.artifacts_dir)
        shutil.copy2(csv_source, self.obj.engine.artifacts_dir)
        self.obj.prepare()

    def test_body_file_in_artifacts(self):
        self.obj.engine.config.merge({
            'execution': {
                'iterations': 1,
                'scenario': {
                    'requests': [{
                        "method": "PUT",
                        "url": "http://blazedemo.com/",
                        "body-file": "http.jmx"
                    }]}}})
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        jmx_source = __dir__() + '/../jmeter/jmx/http.jmx'
        self.obj.engine.file_search_paths.append(self.obj.engine.artifacts_dir)
        shutil.copy2(jmx_source, self.obj.engine.artifacts_dir)
        self.obj.prepare()

    def test_jmx_paths_local_prov(self):
        "Ensures that file paths in JMX are not changed during local prov"
        script = __dir__() + "/../jmeter/jmx/csvs.jmx"
        self.obj.engine.config.merge({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": script,
                }
            },
            'provisioning': 'local',
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        original = JMX(script)
        prepared = JMX(self.obj.modified_jmx)
        query = '//CSVDataSet/stringProp[@name="filename"]/text()'
        original_paths = original.tree.xpath(query)
        prepared_paths = prepared.tree.xpath(query)
        self.assertEqual(original_paths, prepared_paths)

    def test_jmx_paths_remote_prov(self):
        "Ensures that file paths in JMX are modified during remote prov"
        script = __dir__() + "/../jmeter/jmx/csvs.jmx"
        self.obj.engine.config.merge({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": script,
                }
            },
            'provisioning': 'test',
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.resource_files()
        original = JMX(script)
        prepared = JMX(self.obj.original_jmx)
        query = '//CSVDataSet/stringProp[@name="filename"]/text()'
        original_paths = original.tree.xpath(query)
        prepared_paths = prepared.tree.xpath(query)
        self.assertEqual(len(original_paths), len(prepared_paths))
        for orig, modified in zip(original_paths, prepared_paths):
            self.assertNotEqual(orig, modified)
            self.assertEqual(os.path.basename(orig), os.path.basename(modified))

    def test_request_logic_if(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "if": "<cond>",
                            "then": [
                                "http://blazedemo.com/",
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        if_controller = xml_tree.find(".//IfController")
        self.assertIsNotNone(if_controller)
        condition = xml_tree.find(".//IfController/stringProp[@name='IfController.condition']")
        self.assertIsNotNone(condition)
        self.assertEqual(condition.text, "<cond>")

    def test_request_logic_if_else(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "if": "<cond>",
                            "then": [
                                "http://blazedemo.com/",
                            ],
                            "else": [
                                "http://demo.blazemeter.com/",
                            ]
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        ifs = xml_tree.findall(".//IfController")
        self.assertEqual(2, len(ifs))
        conditions = xml_tree.findall(".//IfController/stringProp[@name='IfController.condition']")
        self.assertEqual(2, len(conditions))
        self.assertEqual(conditions[0].text, "<cond>")
        self.assertEqual(conditions[1].text, "!(<cond>)")

    def test_request_logic_nested_if(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "if": "<cond1>",
                            "then": [
                                "http://blazedemo.com/",
                                {
                                    "if": "<cond2>",
                                    "then": [
                                        "http://demo.blazemeter.com/"
                                    ]
                                },
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        ifs = xml_tree.findall(".//IfController")
        self.assertEqual(2, len(ifs))
        conditions = xml_tree.findall(".//IfController/stringProp[@name='IfController.condition']")
        self.assertEqual(2, len(conditions))
        self.assertEqual(conditions[0].text, "<cond1>")
        self.assertEqual(conditions[1].text, "<cond2>")

    def test_resource_files_nested_requests(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "data-sources": [__dir__() + "/../data/test1.csv"],
                    "requests": [
                        {
                            "if": "<cond1>",
                            "then": [
                                {
                                    "if": "<cond2>",
                                    "then": [
                                        {
                                            "url": "http://demo.blazemeter.com/",
                                            "method": "POST",
                                            "body-file": __dir__() + "/../jmeter/jmx/dummy.jmx",
                                        }
                                    ]
                                },
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 2)

    def test_request_logic_loop(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "loop": 10,
                            "do": [
                                "http://blazedemo.com/",
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        controller = xml_tree.find(".//LoopController")
        self.assertIsNotNone(controller)
        loops = xml_tree.find(".//LoopController/stringProp[@name='LoopController.loops']")
        self.assertEqual(loops.text, "10")
        forever = xml_tree.find(".//LoopController/boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(forever.text, "false")

    def test_request_logic_loop_forever(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "loop": "forever",
                            "do": [
                                "http://blazedemo.com/",
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        controller = xml_tree.find(".//LoopController")
        self.assertIsNotNone(controller)
        forever = xml_tree.find(".//LoopController/boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(forever.text, "true")
        loops = xml_tree.find(".//LoopController/stringProp[@name='LoopController.loops']")
        self.assertEqual(loops.text, "-1")

    def test_request_logic_loop_invalid(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "loop": 100,
                        }
                    ],
                }
            },
            "provisioning": "local",
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.assertRaises(ValueError, self.obj.prepare)

    def test_resource_files_loops(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "loop": 100,
                            "do": [
                                {
                                    "url": "http://demo.blazemeter.com/",
                                    "method": "POST",
                                    "body-file": __dir__() + "/../jmeter/jmx/dummy.jmx",
                                },
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_request_logic_while(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "while": "<cond>",
                            "do": [
                                "http://blazedemo.com/",
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge(self.obj.engine.config.get("modules").get("jmeter"))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        if_controller = xml_tree.find(".//WhileController")
        self.assertIsNotNone(if_controller)
        condition = xml_tree.find(".//WhileController/stringProp[@name='WhileController.condition']")
        self.assertIsNotNone(condition)
        self.assertEqual(condition.text, "<cond>")

    def test_request_logic_while_invalid(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "while": "<cond>",
                        }
                    ],
                }
            },
            "provisioning": "local",
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.assertRaises(ValueError, self.obj.prepare)

    def test_request_logic_while_resources(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "while": "<cond>",
                            "do": [
                                {
                                    "url": "http://demo.blazemeter.com/",
                                    "method": "POST",
                                    "body-file": __dir__() + "/../jmeter/jmx/dummy.jmx",
                                }
                            ],
                        }
                    ],
                }
            },
        })
        self.obj.engine.config.merge({"provisioning": "local"})
        self.obj.execution = self.obj.engine.config['execution']
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_request_logic_foreach(self):
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {
                    "requests": [
                        {
                            "foreach": "usernames",
                            "loop-variable": "name",
                            "do": [
                                "http://site.com/users/${name}",
                            ],
                        }
                    ],
                }
            },
            "provisioning": "local",
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertIsNotNone(xml_tree.find(".//ForeachController"))
        input = xml_tree.find(".//ForeachController/stringProp[@name='ForeachController.inputVal']")
        self.assertEqual(input.text, "usernames")
        loop_var = xml_tree.find(".//ForeachController/stringProp[@name='ForeachController.returnVal']")
        self.assertEqual(loop_var.text, "name")


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
