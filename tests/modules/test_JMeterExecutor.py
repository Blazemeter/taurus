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
from bzt.six import etree
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, RecordingHandler


def get_jmeter():
    path = __dir__() + "/../jmeter/jmeter" + EXE_SUFFIX
    obj = JMeterExecutor()
    obj.engine = EngineEmul()
    obj.settings.merge({'path': path})
    return obj


class TestJMeterExecutor(BZTestCase):

    def test_jmx(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/dummy.jmx"}})
        obj.engine.create_artifacts_dir()
        obj.prepare()

    def test_jmx_2tg(self):
        obj = get_jmeter()
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
        obj = get_jmeter()
        obj.execution = {"scenario": {"script": __file__}}
        self.assertRaises(RuntimeError, obj.prepare)

    def test_broken_xml(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/broken.jmx"}})
        self.assertRaises(RuntimeError, obj.prepare)

    def test_not_jmx_xml(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/not-jmx.xml"}})
        self.assertRaises(RuntimeError, obj.prepare)

    def test_requests(self):
        obj = get_jmeter()
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
        obj = get_jmeter()
        obj.execution.merge({
            "concurrency": 10,
            "ramp-up": 10,
            "scenario": {
                "script": __dir__() + "/../jmx/issue_no_iterations.jmx"
            }
        })
        obj.prepare()

    def test_datasources_with_delimiter(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario":
                                 {"requests": ["http://localhost"],
                                  "data-sources": [
                                      {"path": __dir__() + "/../data/test2.csv",
                                       "delimiter": ","}]}})
        obj.prepare()

    def test_datasources_without_delimiter(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario":
                                 {"requests": ["http://localhost"],
                                  "data-sources": [
                                      {"path": __dir__() + "/../data/test2.csv"}]}})
        obj.prepare()

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

        obj = get_jmeter()
        obj.settings.merge({"path": path})
        obj.execution.merge({"scenario": {"requests": ["http://localhost"]}})

        obj.prepare()

        jars = os.listdir(os.path.abspath(os.path.join(path, '../../lib')))
        old_jars = ['httpcore-4.2.5.jar', 'httpmime-4.2.6.jar', 'xercesImpl-2.9.1.jar', 'commons-jexl-1.1.jar',
                    'httpclient-4.2.6.jar']
        for old_jar in old_jars:
            self.assertNotIn(old_jar, jars)

        self.assertTrue(os.path.exists(path))

        obj = get_jmeter()
        obj.settings.merge({"path": path})
        obj.execution.merge({"scenario": {"requests": ["http://localhost"]}})

        obj.prepare()

        JMeterExecutor.JMETER_DOWNLOAD_LINK = jmeter_link
        JMeterExecutor.PLUGINS_DOWNLOAD_TPL = plugins_link
        JMeterExecutor.JMETER_VER = jmeter_ver
        JMeterExecutor.MIRRORS_SOURCE = mirrors_link

    def test_think_time_bug(self):
        obj = get_jmeter()
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
        obj = get_jmeter()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()

        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        sampler_element = xml_tree.findall(".//HTTPSamplerProxy[@testname='With body params']")
        arguments_element_prop = sampler_element[0][0]
        self.assertEqual(10, len(sampler_element[0].getchildren()))
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
        self.assertEqual(len(artifacts), 7)  # 5 + two effective configs
        target_jmx = os.path.join(obj.engine.artifacts_dir, "files.jmx")
        self.__check_path_resource_files(target_jmx)

    def test_resource_files_collection_local_prov(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/files.jmx"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 9)  # minus jmeter.log
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_files.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True)

    def test_resource_files_from_requests_remote_prov(self):
        obj = get_jmeter()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 2)
        self.assertEqual(len(artifacts), 4)  # 2 + two effective configs

    def test_resource_files_from_requests_local_prov(self):
        obj = get_jmeter()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        files = ['http.jmx', 'jmeter-bzt.properties', 'modified_requests.jmx']
        files += ['requests.jmx', 'system.properties', 'test1.csv']
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertTrue(all([_file in artifacts for _file in files]))  # +system.properties, -jmeter.log
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_requests.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True)

    def test_http_request_defaults(self):
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_user_def_vars_override(self):
        obj = get_jmeter()
        obj.engine.config.merge({'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m', 'scenario': {
            'variables': {'my_var': 'http://demo.blazemeter.com/api/user', 'myvar2': 'val2'},
            'properties': {'log_level.jmeter': 'DEBUG'}, 'script': __dir__() + '/../jmx/http.jmx'}}})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_nonstandard_errors_format(self):
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
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        for writer in writers:
            self.assertEqual('true', writer.find('objProp/value/hostname').text)

        obj = get_jmeter()
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        for writer in writers:
            self.assertEqual('true', writer.find('objProp/value/hostname').text)

    def test_dns_cache_mgr_scenario(self):
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.prepare()
        xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        dns_element = xml_tree.findall(".//DNSCacheManager")
        # no dns manager when using jmx, no system.properties file
        self.assertEqual(len(dns_element), 0)
        arts = os.listdir(obj.engine.artifacts_dir)
        self.assertNotIn("system.properties", arts)

    def test_dns_cache_mgr_requests(self):
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/files.jmx"}})
        obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        obj.prepare()
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_files.jmx")
        self.__check_path_resource_files(target_jmx, exclude_jtls=True, reverse_check=True)

    def test_duration_loops_bug(self):
        obj = get_jmeter()
        obj.engine.config[Provisioning.PROV] = 'test'
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

    def test_force_delimiters(self):
        obj = get_jmeter()
        obj.execution.merge({"iterations": 10, "scenario": {"script": __dir__() + "/../jmx/delimiters.jmx"}})
        obj.prepare()
        jmx = JMX(obj.modified_jmx)
        delimiters = [delimiter.text for delimiter in jmx.get("CSVDataSet>stringProp[name='delimiter']")]
        self.assertEqual(['1', '2', ','], delimiters)

    def test_iterations_loop_bug(self):
        obj = get_jmeter()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution.merge({"iterations": 10, "scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.prepare()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//stringProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "10")
        self.assertEqual(tg_forever.text, "false")

        obj = get_jmeter()
        obj.engine.config[Provisioning.PROV] = 'test'
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/http.jmx"}})
        obj.prepare()
        modified_xml_tree = etree.fromstring(open(obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find("*[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "1")  # default value, not disabled
        self.assertEqual(tg_forever.text, "false")

    def test_distributed_gui(self):
        obj = get_jmeter()
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
        obj = get_jmeter()
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
        obj = get_jmeter()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/variable_csv.jmx"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 5)  # minus jmeter.log
        target_jmx = os.path.join(obj.engine.artifacts_dir, "modified_variable_csv.jmx")
        with open(target_jmx) as fds:
            jmx = fds.read()
            self.assertIn('<stringProp name="filename">${root}/csvfile.csv</stringProp>', jmx)

    def test_css_jquery_extractor(self):
        obj = get_jmeter()
        handler = RecordingHandler()
        obj.log.addHandler(handler)

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

    def test_xpath_extractor(self):
        obj = get_jmeter()
        handler = RecordingHandler()
        obj.log.addHandler(handler)
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        target_jmx = os.path.join(obj.engine.artifacts_dir, "requests.jmx")
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
        obj.log.removeHandler(handler)

    def test_xpath_assertion(self):
        obj = get_jmeter()
        handler = RecordingHandler()
        obj.log.addHandler(handler)
        obj.engine.config = json.loads(open(__dir__() + "/../json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        target_jmx = os.path.join(obj.engine.artifacts_dir, "requests.jmx")
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
        obj.log.removeHandler(handler)

    def test_shutdown_soft(self):
        obj = get_jmeter()
        #obj.settings.merge({'path': ''})
        log_recorder = RecordingHandler()
        obj.log.addHandler(log_recorder)
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
        return
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
        obj.execution.merge({"scenario": {"script": __dir__() + "/../jmx/dummy.jmx"}})
        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_convert_tgroups_no_load(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'test'
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
        self.assertEqual(jmx.get(selector)[0].text.find('"store_id": "${store_id}"'), -1)

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

    def test_jmx_modification_unicode(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        cfg_selector = ('Home Page>HTTPsampler.Arguments>Arguments.arguments'
                        '>param>Argument.value')

        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../jmx/dummy_plan.jmx",
                "modifications": {
                    "set-prop": {
                        cfg_selector: u"✓",
                    }
                }
            }
        })
        selector = ("[testname='Home Page']>[name='HTTPsampler.Arguments']"
                    ">[name='Arguments.arguments']>[name='param']>[name='Argument.value']")
        obj.prepare()
        jmx = JMX(obj.modified_jmx)
        self.assertEqual(jmx.get(selector)[0].text, u"✓")

    def test_data_source_list(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com/"],
                # note that data-sources should be a list of strings/objects
                "data-sources": {
                    "path": __dir__() + "/../data/test1.csv",
                }
            }
        })
        self.assertRaises(ValueError, obj.prepare)

    def test_force_parent_sample(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../jmx/transactions.jmx',
                    # 'force-parent-sample' is True by default
                }
            }
        })
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        jmx = JMX(obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        self.assertTrue(all(prop.text == 'true' for prop in props))

    def test_disable_force_parent_sample(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'execution': {
                'scenario': {
                    'script': __dir__() + '/../jmx/transactions.jmx',
                    'force-parent-sample': False,
                }
            }
        })
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        jmx = JMX(obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        non_parent = props[1]
        self.assertEqual(non_parent.text, 'false')

    def test_jvm_heap_settings(self):
        """

        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({'execution': {'iterations': 1,
                                               'scenario': {'script': __dir__() + '/../jmx/http.jmx'}},
                                 'modules': {'jmeter': {'memory-xmx': '2G'}}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        fake_path = os.path.join(__dir__(), os.pardir, 'data', 'jmeter_jvm_args' + EXE_SUFFIX)
        obj.settings.merge({"path": fake_path})
        obj.prepare()
        obj.startup()
        stdout, _ = obj.process.communicate()
        obj.shutdown()
        obj.post_process()
        self.assertIn("-Xmx2G", str(stdout))

    def test_jvm_heap_default_value(self):
        """

        :return:
        """
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({'execution': {'iterations': 1,
                                               'scenario': {'script': __dir__() + '/../jmx/http.jmx'}}})
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("jmeter"))
        fake_path = os.path.join(__dir__(), os.pardir, 'data', 'jmeter_jvm_args' + EXE_SUFFIX)
        obj.settings.merge({"path": fake_path})
        obj.prepare()
        obj.startup()
        stdout, _ = obj.process.communicate()
        obj.shutdown()
        obj.post_process()
        self.assertIn("-Xmx", str(stdout))


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
