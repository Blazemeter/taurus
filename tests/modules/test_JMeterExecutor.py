""" test """
import json
import time
import os
import shutil
import yaml

from bzt.engine import Provisioning
from bzt.modules.jmeter import JMeterExecutor, JMX
from tests import setup_test_logging, BZTestCase, __dir__
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import bzt.utils

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

    def test_resource_files_collection(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/jmx/files.jmx"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 5)
        self.assertEqual(len(artifacts), 5)

    def test_resource_files_from_requests(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 1)
        self.assertEqual(len(artifacts), 1)

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
        self.assertEqual("60.0", shaper_coll_element.find(".//stringProp[@name='53']").text)


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

        self.assertEqual("1", shaper_coll_element.find(".//stringProp[@name='49']").text)
        self.assertEqual("100", shaper_coll_element.find(".//stringProp[@name='1567']").text)
        self.assertEqual("75.0", shaper_coll_element.find(".//stringProp[@name='53']").text)