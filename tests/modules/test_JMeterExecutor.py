""" test """
import hashlib
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

    def clean_har(self):
        def clean_cookies(visitor):
            if isinstance(visitor, dict):
                if "name" in visitor and visitor['name'] == 'Cookie':
                    visitor['value'] = hashlib.md5(visitor['value']).hexdigest()

                if "cookies" in visitor:
                    for c in visitor['cookies']:
                        c['value'] = hashlib.md5(c['value']).hexdigest()

        obj = json.loads(open("har/demo.har").read())
        BetterDict.traverse(obj, clean_cookies)
        open("har/demo1.har", 'w').write(json.dumps(obj, indent=True))

    def test_install_jmeter(self):

        bzt.utils.TEST_RUNNING = True

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

        bzt.utils.TEST_RUNNING = False

    def test_think_time_bug(self):
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = BetterDict()
        obj.engine.config.merge(yaml.load(open("tests/yaml/think-time-bug.yml").read()))
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        result = open(obj.modified_jmx).read()
        self.assertIn('<stringProp name="ConstantTimer.delay">750</stringProp>', result)
