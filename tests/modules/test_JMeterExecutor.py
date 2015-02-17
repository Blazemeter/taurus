""" test """
import hashlib
import json
import time
import os
import shutil

from bzt import BetterDict
from bzt.modules import Provisioning
from bzt.modules.jmeter import JMeterExecutor, JMX
from tests import setup_test_logging, BZTestCase, __dir__
from tests.mocks import EngineEmul


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
        obj.execution = {"scenario": {"script": "tests/jmx/broken.jmx"}}
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
        jstr = open("tests/json/get-post.json").read()
        obj.execution = json.loads(jstr)['execution']
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
        path = __dir__() + "/../../build/tmp/jmeter/bin/jmeter"
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        JMeterExecutor.JMETER_DOWNLOAD_LINK = "file://" + __dir__() + "/../data/jmeter-dist.zip"
        JMeterExecutor.PLUGINS_DOWNLOAD_TPL= "file://" + __dir__() + "/../data/jmeter-plugins-%s.zip"
        self.assertFalse(os.path.exists(path))
        obj = JMeterExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})

        obj.execution = {"scenario": {"requests": []}}

        obj.prepare()
        self.assertTrue(os.path.exists(path))

        obj.prepare()