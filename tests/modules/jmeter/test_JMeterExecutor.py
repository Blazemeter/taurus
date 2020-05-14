# coding=utf-8
import json
import os
import shutil
import time
from unittest import skipUnless, skipIf
from distutils.version import LooseVersion

import yaml

from bzt import ToolError, TaurusConfigError, TaurusInternalException
from bzt.jmx import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning
from bzt.modules.functional import FunctionalAggregator
from bzt.modules.jmeter import JTLReader, FuncJTLReader, JMeter
from bzt.modules.provisioning import Local
from bzt.six import etree, u
from bzt.utils import EXE_SUFFIX, get_full_path, BetterDict, is_windows, JavaVM
from tests import RESOURCES_DIR, BUILD_DIR, close_reader_file, ExecutorTestCase
from . import MockJMeterExecutor, MockHTTPClient

_jvm = JavaVM()
_jvm.check_if_installed()
java_version = _jvm.version
java10 = LooseVersion(java_version) >= LooseVersion("10")


class TestJMeterExecutor(ExecutorTestCase):
    EXECUTOR = MockJMeterExecutor

    def tearDown(self):
        if self.obj.modified_jmx and os.path.exists(self.obj.modified_jmx):
            os.remove(self.obj.modified_jmx)
        if self.obj.reader:
            if isinstance(self.obj.reader, FuncJTLReader):
                close_reader_file(self.obj.reader)
            if isinstance(self.obj.reader, JTLReader):
                close_reader_file(self.obj.reader.csvreader)
                close_reader_file(self.obj.reader.errors_reader)

        super(TestJMeterExecutor, self).tearDown()

    def configure(self, config):
        """
        Merge config into engine, setup provisioning,
        setup execution and settings attributes for executor.

        :return:
        """
        path = os.path.join(RESOURCES_DIR, "jmeter/jmeter-loader" + EXE_SUFFIX)
        self.obj.settings.merge({
            'path': path,
            'force-ctg': False,
            'protocol-handlers': {'http': 'bzt.jmx.http.HTTPProtocolHandler'}})

        super(TestJMeterExecutor, self).configure(config)
        self.obj.settings.merge(self.obj.engine.config.get('modules').get('jmeter'))

        prov = self.obj.engine.config.get('provisioning')
        if prov == 'local':
            self.obj.engine.provisioning = Local()
        elif prov == 'cloud':
            self.obj.engine.provisioning = CloudProvisioning()
        else:
            self.fail('Wrong provisioning value: %s' % prov)

    def test_jmx(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}})
        self.obj.prepare()

    def test_jmx_with_props(self):
        self.configure({"execution": {
            "concurrency": 10,
            "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/props_tg.jmx"}
        }})
        self.obj.prepare()

    def test_zero_concurrency(self):
        self.configure({"execution": {
            "concurrency": 60,
            "scenario": {
                "script": RESOURCES_DIR + "/jmeter/jmx/zero-concurrency.jmx"}
        }})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'
        selector += '>stringProp[name=ThreadGroup\.num_threads]'
        thr = jmx.get(selector)
        self.assertEqual(4, len(thr))   # tg with concurrency=0 must be disabled
        self.assertEqual('20', thr[0].text)    # 2 -> 20
        self.assertEqual("false", thr[1].getparent().attrib["enabled"]) # 0 -> disable tg
        self.assertEqual('10', thr[2].text)    # ${some_var} -> 1 -> 10
        self.assertEqual('30', thr[3].text)    # {__P(prop, 3)} -> 3 -> 30

    def test_jmx_2tg(self):
        self.configure({"execution": {
            "concurrency": 1051,
            "ramp-up": 15,
            "iterations": 100,
            "scenario": {
                "script": RESOURCES_DIR + "/jmeter/jmx/two_tg.jmx",
                "modifications": {
                    "disable": ["should_disable"]
                }
            }
        }})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'
        selector += '>stringProp[name=ThreadGroup\.num_threads]'
        thr = jmx.get(selector)
        self.assertEquals('420', thr[0].text)
        self.assertEquals('631', thr[1].text)

    def test_regexp_extractors(self):
        self.configure({"execution":
            {"scenario":
                {"requests": [{
                    "url": "http://localhost",
                    "extract-regexp": {
                        "varname": {
                            "regexp": "???",
                            "scope": "variable",
                            "from-variable": "RESULT"}
                    }}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertEqual("body", xml_tree.findall(".//stringProp[@name='RegexExtractor.useHeaders']")[0].text)
        self.assertEqual("???", xml_tree.findall(".//stringProp[@name='RegexExtractor.regex']")[0].text)
        self.assertEqual("variable", xml_tree.findall(".//stringProp[@name='Sample.scope']")[0].text)
        self.assertEqual("RESULT", xml_tree.findall(".//stringProp[@name='Scope.variable']")[0].text)

    def test_default_load(self):
        self.configure({"execution":
            {"scenario":
                {"requests": [{
                    "url": "http://localhost"}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertEqual("false", xml_tree.findall(".//boolProp[@name='ThreadGroup.scheduler']")[0].text)
        self.assertEqual("1", xml_tree.findall(".//stringProp[@name='LoopController.loops']")[0].text)

    def test_gen_load1(self):
        self.configure({
            "execution": {
                "hold-for": "5m",
                "scenario":
                    {"requests": [{"url": "http://localhost"}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertEqual("-1", xml_tree.findall(".//stringProp[@name='LoopController.loops']")[0].text)
        self.assertEqual("true", xml_tree.findall(".//boolProp[@name='ThreadGroup.scheduler']")[0].text)
        self.assertEqual("300", xml_tree.findall(".//stringProp[@name='ThreadGroup.duration']")[0].text)

    def test_gen_load2(self):
        self.configure({
            "execution": {
                "ramp-up": "3m",
                "scenario":
                    {"requests": [{"url": "http://localhost"}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertEqual("-1", xml_tree.findall(".//stringProp[@name='LoopController.loops']")[0].text)
        self.assertEqual("true", xml_tree.findall(".//boolProp[@name='ThreadGroup.scheduler']")[0].text)
        self.assertEqual("180", xml_tree.findall(".//stringProp[@name='ThreadGroup.ramp_time']")[0].text)
        self.assertEqual("180", xml_tree.findall(".//stringProp[@name='ThreadGroup.duration']")[0].text)

    def test_boundary_extractors(self):
        self.configure({"execution":
            {"scenario":
                {"requests": [{
                    "url": "http://localhost",
                    "extract-boundary": {
                        "varname": {
                            "left": "foo",
                            "right": "bar",
                            "scope": "variable",
                            "from-variable": "RESULT"}}}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertEqual("false", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.useHeaders']")[0].text)
        self.assertEqual("foo", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.lboundary']")[0].text)
        self.assertEqual("bar", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.rboundary']")[0].text)
        self.assertEqual("varname", xml_tree.findall(".//stringProp[@name='BoundaryExtractor.refname']")[0].text)
        self.assertEqual("variable", xml_tree.findall(".//stringProp[@name='Sample.scope']")[0].text)
        self.assertEqual("RESULT", xml_tree.findall(".//stringProp[@name='Scope.variable']")[0].text)

    def test_boundary_extractors_exc(self):
        self.configure({"execution":
            {"scenario":
                {"requests": [{
                    "url": "http://localhost",
                    "extract-boundary": {
                        "varname": {"left": "foo"}}}]}}})  # no "right"
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_not_jmx(self):
        self.obj.execution = {"scenario": {"script": __file__}}
        self.assertRaises(TaurusInternalException, self.obj.prepare)

    def test_broken_xml(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/broken.jmx"}})
        self.assertRaises(TaurusInternalException, self.obj.prepare)

    def test_not_jmx_xml(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/not-jmx.xml"}})
        self.assertRaises(TaurusInternalException, self.obj.prepare)

    def test_requests(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        self.obj.log.debug("%s: %s", self.obj.modified_jmx, open(self.obj.modified_jmx).read())
        self.obj.log.debug("%s", json.dumps(self.obj.execution, indent=True))
        try:
            self.obj.startup()
            while not self.obj.check():
                self.obj.log.debug("Check...")
                time.sleep(self.obj.engine.check_interval)
            self.obj.shutdown()
            self.obj.post_process()
        except:
            pass
        finally:
            if self.obj.jmeter_log and os.path.exists(self.obj.jmeter_log):
                self.obj.log.debug("%s", open(self.obj.jmeter_log).read())

    def test_issue_no_iterations(self):
        self.configure({"execution": {
            "concurrency": 10,
            "ramp-up": 10,
            "scenario": {
                "script": RESOURCES_DIR + "/jmeter/jmx/issue_no_iterations.jmx"
            }
        }})
        self.obj.prepare()

    def test_body_file(self):
        body_file0 = RESOURCES_DIR + "/jmeter/file-not-found"
        body_file1 = RESOURCES_DIR + "/jmeter/body-file.dat"
        body_file2 = RESOURCES_DIR + "/jmeter/jmx/http.jmx"
        self.configure({
            'execution': [{
                'iterations': 1,
                'scenario': 'bf'}],
            'scenarios': {
                'bf': {
                    "variables": {
                        "put_method": "put",
                        "J_VAR": "some_value"
                    },
                    "requests": [
                        {
                            'url': 'http://zero.com',
                            "method": "get",
                            'body-file': body_file0     # ignore because method is GET
                        }, {
                            'url': 'http://first.com',
                            "method": "${put_method}",
                            'body-file': body_file1     # handle as body-file
                        }, {
                            'url': 'http://second.com',
                            'method': 'post',
                            'body': 'body2',    # handle only 'body' as both are mentioned (body and body-file)
                            'body-file': body_file2
                        }, {
                            'url': 'https://the third.com',
                            'method': 'post',
                            'body-file': '${J_VAR}'     # write variable as body-file
                        }
                    ]}}})
        res_files = self.obj.get_resource_files()
        scenario = self.obj.get_scenario()
        body_files = [req.get('body-file') for req in scenario.get('requests')]
        body_fields = [req.get('body') for req in scenario.get('requests')]
        self.assertIn(body_file1, res_files)
        self.assertIn(body_file2, res_files)
        self.assertEqual(body_fields, [{}, {}, 'body2', {}])
        self.assertEqual(body_files, [body_file0, body_file1, body_file2, '${J_VAR}'])

        self.obj.prepare()

        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        elements = xml_tree.findall(".//HTTPSamplerProxy/elementProp[@name='HTTPsampler.Files']")
        self.assertEqual(2, len(elements))
        self.assertEqual(body_file1, elements[0].find(".//stringProp[@name='File.path']").text)
        self.assertIsNone(elements[0].find(".//stringProp[@name='File.paramname']").text)
        self.assertEqual("${J_VAR}", elements[1].find(".//stringProp[@name='File.path']").text)
        self.assertIsNone(elements[1].find(".//stringProp[@name='File.paramname']").text)

    def test_datasources_with_delimiter(self):
        self.configure({"execution": {
            "scenario": {
                "requests": ["http://localhost"],
                "data-sources": [{"path": RESOURCES_DIR + "test2.csv", "delimiter": ","}]}}})
        self.obj.prepare()

    def test_datasources_with_delimiter_tab(self):
        self.configure({"execution": {
            "scenario": {
                "requests": ["http://localhost"],
                "data-sources": [{"path": RESOURCES_DIR + "test2.csv", "delimiter": "tab"}]}}})
        self.obj.prepare()

        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        elements = xml_tree.findall(".//CSVDataSet[@testclass='CSVDataSet']")
        self.assertEqual(1, len(elements))
        element = elements[0]
        self.assertEqual("\t", element.find(".//stringProp[@name='delimiter']").text)

    def test_datasources_jmeter_var(self):
        self.configure({"execution": {
            "scenario": {
                "requests": ["http://localhost"],
                "data-sources": [{"path": "/before/${some_jmeter_variable}/after"}]}}})
        self.obj.prepare()

        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        elements = xml_tree.findall(".//CSVDataSet[@testclass='CSVDataSet']")
        self.assertEqual(1, len(elements))
        element = elements[0]
        self.assertEqual("/before/${some_jmeter_variable}/after", element.find(".//stringProp[@name='filename']").text)
        self.assertEqual(",", element.find(".//stringProp[@name='delimiter']").text)

    def test_datasources_wrong_path(self):
        self.obj.execution.merge({"scenario":
                                      {"requests": ["http://localhost"],
                                       "data-sources": [
                                           {"path": "really_wrong_path"}]}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_datasources_without_delimiter(self):
        self.configure({"execution": {
            "scenario": {
                "requests": ["http://localhost"],
                "data-sources": [{"path": RESOURCES_DIR + "test2.csv"}]}}})
        self.obj.prepare()

    def test_path_processing(self):
        class FakeTool(JMeter):
            tool_path = ''
            installed = None

            def set(self, tool_path, installed):
                self.tool_path = tool_path
                self.installed = installed

            def run_and_check(self):
                return self.installed

        fake = FakeTool()
        end_str = os.path.join('bin', 'jmeter' + EXE_SUFFIX)

        fake.set(__file__, True)  # real file, jmeter works: do nothing
        self.assertEqual(fake.check_if_installed(), True)

        fake.set(__file__, False)  # real file, jmeter doesn't work: raise
        with self.assertRaises(TaurusConfigError):
            fake.check_if_installed()

        fake.set(os.path.curdir, True)  # real dir, $dir/bin/jmeter.EXT works: fix path only
        self.assertEqual(fake.check_if_installed(), True)
        self.assertEqual(fake.tool_path, os.path.join(os.path.curdir, end_str))

        fake.set(os.path.curdir, False)  # real dir, $dir/bin/jmeter.EXT doesn't work: install into $dir
        self.assertEqual(fake.check_if_installed(), False)
        self.assertEqual(fake.tool_path, os.path.join(os.path.curdir, end_str))

        # not real file/dir, looks like *bin/jmeter.EXT: make two steps up, use as dir, install jmeter into it
        fake.set('*' + end_str, False)
        self.assertEqual(fake.check_if_installed(), False)
        self.assertEqual(fake.tool_path, '*' + end_str)

        # not real file/dir, doesn't look like *bin/jmeter.EXT: use as dir, install jmeter into it
        fake.set('*', False)
        self.assertEqual(fake.check_if_installed(), False)
        self.assertEqual(fake.tool_path, os.path.join('*', end_str))

    @skipIf(java10, "Disabled on Java 10")
    def test_install_jmeter_3_0(self):
        path = os.path.abspath(BUILD_DIR + "jmeter-taurus/bin/jmeter" + EXE_SUFFIX)
        self.obj.mock_install = False

        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        self.assertFalse(os.path.exists(path))

        jmeter_res_dir = RESOURCES_DIR + "/jmeter/"
        http_client = MockHTTPClient()
        http_client.add_response('GET', 'https://jmeter.apache.org/download_jmeter.cgi',
                                 file=jmeter_res_dir + "unicode_file")
        http_client.add_response('GET', 'https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-3.0.zip',
                                 file=jmeter_res_dir + "jmeter-dist-3.0.zip")
        url = 'https://search.maven.org/remotecontent?filepath=kg/apc/jmeter-plugins-manager/' \
              '{v}/jmeter-plugins-manager-{v}.jar'.format(v=JMeter.PLUGINS_MANAGER_VERSION)

        http_client.add_response('GET', url, file=jmeter_res_dir + "jmeter-plugins-manager.jar")
        http_client.add_response('GET',
                                 'https://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/2.2/cmdrunner-2.2.jar',
                                 file=jmeter_res_dir + "jmeter-plugins-manager.jar")

        self.obj.engine.get_http_client = lambda: http_client
        jmeter_ver = JMeter.VERSION
        try:
            JMeter.VERSION = '3.0'

            self.configure({
                "execution": [{"scenario": {"requests": ["http://localhost"]}}],
                "settings": {
                    "proxy": {
                        "address": "http://myproxy.com:8080",
                        "username": "user",
                        "password": "pass"}}})
            self.obj.settings.merge({"path": path})
            self.obj.prepare()
            jars = os.listdir(os.path.abspath(os.path.join(path, '../../lib')))
            self.assertNotIn('httpclient-4.5.jar', jars)
            self.assertIn('httpclient-4.5.2.jar', jars)

            self.assertTrue(os.path.exists(path))

            # start again..
            self.tearDown()
            self.setUp()

            self.configure({"execution": {"scenario": {"requests": ["http://localhost"]}}})
            self.obj.settings.merge({"path": path})

            self.obj.prepare()
        finally:
            JMeter.VERSION = jmeter_ver

    @skipIf(java10, "Disabled on Java 10")
    def test_install_jmeter_2_13(self):
        path = os.path.abspath(BUILD_DIR + "jmeter-taurus/bin/jmeter" + EXE_SUFFIX)
        self.obj.mock_install = False

        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        self.assertFalse(os.path.exists(path))

        jmeter_res_dir = RESOURCES_DIR + "/jmeter/"
        http_client = MockHTTPClient()
        http_client.add_response('GET', 'https://jmeter.apache.org/download_jmeter.cgi',
                                 file=jmeter_res_dir + "unicode_file")
        http_client.add_response('GET', 'https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-2.13.zip',
                                 file=jmeter_res_dir + "jmeter-dist-2.13.zip")
        url = 'https://search.maven.org/remotecontent?filepath=kg/apc/jmeter-plugins-manager/' \
              '{v}/jmeter-plugins-manager-{v}.jar'.format(v=JMeter.PLUGINS_MANAGER_VERSION)
        http_client.add_response('GET', url, file=jmeter_res_dir + "jmeter-plugins-manager.jar")
        http_client.add_response('GET',
                                 'https://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/2.2/cmdrunner-2.2.jar',
                                 file=jmeter_res_dir + "jmeter-plugins-manager.jar")

        jmeter_ver = JMeter.VERSION
        self.obj.engine.get_http_client = lambda: http_client
        try:
            JMeter.VERSION = '2.13'

            self.configure({
                "execution": [{"scenario": {"requests": ["http://localhost"]}}],
                "settings": {
                    "proxy": {
                        "address": "http://myproxy.com:8080",
                        "username": "user",
                        "password": "pass"}}})
            self.obj.settings.merge({"path": path})
            self.obj.prepare()
            jars = os.listdir(os.path.abspath(os.path.join(path, '../../lib')))
            old_jars = [
                'httpcore-4.2.5.jar', 'httpmime-4.2.6.jar', 'xercesImpl-2.9.1.jar',
                'commons-jexl-1.1.jar', 'httpclient-4.2.6.jar']
            for old_jar in old_jars:
                self.assertNotIn(old_jar, jars)

            self.assertTrue(os.path.exists(path))

            # start again..
            self.tearDown()
            self.setUp()

            self.configure({"execution": {"scenario": {"requests": ["http://localhost"]}}})
            self.obj.settings.merge({"path": path})

            self.obj.prepare()
        finally:
            JMeter.VERSION = jmeter_ver

    def test_install_disabled(self):
        path = os.path.abspath(BUILD_DIR + "jmeter-taurus/bin/jmeter" + EXE_SUFFIX)
        self.obj.mock_install = False

        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        self.assertFalse(os.path.exists(path))
        try:
            os.environ["TAURUS_DISABLE_DOWNLOADS"] = "true"
            self.configure({"execution": [{"scenario": {"requests": ["http://localhost"]}}], })
            self.obj.settings.merge({"path": path})
            self.assertRaises(TaurusInternalException, self.obj.prepare)
        finally:
            os.environ["TAURUS_DISABLE_DOWNLOADS"] = ""

    def test_timers(self):
        with open(os.path.join(RESOURCES_DIR, "yaml/timers.yml")) as config_file:
            config = yaml.full_load(config_file.read())

        self.configure(config)
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        constant_timer = jmx.tree.find(".//ConstantTimer[@testname='Think-Time']")
        constant_timer.find(".//stringProp[@name='CookieManager.implementation']")
        uniform_timer = jmx.tree.find(".//UniformRandomTimer[@testname='Think-Time']")
        gaussian_timer = jmx.tree.find(".//GaussianRandomTimer[@testname='Think-Time']")
        poisson_timer = jmx.tree.find(".//PoissonRandomTimer[@testname='Think-Time']")
        
        ctd = "ConstantTimer.delay"
        rtr = "RandomTimer.range"
        str_prop = ".//stringProp[@name='%s']"
        
        const_delay = constant_timer.find(str_prop % ctd).text
        self.assertEqual("750", const_delay)

        uniform_delay = uniform_timer.find(str_prop % ctd).text
        uniform_range = uniform_timer.find(str_prop % rtr).text
        self.assertEqual("55000", uniform_delay)
        self.assertEqual("10000", uniform_range)

        gaussian_delay = gaussian_timer.find(str_prop % ctd).text
        gaussian_range = gaussian_timer.find(str_prop % rtr).text
        self.assertEqual("11000", gaussian_delay)
        self.assertEqual("500", gaussian_range)

        poisson_delay = poisson_timer.find(str_prop % ctd).text
        poisson_range = poisson_timer.find(str_prop % rtr).text
        self.assertEqual("30000", poisson_delay)
        self.assertEqual("90000", poisson_range)

    def test_think_time_bug(self):
        self.configure({
            'execution': {
                'ramp-up': '1m',
                'hold-for': '1m30s',
                'concurrency': 10,
                'scenario': {
                    'think-time': 0.75,
                    'requests': [
                        'http://blazedemo.com/',
                        'http://blazedemo.com/vacation.html']}}})
        self.obj.prepare()
        result = open(self.obj.modified_jmx).read()
        self.assertIn('<stringProp name="ConstantTimer.delay">750</stringProp>', result)

    def test_cookiemanager_3_2_bug_requests(self):
        """ specify implementation of CookieManager for case of generation from requests """
        self.configure({
            'execution': {
                'hold-for': '1s',
                'concurrency': 10,
                'scenario': {
                    'requests': [
                        'http://blazedemo.com/']}}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        resource_elements = jmx.tree.findall(".//stringProp[@name='CookieManager.implementation']")
        self.assertEqual(1, len(resource_elements))
        new_implementation = "org.apache.jmeter.protocol.http.control.HC4CookieHandler"
        self.assertEqual(resource_elements[0].text, new_implementation)

    def test_cookiemanager_3_2_bug_jmx(self):
        """ specify implementation of CookieManager for existing jmx """
        self.configure({
            'execution': {
                'hold-for': '1s',
                'concurrency': 10,
                'scenario': {
                    'script': RESOURCES_DIR + '/jmeter/jmx/cookiemanagers.jmx'}}})
        self.obj.prepare()
        orig_jmx = JMX(self.obj.original_jmx)
        mod_jmx = JMX(self.obj.modified_jmx)
        orig_elements = orig_jmx.tree.findall(".//stringProp[@name='CookieManager.implementation']")
        mod_elements = mod_jmx.tree.findall(".//stringProp[@name='CookieManager.implementation']")
        self.assertEqual(0, len(orig_elements))
        self.assertEqual(2, len(mod_elements))
        new_implementation = "org.apache.jmeter.protocol.http.control.HC4CookieHandler"
        self.assertTrue(all(re.text == new_implementation for re in mod_elements))

    def test_body_parse(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        sampler_element = xml_tree.findall(".//HTTPSamplerProxy[@testname='With body params']")
        arguments_element_prop = sampler_element[0][0]
        self.assertEqual(11, len(sampler_element[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop.getchildren()))
        self.assertEqual(2, len(arguments_element_prop[0].getchildren()))
        self.assertEqual(1, len(arguments_element_prop[0].findall(".//elementProp[@name='param1']")))
        self.assertEqual(1, len(arguments_element_prop.findall(".//elementProp[@name='param2']")))

    def test_resource_files_collection_remote_prov(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/files.jmx"}})
        self.assertNotIn('files', self.obj.execution)
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)
        self.assertIn('files', self.obj.execution)
        self.assertEqual(4, len(self.obj.execution['files']))

    def test_resource_files_paths(self):
        """
        Check whether JMeter.resource_files() modifies filenames in JMX carefully
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/files_paths.jmx"}})

        file_in_home = get_full_path('~/file-in-home.csv')
        file_was_created = False
        if not os.path.exists(file_in_home):
            file_was_created = True
            with open(file_in_home, 'w') as _file:  # real file is required by Engine.find_file()
                _file.write('')
        self.obj.engine.file_search_paths = ['tests']  # config not in cwd
        self.obj.resource_files()
        if file_was_created:
            os.remove(file_in_home)

        resource_files = []
        jmx = JMX(self.obj.original_jmx)
        resource_elements = jmx.tree.findall(".//stringProp[@name='filename']")
        for resource_element in resource_elements:
            if resource_element.text:
                resource_files.append(resource_element.text)
        self.assertEqual(2, len(resource_files))
        for res_file in resource_files:
            self.assertEqual(res_file, os.path.basename(res_file))

    def test_resource_files_from_requests_remote_prov(self):
        config = json.loads(open(RESOURCES_DIR + "json/get-post.json").read())
        config['provisioning'] = 'cloud'
        self.configure(config)
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 3)
        self.assertEqual(len(set(res_files)), 2)

    def test_resource_files_from_requests_local_prov(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        files = ['jmeter-bzt.properties', 'modified_requests.jmx']
        files += ['requests.jmx', 'system.properties']
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertTrue(all([_file in artifacts for _file in files]))

    def test_resource_files_data_sources_shorthand(self):
        csv_file = RESOURCES_DIR + 'test1.csv'
        csv_file_uni = u(RESOURCES_DIR + 'test2.csv')
        self.configure({
            'execution': {
                'scenario': {
                    'data-sources': [csv_file, csv_file_uni]}}})
        resource_files = self.obj.resource_files()
        self.assertIn(csv_file, resource_files)
        self.assertIn(csv_file_uni, resource_files)

    def test_resource_files_data_sources_full_form(self):
        csv_file = RESOURCES_DIR + 'test1.csv'
        csv_file_uni = u(RESOURCES_DIR + 'test2.csv')
        self.configure({
            'execution': {
                'scenario': {
                    'data-sources': [{
                        'path': csv_file,
                        'loop': False,
                        'quoted': True,
                    }, {
                        'path': csv_file_uni,
                        'loop': False,
                        'quoted': True}]}}})
        resource_files = self.obj.resource_files()
        self.assertIn(csv_file, resource_files)
        self.assertIn(csv_file_uni, resource_files)

    def test_resource_files_jsr223(self):
        js_file = RESOURCES_DIR + 'data.js'
        self.configure({
            'execution': {
                'scenario': {
                    'requests': [{
                        'url': 'http://blazedemo.com/',
                        'jsr223': {
                            'language': 'javascript',
                            'script-file': js_file,
                        }}]}}})
        resource_files = self.obj.resource_files()
        self.assertIn(js_file, resource_files)

    def test_resource_files_jsr223s(self):
        js_file = RESOURCES_DIR + 'data.js'
        js_file2 = RESOURCES_DIR + 'data2.js'
        self.configure({
            'execution': {
                'scenario': {
                    'requests': [{
                        'url': 'http://blazedemo.com/',
                        'jsr223': [{
                            'language': 'javascript',
                            'script-file': js_file,
                        }, {
                            'language': 'javascript',
                            'script-file': js_file2,
                        }]}]}}})
        resource_files = self.obj.resource_files()
        self.assertEqual(2, len(resource_files))
        self.assertIn(js_file, resource_files)
        self.assertIn(js_file2, resource_files)

    def test_http_request_defaults(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
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

    def test_http_request_defaults_property(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))

        addr = 'https://${__P(hostname)}:${__P(port)}'
        self.obj.engine.config.merge({'scenarios': {'get-post': {'default-address': addr}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        default_elements = xml_tree.findall(".//ConfigTestElement[@testclass='ConfigTestElement']")
        self.assertEqual(1, len(default_elements))

        default_element = default_elements[0]
        self.assertEqual("${__P(hostname)}", default_element.find(".//stringProp[@name='HTTPSampler.domain']").text)
        self.assertEqual("${__P(port)}", default_element.find(".//stringProp[@name='HTTPSampler.port']").text)
        self.assertEqual("https", default_element.find(".//stringProp[@name='HTTPSampler.protocol']").text)

    def test_add_shaper_constant(self):
        self.configure({'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m',
                                      'scenario': {'script': RESOURCES_DIR + '/jmeter/jmx/http.jmx'}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        timer_ = ".//%s[@testclass='%s']" % (JMX.THR_TIMER, JMX.THR_TIMER)
        shaper_elements = xml_tree.findall(timer_)
        self.assertEqual(1, len(shaper_elements))

        shaper_collection = shaper_elements[0].find(".//collectionProp[@name='load_profile']")
        coll_elements = shaper_collection.findall(".//collectionProp")

        self.assertEqual(1, len(coll_elements))

        val_strings = coll_elements[0].findall(".//stringProp")

        self.assertEqual("100.0", val_strings[0].text)
        self.assertEqual("100.0", val_strings[1].text)
        self.assertEqual("60", val_strings[2].text)

    def test_default_iteartions(self):
        self.configure({'execution': {
            'concurrency': 1,
            'scenario': {
                'requests': ['http://blazedemo.com']}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())

        path = './/stringProp[@name="LoopController.loops"]'
        iterations = int(xml_tree.find(path).text)
        self.assertEqual(1, iterations)

    def test_add_cookies(self):
        self.configure({'execution': {
            'scenario': {
                'cookies': [
                    {"name": "n0", "value": "v0", "domain": "blazedemo.com"},
                    {"name": "n1", "value": "v1", "domain": "blazemeter.com", "path": "/pricing", "secure": "true"}],
                'requests': ['http://blazedemo.com', "http://blazemeter.com/pricing"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())

        cookes_query = './/collectionProp[@name="CookieManager.cookies"]/elementProp'
        cookies = xml_tree.findall(cookes_query)

        self.assertEqual(2, len(cookies))
        self.assertEqual('n0', cookies[0].attrib['name'])
        self.assertEqual('v0', cookies[0].find('.//stringProp[@name="Cookie.value"]').text)
        self.assertEqual('blazedemo.com', cookies[0].find('.//stringProp[@name="Cookie.domain"]').text)
        self.assertIn(cookies[0].find('.//stringProp[@name="Cookie.path"]').text, ['', None])
        self.assertEqual('false', cookies[0].find('.//boolProp[@name="Cookie.secure"]').text)
        self.assertEqual('0', cookies[0].find('.//longProp[@name="Cookie.expires"]').text)
        self.assertEqual('true', cookies[0].find('.//boolProp[@name="Cookie.path_specified"]').text)
        self.assertEqual('true', cookies[0].find('.//boolProp[@name="Cookie.domain_specified"]').text)
        self.assertEqual('n1', cookies[1].attrib['name'])
        self.assertEqual('v1', cookies[1].find('.//stringProp[@name="Cookie.value"]').text)
        self.assertEqual('blazemeter.com', cookies[1].find('.//stringProp[@name="Cookie.domain"]').text)
        self.assertEqual('/pricing', cookies[1].find('.//stringProp[@name="Cookie.path"]').text)
        self.assertEqual('true', cookies[1].find('.//boolProp[@name="Cookie.secure"]').text)
        self.assertEqual('0', cookies[1].find('.//longProp[@name="Cookie.expires"]').text)
        self.assertEqual('true', cookies[1].find('.//boolProp[@name="Cookie.path_specified"]').text)
        self.assertEqual('true', cookies[1].find('.//boolProp[@name="Cookie.domain_specified"]').text)

    def test_add_shaper_ramp_up(self):
        self.configure(
            {'execution': {'ramp-up': '1m', 'throughput': 9, 'hold-for': '2m', 'concurrency': 20,
                           'scenario': {'script': RESOURCES_DIR + '/jmeter/jmx/http.jmx'}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        timer_ = ".//%s[@testclass='%s']" % (JMX.THR_TIMER, JMX.THR_TIMER)
        shaper_elements = xml_tree.findall(timer_)
        self.assertEqual(1, len(shaper_elements))
        shaper_collection = shaper_elements[0].find(".//collectionProp[@name='load_profile']")
        coll_elements = shaper_collection.findall(".//collectionProp")

        self.assertEqual(2, len(coll_elements))

        val_strings = coll_elements[0].findall(".//stringProp")

        self.assertEqual("0.05", val_strings[0].text)
        self.assertEqual("9.0", val_strings[1].text)
        self.assertEqual("60", val_strings[2].text)

        val_strings = coll_elements[1].findall(".//stringProp")

        self.assertEqual("9.0", val_strings[0].text)
        self.assertEqual("9.0", val_strings[1].text)
        self.assertEqual("120", val_strings[2].text)

    def test_user_def_vars_from_requests(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_get_classpath_from_files(self):
        res_dir = RESOURCES_DIR + ''
        self.configure(
            {'execution': {
                'concurrency': 200,
                'hold-for': '1m',
                'files': [
                    res_dir + 'selenium/junit/jar',
                    res_dir + 'selenium/testng/jars/test-suite.jar'],
                'scenario': {
                    'properties': {'one': 'two', 'user.classpath': 'user_class_path'},
                    'script': res_dir + 'jmeter/jmx/http.jmx'}}})
        self.obj.prepare()

        prop_file_path = os.path.join(self.obj.engine.artifacts_dir, "jmeter-bzt.properties")
        self.assertTrue(os.path.exists(prop_file_path))
        with open(prop_file_path) as prop_file:
            contents = prop_file.readlines()
        cp_lines = [line for line in contents if line.startswith('user.classpath')]
        self.assertEqual(len(cp_lines), 1)
        user_cp = cp_lines[0][len('user.classpath='): -1]
        user_cp_elements = user_cp.split(os.pathsep)
        self.assertEqual(5, len(user_cp_elements))
        targets = [self.obj.engine.artifacts_dir, get_full_path(self.obj.modified_jmx, step_up=1),
                   self.obj.execution['files'][0],
                   get_full_path(self.obj.execution['files'][1], step_up=1)]
        targets = [get_full_path(target).replace(os.path.sep, '/') for target in targets]
        targets.append(self.obj.get_scenario()['properties']['user.classpath'])

        self.assertEqual(set(targets), set(user_cp_elements))

    def test_user_def_vars_override(self):
        self.configure(
            {'execution': {'concurrency': 200, 'throughput': 100, 'hold-for': '1m', 'scenario': {
                'variables': {'my_var': 'http://demo.blazemeter.com/api/user', 'myvar2': 'val2'},
                'properties': {'log_level.jmeter': 'DEBUG'},
                'script': RESOURCES_DIR + '/jmeter/jmx/http.jmx'}}})
        self.obj.prepare()

        # no new properties in scenario properties list
        self.assertEqual(1, len(self.obj.engine.config['scenarios']['http.jmx']['properties']))

        # no properties in module properties list
        self.assertEqual(0, len(self.obj.settings.get('properties')))

        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        udv_elements = xml_tree.findall(".//Arguments[@testclass='Arguments']")
        self.assertEqual(1, len(udv_elements))

    def test_distributed_th_hostnames(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/http.jmx"}})
        self.obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        for writer in writers:
            self.assertEqual('true', writer.find('objProp/value/hostname').text)

    def test_distributed_props(self):
        self.sniff_log(self.obj.log)

        self.configure({"execution":{"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/http.jmx"}}})
        self.obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        self.obj.settings['properties'] = BetterDict.from_dict({"a": 1})

        self.obj.prepare()
        self.obj.startup()

        self.assertIn("', '-G', '", self.log_recorder.debug_buff.getvalue())

    def test_distributed_th_hostnames_complex(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.distributed_servers = ["127.0.0.1", "127.0.0.1"]
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        for writer in writers:
            self.assertEqual('true', writer.find('objProp/value/hostname').text)

    def test_dns_cache_mgr_scenario(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/http.jmx"}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        dns_element = xml_tree.findall(".//DNSCacheManager")
        # no dns manager when using jmx, no system.properties file
        self.assertEqual(len(dns_element), 0)
        arts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertNotIn("system.properties", arts)

    def test_dns_cache_mgr_requests(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
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
        self.configure({
            'execution': {
                'ramp-up': 10,
                'throughput': 2,
                'hold-for': 20,
                'concurrency': 5,
                'scenario': {
                    'think-time': '0.75s',
                    'script': RESOURCES_DIR + '/jmeter/jmx/http.jmx'}},
            'modules': {
                'jmeter': {
                    'system-properties': {'any_prop': 'true'},
                    'properties': {
                        'log_level.jmeter': 'WARN',
                        'log_level.jmeter.threads': 'DEBUG',
                        'my-hostname': 'www.pre-test.com'}}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        dns_managers = xml_tree.findall(".//DNSCacheManager")
        # 0 dns_managers
        self.assertEqual(len(dns_managers), 0)
        sys_prop = open(os.path.join(self.obj.engine.artifacts_dir, "system.properties")).read()
        self.assertTrue("any_prop=true" in sys_prop)
        self.assertFalse("sun.net.inetaddr.ttl=0" in sys_prop)

    def test_jpgc_props(self):
        self.configure({
            'execution': {
                'hold-for': 2,
                'concurrency': 1,
                'scenario': {
                    'script': RESOURCES_DIR + '/jmeter/jmx/http.jmx',
                    "properties": {"jpgc.3": "scenario_prop"}}},
            'modules': {
                'jmeter': {
                    'system-properties': {'sys0': 'val_s0', 'jpgc.1': 'val_s1', 'jpgc.2': 'val_s2'},
                    'properties': {'bzt0': 'val_p0', 'jpgc.2': 'val_p1'}}}})

        self.obj.env.set({"JVM_ARGS": "-Dsettings_env=val_set_env"})

        self.obj.prepare()
        jvm_args = self.obj.tool.env.get('JVM_ARGS')

        for val in ("sys0", "val_s0", "val_s2", "bzt0", "val_p0"):
            self.assertNotIn(val, jvm_args)

        for val in ("jpgc.1", "val_s1", "jpgc.2", "val_p1", "settings_env", "val_set_env", "jpgc.3", "scenario_prop"):
            self.assertIn(val, jvm_args)

    def test_stepping_tg_ramp_no_proportion(self):
        self.configure({
            'execution': {
                'steps': 5,
                'concurrency': 170,
                'scenario': {
                    'script': RESOURCES_DIR + '/jmeter/jmx/stepping_ramp_up.jmx'},
                'ramp-up': '1m',
                'distributed': ['127.0.0.1'],
                'hold-for': '2m'}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        mod_stepping_tgs = modified_xml_tree.findall(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        self.assertEqual(0, len(mod_stepping_tgs))  # generation of STG is obsolete

    def test_duration_loops_bug(self):
        self.configure({
            "execution": {
                "concurrency": 10,
                "ramp-up": 15,
                "hold-for": "2m",
                "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/http.jmx"}}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//stringProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "-1")
        self.assertEqual(tg_forever.text, "false")

    def test_force_delimiters(self):
        self.obj.execution.merge({
            "iterations": 10,
            "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/delimiters.jmx"}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        delimiters = [delimiter.text for delimiter in jmx.get("CSVDataSet>stringProp[name='delimiter']")]
        self.assertEqual(['1', '2', ','], delimiters)

    def test_iterations_loop_bug(self):
        self.obj.execution.merge({
            "iterations": 10,
            "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/http.jmx"}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
        tg_loops = loop_ctrl.find(".//stringProp[@name='LoopController.loops']")
        tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(tg_loops.text, "10")
        self.assertEqual(tg_forever.text, "false")

        self.tearDown()
        self.setUp()

        script_path = RESOURCES_DIR + "/jmeter/jmx/http.jmx"
        self.obj.execution.merge({"scenario": {"script": script_path}})
        try:
            self.obj.prepare()
            modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
            tg = modified_xml_tree.find(".//ThreadGroup")
            loop_ctrl = tg.find(".//elementProp[@name='ThreadGroup.main_controller']")
            tg_loops = loop_ctrl.find("*[@name='LoopController.loops']")
            tg_forever = loop_ctrl.find(".//boolProp[@name='LoopController.continue_forever']")
            self.assertEqual(tg_loops.text, "1")  # default value, not disabled
            self.assertEqual(tg_forever.text, "false")
        finally:
            self.obj.post_process()

    def test_load_overriding(self):
        self.obj.execution.merge({
            "iterations": 2,
            "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/http.jmx"}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        tg = modified_xml_tree.find(".//ThreadGroup")
        ramp_up = tg.find(".//stringProp[@name='ThreadGroup.ramp_time']")
        conc = tg.find(".//stringProp[@name='ThreadGroup.num_threads']")
        delay = tg.find(".//boolProp[@name='ThreadGroup.delayedStart']")
        self.assertEqual(conc.text, '${__P(val_c)}')    # concurrency should be saved
        self.assertEqual(ramp_up.text, '0')             # ramp-up should be removed

        delay = delay is not None and delay.text == "true"
        self.assertTrue(delay)

        self.obj.post_process()

    def test_distributed_gui(self):
        self.configure(yaml.full_load(open(RESOURCES_DIR + "yaml/distributed_gui.yml").read()))
        self.obj.prepare()

        prop_file_path = os.path.join(self.obj.engine.artifacts_dir, "jmeter-bzt.properties")
        self.assertTrue(os.path.exists(prop_file_path))
        with open(prop_file_path) as prop_file:
            contents = prop_file.read()
        self.assertIn("remote_hosts=127.0.0.1,127.0.0.2", contents)

    def test_empty_requests(self):
        # https://groups.google.com/forum/#!topic/codename-taurus/iaT6O2UhfBE
        self.configure({
            'execution': {
                'ramp-up': '10s',
                'requests': ['http://blazedemo.com/',
                             'http://blazedemo.com/vacation.html'],
                'hold-for': '30s',
                'concurrency': 5,
                'scenario': {'think-time': 0.75}}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_wrong_loop(self):
        # https://groups.google.com/forum/#!topic/codename-taurus/iaT6O2UhfBE
        self.configure({
            'execution': {
                'scenario': {
                    'requests': [{"loop": "forever", "do": "bla-bla"}]}}})

        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_variable_csv_file(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "/jmeter/jmx/variable_csv.jmx"}})
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        info = "\n artifacts_dir1: %s" % self.obj.engine.artifacts_dir + "\nartifacts1: %s" % artifacts
        self.obj.prepare()
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        info += "\n artifacts_dir2: %s" % self.obj.engine.artifacts_dir + "\n artifacts2: %s" % artifacts
        self.assertEqual(len(artifacts), 5, "find extra ones: %s" % info)  # 2*effective, .properties, .out, .err
        with open(self.obj.modified_jmx) as fds:
            jmx = fds.read()
            self.assertIn('<stringProp name="filename">${root}/csvfile.csv</stringProp>', jmx)

    def test_css_jquery_extractor(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        target_jmx = os.path.join(self.obj.engine.artifacts_dir, "requests.jmx")
        modified_xml_tree = etree.fromstring(open(target_jmx, "rb").read())
        jq_css_extractors = modified_xml_tree.findall(".//HtmlExtractor")
        self.assertEqual(2, len(jq_css_extractors))
        simply_form = modified_xml_tree.find(".//HtmlExtractor[@testname='Get name1']")
        self.assertEqual(simply_form.find(".//stringProp[@name='HtmlExtractor.refname']").text, "name1")
        self.assertEqual(simply_form.find(".//stringProp[@name='HtmlExtractor.expr']").text,
                         "input[name~=my_input]")
        self.assertEqual(simply_form.find(".//stringProp[@name='HtmlExtractor.attribute']").text, None)
        self.assertEqual(simply_form.find(".//stringProp[@name='HtmlExtractor.match_number']").text, "0")
        self.assertEqual(simply_form.find(".//stringProp[@name='HtmlExtractor.default']").text, "NOT_FOUND")

        full_form = modified_xml_tree.find(".//HtmlExtractor[@testname='Get name2']")
        self.assertEqual(full_form.find(".//stringProp[@name='HtmlExtractor.refname']").text, "name2")
        self.assertEqual(full_form.find(".//stringProp[@name='HtmlExtractor.expr']").text, "input[name=JMeter]")
        self.assertEqual(full_form.find(".//stringProp[@name='HtmlExtractor.attribute']").text, "value")
        self.assertEqual(full_form.find(".//stringProp[@name='HtmlExtractor.match_number']").text, "1")
        self.assertEqual(full_form.find(".//stringProp[@name='HtmlExtractor.default']").text, "NV_JMETER")
        self.assertEqual("variable", full_form.find(".//stringProp[@name='Sample.scope']").text)
        self.assertEqual("CSS_RESULT", full_form.find(".//stringProp[@name='Scope.variable']").text)

    def test_xpath_extractor(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
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
        self.assertEqual("variable", full_form.find(".//stringProp[@name='Sample.scope']").text)
        self.assertEqual("XPATH_RESULT", full_form.find(".//stringProp[@name='Scope.variable']").text)

    def test_xpath_assertion(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
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

    def test_jsonpath_assertion(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        target_jmx = os.path.join(self.obj.engine.artifacts_dir, "requests.jmx")
        modified_xml_tree = etree.fromstring(open(target_jmx, "rb").read())
        path = ".//com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion.JSONPathAssertion"
        assertions = modified_xml_tree.findall(path)
        self.assertEqual(4, len(assertions))

        vals = [
            {'path': '$.', 'exp_val': None, 'valid': 'false',
             'null': 'false', 'invert': 'false', 'regexp': 'true'},
            {'path': '$.res[0].type', 'exp_val': 'some_value.1', 'valid': 'true',
             'null': 'false', 'invert': 'false', 'regexp': 'true'},
            {'path': '$.res[1].ip', 'exp_val': 'some_value.2', 'valid': 'true',
             'null': 'false', 'invert': 'true', 'regexp': 'false'},
            {'path': '$.res[2].default', 'exp_val': None, 'valid': 'false',
             'null': 'true', 'invert': 'false', 'regexp': 'true'}]
        for num in range(len(assertions)):
            assertion = assertions[num]
            val = vals[num]
            self.assertEqual(val['path'], assertion.find(".//stringProp[@name='JSON_PATH']").text)
            self.assertEqual(val['exp_val'], assertion.find(".//stringProp[@name='EXPECTED_VALUE']").text)
            self.assertEqual(val['valid'], assertion.find(".//boolProp[@name='JSONVALIDATION']").text)
            self.assertEqual(val['null'], assertion.find(".//boolProp[@name='EXPECT_NULL']").text)
            self.assertEqual(val['invert'], assertion.find(".//boolProp[@name='INVERT']").text)
            self.assertEqual(val['regexp'], assertion.find(".//boolProp[@name='ISREGEX']").text)

    def test_shutdown_soft(self):
        self.sniff_log(self.obj.log)
        self.configure({"execution": {"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}}})
        self.obj.prepare()
        self.obj.env.set({'TEST_MODE': 'server'})
        self.obj.startup()
        time.sleep(1)
        self.obj.management_port = 8089
        self.obj.shutdown()

        self.assertIn("JMeter stopped on Shutdown command", self.log_recorder.debug_buff.getvalue())

    def test_fail_on_zero_results(self):
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.configure({"execution": {"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}}})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()

        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        prov.started_modules = [self.obj]
        self.obj.engine.provisioning = prov
        self.assertRaises(ToolError, self.obj.engine.provisioning.post_process)

    def test_ok_with_results(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}})
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        self.obj.engine.provisioning = prov
        self.obj.reader.read_records = 13
        self.obj.engine.provisioning.post_process()

    def test_convert_tgroups_no_load(self):
        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/SteppingThreadGroup.jmx"}})
        self.obj.prepare()
        modified_xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        st_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.SteppingThreadGroup")
        self.assertNotEqual(st_tg, None)
        ul_tg = modified_xml_tree.find(".//kg.apc.jmeter.threads.UltimateThreadGroup")
        self.assertNotEqual(ul_tg, None)

    def test_convert_tgroups_load_modifications(self):
        self.obj.execution.merge({
            "iterations": 20,
            "ramp-up": 10,
            "hold-for": "2m",
            "scenario": {"script": RESOURCES_DIR + "/jmeter/jmx/SteppingThreadGroup.jmx"}})
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
        s_t = ProtocolHandler.safe_time
        self.assertEqual(s_t('1m'), 60 * 1000.0)
        self.assertEqual(s_t('${VAR}'), '${VAR}')

    def test_json_body_app_str(self):
        self.configure({"execution": {
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": "{\"store_id\": \"${store_id}\", \"display_name\": \"${display_name}\"}"}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        res = jmx.get(selector)[0].text
        self.assertNotEqual(res.find('store_id'), -1)

    def test_json_body_app_dic(self):
        self.configure({"execution": {
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": {
                        "store_id": "${store_id}",
                        "display_name": "${display_name}"}}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        res = jmx.get(selector)[0].text
        self.assertNotEqual(res.find('store_id'), -1)
        self.assertTrue(isinstance(json.loads(res), dict))

    def test_json_body_app_list(self):
        self.configure({"execution": {
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/json"},
                    "body": [
                        {"store_id": "${store_id}"},
                        {"display_name": "${display_name}"}]}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        res = jmx.get(selector)[0].text
        self.assertNotEqual(res.find('store_id'), -1)
        self.assertTrue(isinstance(json.loads(res), list))

    def test_json_body_requires_header(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "body": {
                        "structure": {
                            "one": 2,
                            "two": "1"}}}]}})
        self.assertRaises(TaurusInternalException, self.obj.prepare)
        jmx = JMX(self.obj.original_jmx)
        selector = 'stringProp[name="Argument.value"]'
        self.assertTrue(all(not jprop.text.startswith('defaultdict') for jprop in jmx.get(selector)))

    def test_json_body_no_app(self):
        self.configure({"execution": {
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"Content-Type": "application/exi"},
                    "body": {
                        "store_id": "${store_id}",
                        "display_name": "${display_name}"
                    }}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp'
        selector += '>elementProp>stringProp[name="Argument.value"]'
        self.assertEqual(jmx.get(selector)[0].text.find('"store_id": "${store_id}"'), -1)

    def test_complicate_body(self):
        self.configure({"execution": {
            "scenario": {
                "requests": [
                    {
                        "url": "http://blazedemo1.com",         # header + complicated dict = json
                        "method": "POST",
                        "headers": {"Content-Type": "application/json"},
                        "body": {"key1": {"key2": "val3"}}
                    }, {
                        "url": "http://blazedemo2.com",         # no header + easy dict = form
                        "method": "POST",
                        "body": {"key4": "val5"}
                    }, {
                        "url": "http://blazedemo3.com",         # no header + complicated dict = json
                        "method": "POST",
                        "body": {
                            "key6": {"key7": "val8"}}
                    }, {
                        "url": "http://blazedemo4.com",         # header + easy dict = json
                        "method": "POST",
                        "headers": {"Content-Type": "application/json"},
                        "body": {"key9": "val10"}
                    }]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.original_jmx)
        selector = 'elementProp[name="HTTPsampler.Arguments"]>collectionProp>elementProp>stringProp[name="Argument.%s"]'

        names = jmx.get(selector % "name")
        vals = jmx.get(selector % "value")

        target_names = ['key4']
        target_vals = ['{"key1": {"key2": "val3"}}', 'val5', '{"key6": {"key7": "val8"}}', '{"key9": "val10"}']

        self.assertEqual(target_names, [name.text for name in names])
        self.assertEqual(target_vals, [val.text for val in vals])

    def test_jtl_verbose(self):
        self.configure({"execution": {
            "write-xml-jtl": "full",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com"}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertNotEqual(jmx.get('ResultCollector[testname="Trace Writer"]'), [])
        self.assertEqual(jmx.get('ResultCollector[testname="Errors Writer"]'), [])

    def test_jtl_errors(self):
        self.configure({"execution": {
            "write-xml-jtl": "error",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com"}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertNotEqual(jmx.get('ResultCollector[testname="Errors Writer"]'), [])
        self.assertEqual(jmx.get('ResultCollector[testname="Trace Writer"]'), [])

    def test_jtl_none(self):
        self.configure({"execution": {
            "write-xml-jtl": "bla-bla-bla",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com"}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertEqual(jmx.get('ResultCollector[testname="Trace Writer"]'), [])
        self.assertEqual(jmx.get('ResultCollector[testname="Errors Writer"]'), [])

    def test_csv_jtl_flags(self):
        self.configure({"execution": {
            "write-xml-jtl": "error",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com"}]}}})
        self.obj.settings.merge({'csv-jtl-flags': {
            'saveAssertionResultsFailureMessage': True,
            'sentBytes': True,
            'idleTime': True,
            'dataType': False}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='KPI Writer']")
        self.assertEqual(1, len(writers))
        self.assertEqual('true', writers[0].find('objProp/value/saveAssertionResultsFailureMessage').text)
        self.assertEqual('true', writers[0].find('objProp/value/sentBytes').text)
        self.assertEqual('true', writers[0].find('objProp/value/idleTime').text)
        self.assertEqual('false', writers[0].find('objProp/value/dataType').text)

    def test_jtl_flags(self):
        self.configure({"execution": {
            "write-xml-jtl": "error",
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com"}]}}})
        self.obj.settings.merge({'xml-jtl-flags': {
            'responseData': True,
            'message': False}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        writers = xml_tree.findall(".//ResultCollector[@testname='Errors Writer']")
        self.assertEqual(1, len(writers))
        self.assertEqual('false', writers[0].find('objProp/value/samplerData').text)
        self.assertEqual('false', writers[0].find('objProp/value/message').text)
        self.assertEqual('true', writers[0].find('objProp/value/responseData').text)
        self.assertEqual('true', writers[0].find('objProp/value/bytes').text)

    def test_jmx_modification_unicode(self):
        cfg_selector = ('Home Page>HTTPsampler.Arguments>Arguments.arguments'
                        '>param>Argument.value')

        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "/jmeter/jmx/dummy_plan.jmx",
                "modifications": {
                    "set-prop": {
                        cfg_selector: u"✓"}}}})
        selector = ("[testname='Home Page']>[name='HTTPsampler.Arguments']"
                    ">[name='Arguments.arguments']>[name='param']>[name='Argument.value']")
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertEqual(jmx.get(selector)[0].text, u"✓")

    def test_jmx_modification_add_stringprop(self):
        cfg_selector = ('Home Page>HTTPsampler.Arguments>Arguments.arguments>param>new_str')

        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "/jmeter/jmx/dummy_plan.jmx",
                "modifications": {
                    "set-prop": {
                        cfg_selector: 'new_value'}}}})
        selector = ("[testname='Home Page']>[name='HTTPsampler.Arguments']"
                    ">[name='Arguments.arguments']>[name='param']>[name='new_str']")
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertEqual(jmx.get(selector)[0].text, 'new_value')

    def test_resources_regex(self):
        self.configure({"execution": {
            "scenario": {
                "retrieve-resources": True,
                "retrieve-resources-regex": "myregex",
                "requests": [{"url": "http://blazedemo.com/"}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        self.assertEqual(jmx.get('boolProp[name="HTTPSampler.image_parser"]')[0].text, "true")
        self.assertEqual(jmx.get('stringProp[name="HTTPSampler.embedded_url_re"]')[0].text, "myregex")

    def test_data_source_list(self):
        self.obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com/"],
                # note that data-sources should be a list of strings/objects
                "data-sources": {
                    "path": RESOURCES_DIR + "test1.csv"}}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_force_parent_sample(self):
        self.configure({
            'execution': {
                'scenario': {
                    'force-parent-sample': True,
                    'script': RESOURCES_DIR + '/jmeter/jmx/transactions.jmx'}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        self.assertTrue(all(prop.text == 'true' for prop in props))

    def test_force_parent_sample_default(self):
        self.configure({
            'execution': {
                'scenario': {
                    # 'force-parent-sample' is False by default
                    'script': RESOURCES_DIR + '/jmeter/jmx/transactions.jmx'}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        self.assertTrue(all(prop.text == 'false' for prop in props))

    def test_disable_force_parent_sample(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + '/jmeter/jmx/transactions.jmx',
                    'force-parent-sample': False}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 2)
        non_parent = props[1]
        self.assertEqual(non_parent.text, 'false')

    def test_transaction_and_requests1(self):
        self.configure({
            'execution': {
                'scenario': {
                    'force-parent-sample': False,
                    'requests': [{
                        'transaction': 'MY_TRANSACTION',
                        'do': [{
                            'url': 'http://blazedemo.com'}]}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 1)
        non_parent = props[0]
        self.assertEqual(non_parent.text, 'false')

    def test_transaction_and_requests2(self):
        self.configure({
            'execution': {
                'scenario': {
                    'requests': [{
                        'transaction': 'MY_TRANSACTION',
                        'force-parent-sample': False,
                        'do': [{
                            'url': 'http://blazedemo.com'}]}]}}})
        self.obj.prepare()
        jmx = JMX(self.obj.modified_jmx)
        selector = 'TransactionController > boolProp[name="TransactionController.parent"]'
        props = jmx.get(selector)
        self.assertEqual(len(props), 1)
        non_parent = props[0]
        self.assertEqual(non_parent.text, 'false')

    def test_jvm_heap_settings(self):
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {
                    'script': RESOURCES_DIR + '/jmeter/jmx/http.jmx'}},
            'modules': {
                'jmeter': {
                    'memory-xmx': '2G'}}})
        self.obj.prepare()
        self.obj.env.set({'TEST_MODE': 'jvm_args'})
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()
        with open(os.path.join(self.obj.engine.artifacts_dir, "jmeter.out")) as fds:
            stdout = fds.read()
        self.assertIn("-Xmx2G", stdout)

    def test_data_sources_in_artifacts(self):
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {
                    'data-sources': ['test1.csv'],
                    'requests': ['http://blazedemo.com/${url}']}}})
        csv_source = RESOURCES_DIR + 'test1.csv'
        self.obj.engine.file_search_paths.append(self.obj.engine.artifacts_dir)
        shutil.copy2(csv_source, self.obj.engine.artifacts_dir)
        self.obj.prepare()

    def test_headers(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "headers": {
                            "str": "string_value",
                            "bool": True,
                            "num": 3}}]}}})
        self.obj.prepare()

    def test_body_file_in_artifacts(self):
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {
                    'requests': [{
                        "method": "PUT",
                        "url": "http://blazedemo.com/",
                        "body-file": "http.jmx"}]}}})
        jmx_source = RESOURCES_DIR + '/jmeter/jmx/http.jmx'
        self.obj.engine.file_search_paths.append(self.obj.engine.artifacts_dir)
        shutil.copy2(jmx_source, self.obj.engine.artifacts_dir)
        self.obj.prepare()

    def test_jmx_paths_local_prov(self):
        "Ensures that file paths in JMX are not changed during local prov"
        script = RESOURCES_DIR + "/jmeter/jmx/csvs.jmx"
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": script}}})
        self.obj.prepare()
        original = JMX(script)
        prepared = JMX(self.obj.modified_jmx)
        query = '//CSVDataSet/stringProp[@name="filename"]/text()'
        original_paths = original.tree.xpath(query)
        prepared_paths = prepared.tree.xpath(query)
        self.assertEqual(original_paths, prepared_paths)

    def test_jmx_paths_remote_prov(self):
        "Ensures that file paths in JMX are modified during remote prov"
        script = RESOURCES_DIR + "/jmeter/jmx/csvs.jmx"
        self.configure({
            'execution': {
                'iterations': 1,
                'scenario': {
                    "script": script}},
            'provisioning': 'cloud'})
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

    def test_intprop_modification(self):
        script = RESOURCES_DIR + "/jmeter/jmx/int_threads.jmx"
        self.configure({
            'execution': {
                'iterations': 1,
                'concurrency': 3,
                'scenario': {
                    "script": script}}})
        self.obj.prepare()
        prepared = JMX(self.obj.modified_jmx)
        tnum_sel = ".//*[@name='ThreadGroup.num_threads']"
        prepared_threads = prepared.tree.xpath(tnum_sel)
        self.assertEqual(1, int(prepared_threads[0].text))
        self.assertEqual(2, int(prepared_threads[1].text))

    def test_request_logic_if(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "if": "<cond>",
                        "then": [
                            "http://blazedemo.com/"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        if_controller = xml_tree.find(".//IfController")
        self.assertIsNotNone(if_controller)
        condition = xml_tree.find(".//IfController/stringProp[@name='IfController.condition']")
        self.assertIsNotNone(condition)
        self.assertEqual(condition.text, "<cond>")

    def test_request_logic_if_else(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "if": "<cond>",
                        "then": [
                            "http://blazedemo.com/"],
                        "else": [
                            "http://demo.blazemeter.com/"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        ifs = xml_tree.findall(".//IfController")
        self.assertEqual(2, len(ifs))
        conditions = xml_tree.findall(".//IfController/stringProp[@name='IfController.condition']")
        self.assertEqual(2, len(conditions))
        self.assertEqual(conditions[0].text, "<cond>")
        self.assertEqual(conditions[1].text, "!(<cond>)")

    def test_request_logic_nested_if(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "if": "<cond1>",
                        "then": [
                            "http://blazedemo.com/", {
                                "if": "<cond2>",
                                "then": [
                                    "http://demo.blazemeter.com/"]}]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        ifs = xml_tree.findall(".//IfController")
        self.assertEqual(2, len(ifs))
        conditions = xml_tree.findall(".//IfController/stringProp[@name='IfController.condition']")
        self.assertEqual(2, len(conditions))
        self.assertEqual(conditions[0].text, "<cond1>")
        self.assertEqual(conditions[1].text, "<cond2>")

    def test_resource_files_nested_requests(self):
        self.configure({
            'execution': {
                'scenario': {
                    "data-sources": [RESOURCES_DIR + "test1.csv"],
                    "requests": [{
                        "if": "<cond1>",
                        "then": [{
                            "if": "<cond2>",
                            "then": [{
                                "url": "http://demo.blazemeter.com/",
                                "method": "POST",
                                "body-file": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"
                            }],
                            "else": [
                                {"action": "continue"},
                            ]
                        }]}]}},
            'provisioning': 'cloud'})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 2)

    def test_request_logic_loop(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "loop": 10,
                        "do": [
                            "http://blazedemo.com/"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        controller = xml_tree.find(".//LoopController")
        self.assertIsNotNone(controller)
        loops = xml_tree.find(".//LoopController/stringProp[@name='LoopController.loops']")
        self.assertEqual(loops.text, "10")
        forever = xml_tree.find(".//LoopController/boolProp[@name='LoopController.continue_forever']")
        self.assertEqual(forever.text, "true")

    def test_auth_manager_multi_form(self):
        self.configure({
            'execution': {
                'scenario': {
                    "authorization": [{
                        "url": "http://blazedemo.com/",
                        "name": "user1",
                        "password": "pass1"
                    }, {
                        "domain": "my_domain",
                        "name": "user2",
                        "password": "pass2",
                        "realm": "secret_zone",
                        "mechanism": "kerberos"
                    }, {
                        "url": "localhost",
                        "name": "user3",
                        "password": "pass3",
                        "mechanism": "digest"
                    }, {
                        # no url/domain, must be skipped
                        "name": "user4",
                        "password": "pass4"
                    }],
                    "requests": ["empty"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        auth_mgr = xml_tree.findall(".//hashTree[@type='tg']/AuthManager")
        self.assertEqual(1, len(auth_mgr))
        clear = auth_mgr[0].find("boolProp[@name='AuthManager.clearEachIteration']")
        self.assertIsNone(clear)
        collection = auth_mgr[0].findall("collectionProp[@name='AuthManager.auth_list']/elementProp")
        self.assertEqual(3, len(collection))
        self.assertEqual(["http://blazedemo.com/", "user1", "pass1", None, None, None], self.get_auth(collection[0]))
        self.assertEqual([None, "user2", "pass2", "my_domain", "secret_zone", "KERBEROS"], self.get_auth(collection[1]))
        self.assertEqual(["localhost", "user3", "pass3", None, None, None], self.get_auth(collection[2]))

    @staticmethod
    def get_auth(element):
        props = [
            element.find("stringProp[@name='Authorization.url']"),
            element.find("stringProp[@name='Authorization.username']"),
            element.find("stringProp[@name='Authorization.password']"),
            element.find("stringProp[@name='Authorization.domain']"),
            element.find("stringProp[@name='Authorization.realm']"),
            element.find("stringProp[@name='Authorization.mechanism']")]
        for i in range(len(props)):
            if props[i] is not None:
                props[i] = props[i].text
        return props

    def test_auth_manager_short_form(self):
        self.configure({
            'execution': {
                'scenario': {
                    "authorization": {
                        "url": "http://blazedemo.com/",
                        "name": "user1",
                        "password": "pass1"
                    },
                    "requests": ["empty"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        auth_mgr = xml_tree.findall(".//hashTree[@type='tg']/AuthManager")
        self.assertEqual(1, len(auth_mgr))
        clear = auth_mgr[0].find("boolProp[@name='AuthManager.clearEachIteration']")
        self.assertIsNone(clear)
        collection = auth_mgr[0].findall("collectionProp[@name='AuthManager.auth_list']/elementProp")
        self.assertEqual(1, len(collection))
        url = collection[0].findall("stringProp[@name='Authorization.url']")
        self.assertEqual(url[0].text, "http://blazedemo.com/")

    def test_auth_manager_full_form(self):
        self.configure({
            'execution': {
                'scenario': {
                    "authorization": {
                        "clear": True,
                        "list": [{
                            "url": "http://blazedemo.com/",
                            "name": "user1",
                            "password": "pass1"
                        }]},
                    "requests": ["empty"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        auth_mgr = xml_tree.findall(".//hashTree[@type='tg']/AuthManager")
        self.assertEqual(1, len(auth_mgr))
        clear = auth_mgr[0].find("boolProp[@name='AuthManager.clearEachIteration']")
        self.assertEqual("true", clear.text)
        collection = auth_mgr[0].findall("collectionProp[@name='AuthManager.auth_list']/elementProp")
        self.assertEqual(1, len(collection))
        url = collection[0].findall("stringProp[@name='Authorization.url']")
        self.assertEqual(url[0].text, "http://blazedemo.com/")

    def test_request_once(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "once": ["http://blazedemo.com/"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        controller = xml_tree.find(".//OnceOnlyController")
        self.assertIsNotNone(controller)

    def test_request_logic_loop_forever(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "loop": "forever",
                        "do": [
                            "http://blazedemo.com/"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        controller = xml_tree.find(".//LoopController")
        self.assertIsNotNone(controller)

        fprops = xml_tree.findall(".//boolProp[@name='LoopController.continue_forever']")
        for fprop in fprops:
            pptag = fprop.getparent().getparent().tag
            if pptag == "ThreadGroup":
                self.assertEqual("false", fprop.text)
            elif pptag == "hashTree":
                self.assertEqual("true", fprop.text)
            else:
                self.fail()

        loops = xml_tree.find(".//LoopController/stringProp[@name='LoopController.loops']")
        self.assertEqual(loops.text, "-1")

    def test_request_logic_loop_invalid(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "loop": 100}]}}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_resource_files_loops(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "loop": 100,
                        "do": [{
                            "url": "http://demo.blazemeter.com/",
                            "method": "POST",
                            "body-file": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}]}]}},
            'provisioning': 'cloud'})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_request_logic_while(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "while": "<cond>",
                        "do": [
                            "http://blazedemo.com/"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        if_controller = xml_tree.find(".//WhileController")
        self.assertIsNotNone(if_controller)
        condition = xml_tree.find(".//WhileController/stringProp[@name='WhileController.condition']")
        self.assertIsNotNone(condition)
        self.assertEqual(condition.text, "<cond>")

    def test_request_logic_while_invalid(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "while": "<cond>"}]}}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_request_logic_while_resources(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "while": "<cond>",
                        "do": [{
                            "url": "http://demo.blazemeter.com/",
                            "method": "POST",
                            "body-file": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}]}]}},
            'provisioning': 'cloud'})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_request_logic_foreach(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "foreach": "name in usernames",
                        "do": [
                            "http://site.com/users/${name}"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        self.assertIsNotNone(xml_tree.find(".//ForeachController"))
        input = xml_tree.find(".//ForeachController/stringProp[@name='ForeachController.inputVal']")
        self.assertEqual(input.text, "usernames")
        loop_var = xml_tree.find(".//ForeachController/stringProp[@name='ForeachController.returnVal']")
        self.assertEqual(loop_var.text, "name")

    def test_request_logic_foreach_resources(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "foreach": "item in coll",
                        "do": [{
                            "url": "http://${item}.blazemeter.com/",
                            "method": "POST",
                            "body-file": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}]}]}},
            'provisioning': 'cloud'})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_request_logic_transaction(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "transaction": "API",
                        "do": [
                            "http://blazedemo.com/",
                            "http://blazedemo.com/reserve.php"]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        controller = xml_tree.find(".//TransactionController")
        self.assertIsNotNone(controller)
        self.assertEqual(controller.get('testname'), "API")

    def test_request_logic_transaction_resources(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "transaction": "API",
                        "do": [{
                            "url": "http://demo.blazemeter.com/",
                            "method": "POST",
                            "body-file": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}]}]}},
            'provisioning': 'cloud'})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_request_logic_include(self):
        self.configure({
            'scenarios': {
                'login': {
                    'requests': ['http://example.com/login']}},
            'execution': {
                'scenario': {
                    "requests": [{
                        "include-scenario": "login"}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        controller = xml_tree.find(".//GenericController")
        self.assertIsNotNone(controller)
        self.assertEqual(controller.get('testname'), "login")
        ht = controller.getnext()
        sampler = ht.find('HTTPSamplerProxy')
        self.assertIsNotNone(sampler)
        domain = sampler.find('stringProp[@name="HTTPSampler.domain"]')
        self.assertEqual(domain.text, "example.com")
        path = sampler.find('stringProp[@name="HTTPSampler.path"]')
        self.assertEqual(path.text, "/login")

    def test_request_logic_include_resources(self):
        self.configure({
            'scenarios': {
                'login': {
                    'data-sources': [RESOURCES_DIR + "test1.csv"],
                    'requests': [{
                        "url": "http://demo.blazemeter.com/",
                        "method": "POST",
                        "body-file": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}]}},
            'execution': {
                'scenario': {
                    'data-sources': [RESOURCES_DIR + "test2.csv"],
                    "requests": [{
                        "include-scenario": "login"}]}},
            'provisioning': 'cloud'})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 3)

    def test_logic_include_data_sources(self):
        self.configure({
            'scenarios': {
                'login': {
                    'data-sources': [RESOURCES_DIR + "test1.csv"],
                    'requests': ['http://blazedemo.com/auth/${test1}']}},
            'execution': {
                'scenario': {
                    "data-sources": [RESOURCES_DIR + "test2.csv"],
                    "requests": [
                        {"include-scenario": "login"},
                        "http://example.com/${test2}"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        thread_group = xml_tree.find('.//hashTree[@type="tg"]')
        scenario_dataset = xml_tree.find('.//hashTree[@type="tg"]/CSVDataSet')
        self.assertIsNotNone(scenario_dataset)
        filename = scenario_dataset.find('stringProp[@name="filename"]')
        self.assertEqual(filename.text, get_full_path(RESOURCES_DIR + "test2.csv"))
        login_controler = thread_group.find('GenericController')
        self.assertIsNotNone(login_controler)
        login_ht = login_controler.getnext()
        login_dataset = login_ht.find('CSVDataSet')
        self.assertIsNotNone(login_dataset)
        filename = scenario_dataset.find('stringProp[@name="filename"]')
        self.assertEqual(filename.text, get_full_path(RESOURCES_DIR + "test2.csv"))

    def test_include_recursion(self):
        self.configure({
            'scenarios': {
                'a': {
                    'requests': [{
                        "include-scenario": "b"}]},
                'b': {
                    'requests': [{
                        "include-scenario": "a"}]}},
            'execution': {
                'scenario': 'a'}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_include_sources_recursion(self):
        self.configure({
            'scenarios': {
                'a': {
                    'requests': [{
                        "include-scenario": "b"}]},
                'b': {
                    'requests': [{
                        "include-scenario": "a"}]}},
            'execution': {
                'scenario': 'a'}})
        self.assertRaises(TaurusConfigError, self.obj.resource_files)

    def test_logic_test_action(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "action": "pause",
                        "pause-duration": "1s",
                    }]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        block = xml_tree.find(".//TestAction")
        self.assertIsNotNone(block)
        action = block.find('intProp[@name="ActionProcessor.action"]')
        self.assertEqual(action.text, "1")
        target = block.find('intProp[@name="ActionProcessor.target"]')
        self.assertEqual(target.text, "0")
        target = block.find('stringProp[@name="ActionProcessor.duration"]')
        self.assertEqual(target.text, "1000")

    def test_logic_test_action_target(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "action": "stop",
                        "target": "all-threads",
                    }]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        block = xml_tree.find(".//TestAction")
        self.assertIsNotNone(block)
        action = block.find('intProp[@name="ActionProcessor.action"]')
        self.assertEqual(action.text, "0")
        target = block.find('intProp[@name="ActionProcessor.target"]')
        self.assertEqual(target.text, "2")

    def test_logic_test_action_unknown(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "action": "unknown",
                    }]}}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_request_logic_set_vars(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "set-variables": {"foo": "bar"}}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        set_var_action = xml_tree.find(".//" + JMX.SET_VAR_ACTION)
        self.assertIsNotNone(set_var_action)
        self.assertEqual("foo", set_var_action.find(".//stringProp[@name='Argument.name']").text)
        self.assertEqual("bar", set_var_action.find(".//stringProp[@name='Argument.value']").text)

    def test_request_null_headers(self):
        self.configure({
            'execution': {
                'scenario': {
                    "headers": None,
                    "requests": [
                        "http://blazedemo.com/"]}}})
        self.obj.prepare()

    def test_multipart_file_upload(self):
        path0 = "test1.csv"
        path1 = os.path.join("jmeter", "unicode_file")
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "method": "POST",
                        "upload-files": [{
                            "path": path0,
                            "param": "stats",
                            "mime-type": "text/csv"
                        }, {
                            "path": path1,
                            "param": "report",
                            "mime-type": "application/pdf"}]}]}}})
        self.obj.engine.file_search_paths = [RESOURCES_DIR]
        self.assertTrue(os.path.isfile(self.obj.engine.find_file(path0)))
        self.assertTrue(os.path.isfile(self.obj.engine.find_file(path1)))

        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        request = xml_tree.find('.//HTTPSamplerProxy')
        self.assertIsNotNone(request)
        file_query = 'elementProp[@name="HTTPsampler.Files"]/collectionProp[@name="HTTPFileArgs.files"]/elementProp'
        files = request.findall(file_query)
        self.assertEqual(len(files), 2)

        self.assertTrue(files[0].get('name').endswith(path0))
        full_path0 = files[0].find('stringProp[@name="File.path"]').text
        self.assertTrue(full_path0.endswith(path0))
        self.assertTrue(os.path.isfile(full_path0))
        self.assertEqual(files[0].find('stringProp[@name="File.paramname"]').text, "stats")
        self.assertEqual(files[0].find('stringProp[@name="File.mimetype"]').text, "text/csv")

        self.assertTrue(files[1].get('name').endswith(path1))
        full_path1 = files[1].find('stringProp[@name="File.path"]').text
        self.assertTrue(full_path1.endswith(path1))
        self.assertTrue(os.path.isfile(full_path1))

        self.assertEqual(files[1].find('stringProp[@name="File.paramname"]').text, "report")
        self.assertEqual(files[1].find('stringProp[@name="File.mimetype"]').text, "application/pdf")

    def test_upload_files_mime_autodetect(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "method": "POST",
                        "upload-files": [{
                            "path": "sound.mp3",
                            "param": "stats",
                        }, {
                            "path": "report.pdf",
                            "param": "report",
                        }, {
                            "path": "unknown.file",
                            "param": "stuff"}]}]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        request = xml_tree.find('.//HTTPSamplerProxy')
        self.assertIsNotNone(request)
        file_query = 'elementProp[@name="HTTPsampler.Files"]/collectionProp[@name="HTTPFileArgs.files"]/elementProp'
        files = request.findall(file_query)
        self.assertEqual(len(files), 3)
        self.assertEqual(files[0].find('stringProp[@name="File.mimetype"]').text, "audio/mpeg")
        self.assertEqual(files[1].find('stringProp[@name="File.mimetype"]').text, "application/pdf")
        self.assertEqual(files[2].find('stringProp[@name="File.mimetype"]').text, "application/octet-stream")

    def test_upload_files_paths(self):
        self.configure({
            'execution': {
                'scenario': {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "method": "POST",
                        "upload-files": [{
                            "path": "${some_var}",  # variable
                            "param": "stats",
                        }, {
                            "path": "body-file.dat",  # relpath from RES_DIR/jmeter
                            "param": "report",
                        }, {
                            "path": os.path.join(RESOURCES_DIR, 'jmeter', 'unicode-file'),  # abs path
                            "param": "stuff"}]}]}}})
        self.obj.engine.file_search_paths.append(os.path.join(RESOURCES_DIR, 'jmeter'))
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        request = xml_tree.find('.//HTTPSamplerProxy')
        self.assertIsNotNone(request)
        file_query = 'elementProp[@name="HTTPsampler.Files"]/collectionProp[@name="HTTPFileArgs.files"]/elementProp'
        files = request.findall(file_query)
        self.assertEqual(len(files), 3)
        paths = [_file.find('stringProp[@name="File.path"]').text for _file in files]
        paths.sort()
        norm = ['${some_var}',
                os.path.join(RESOURCES_DIR, 'jmeter', 'body-file.dat'),
                os.path.join(RESOURCES_DIR, 'jmeter', 'unicode-file')]
        self.assertEqual(paths, norm)

    def test_data_sources_jmx_gen_loop(self):
        self.configure({
            'execution': {
                'scenario': {
                    "data-sources": [{
                        "path": RESOURCES_DIR + "test1.csv",
                        "loop": True}],
                    "requests": [
                        "http://example.com/${test1}"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        dataset = xml_tree.find('.//hashTree[@type="tg"]/CSVDataSet')
        self.assertIsNotNone(dataset)
        filename = dataset.find('stringProp[@name="filename"]')
        self.assertEqual(filename.text, get_full_path(RESOURCES_DIR + "test1.csv"))
        loop = dataset.find('boolProp[@name="recycle"]')
        self.assertEqual(loop.text, "true")
        stop = dataset.find('boolProp[@name="stopThread"]')
        self.assertEqual(stop.text, "false")

    def test_data_sources_jmx_gen_random_defaults(self):
        self.configure({
            'execution': {
                'scenario': {
                    "data-sources": [{
                        "path": RESOURCES_DIR + "test1.csv",
                        "random-order": True}],
                    "requests": [
                        "http://example.com/${test1}"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        dataset = xml_tree.find('.//hashTree[@type="tg"]/com.blazemeter.jmeter.RandomCSVDataSetConfig')
        self.assertIsNotNone(dataset)
        filename = dataset.find('stringProp[@name="filename"]')
        self.assertEqual(filename.text, get_full_path(RESOURCES_DIR + "test1.csv"))
        encoding = dataset.find('stringProp[@name="fileEncoding"]')
        self.assertEqual(encoding.text, "UTF-8")
        random_order = dataset.find('boolProp[@name="randomOrder"]')
        self.assertEqual(random_order.text, "true")
        ignore_first_line = dataset.find('boolProp[@name="ignoreFirstLine"]')
        self.assertEqual(ignore_first_line.text, "true")
        rewind_list_end = dataset.find('boolProp[@name="rewindOnTheEndOfList"]')
        self.assertEqual(rewind_list_end.text, "true")
        independent_list = dataset.find('boolProp[@name="independentListPerThread"]')
        self.assertEqual(independent_list.text, "false")

    def test_data_sources_jmx_gen_random_reversed(self):
        self.configure({
            'execution': {
                'scenario': {
                    "data-sources": [{
                        "path": RESOURCES_DIR + "test1.csv",
                        "variable-names": "first,second",
                        "random-order": True,
                        "loop": False}],
                    "requests": [
                        "http://example.com/${test1}"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        dataset = xml_tree.find('.//hashTree[@type="tg"]/com.blazemeter.jmeter.RandomCSVDataSetConfig')
        self.assertIsNotNone(dataset)
        filename = dataset.find('stringProp[@name="filename"]')
        self.assertEqual(filename.text, get_full_path(RESOURCES_DIR + "test1.csv"))
        variable_names = dataset.find('stringProp[@name="variableNames"]')
        self.assertEqual(variable_names.text, "first,second")
        random_order = dataset.find('boolProp[@name="randomOrder"]')
        self.assertEqual(random_order.text, "true")
        ignore_first_line = dataset.find('boolProp[@name="ignoreFirstLine"]')
        self.assertEqual(ignore_first_line.text, "false")
        rewind_list_end = dataset.find('boolProp[@name="rewindOnTheEndOfList"]')
        self.assertEqual(rewind_list_end.text, "false")

    def test_data_sources_jmx_gen_stop(self):
        self.configure({
            'execution': {
                'scenario': {
                    "data-sources": [{
                        "path": RESOURCES_DIR + "test1.csv",
                        "loop": False}],
                    "requests": [
                        "http://example.com/${test1}"]}}})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        dataset = xml_tree.find('.//hashTree[@type="tg"]/CSVDataSet')
        self.assertIsNotNone(dataset)
        filename = dataset.find('stringProp[@name="filename"]')
        self.assertEqual(filename.text, get_full_path(RESOURCES_DIR + "test1.csv"))
        loop = dataset.find('boolProp[@name="recycle"]')
        self.assertEqual(loop.text, "false")
        stop = dataset.find('boolProp[@name="stopThread"]')
        self.assertEqual(stop.text, "true")

    def test_data_sources_varnames(self):
        origin = [{
            "url": "http://example.com:8080/some${where}",
            "label": "food${type}",
            "method": "${method}"
        }, {
            "url": "some_url",
            "method": "get"
        }]

        self.configure({
            'execution': {
                'scenario': {
                    "data-sources": [{
                        "path": RESOURCES_DIR + "test1.csv",
                        "variable-names": "a,b,c"}],
                    "requests": origin}}})

        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        dataset = xml_tree.find('.//hashTree[@type="tg"]/CSVDataSet')
        self.assertIsNotNone(dataset)
        varnames = dataset.find('stringProp[@name="variableNames"]')
        self.assertEqual(varnames.text, "a,b,c")

        samplers = xml_tree.findall('.//HTTPSamplerProxy')
        self.assertEqual(2, len(samplers))

        protocol = samplers[0].find('stringProp[@name="HTTPSampler.protocol"]').text
        domain = samplers[0].find('stringProp[@name="HTTPSampler.domain"]').text
        port = samplers[0].find('stringProp[@name="HTTPSampler.port"]').text
        path = samplers[0].find('stringProp[@name="HTTPSampler.path"]').text
        method = samplers[0].find('stringProp[@name="HTTPSampler.method"]').text
        label = samplers[0].attrib["testname"]

        self.assertEqual(protocol, "http")
        self.assertEqual(domain, "example.com")
        self.assertEqual(port, "8080")
        self.assertEqual(path, "/some${where}")
        self.assertEqual(method, origin[0]["method"])
        self.assertEqual(label, origin[0]["label"])

        url = samplers[1].find('stringProp[@name="HTTPSampler.path"]').text
        method = samplers[1].find('stringProp[@name="HTTPSampler.method"]').text

        self.assertEqual(url, origin[1]["url"])
        self.assertEqual(method, origin[1]["method"].upper())

    def test_func_mode_jmeter_2_13(self):
        self.obj.engine.aggregator.is_functional = True
        self.configure({"execution": {
            'scenario': {
                "requests": [
                    "http://example.com/",
                ],
            }
        }})
        self.obj.settings.merge({"version": "2.13"})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        functional_switch = xml_tree.find('.//TestPlan/boolProp[@name="TestPlan.functional_mode"]')
        self.assertEqual(functional_switch.text, "true")
        connect_time_flag = xml_tree.find('.//ResultCollector/objProp/value/connectTime')
        self.assertEqual(connect_time_flag.text, "true")
        bytes_flag = xml_tree.find('.//ResultCollector/objProp/value/bytes')  # 'bytes' in JMeter 2.13
        self.assertEqual(bytes_flag.text, "true")

    def test_func_mode_jmeter_3_xx(self):
        self.obj.engine.aggregator.is_functional = True
        self.configure({"execution": {
            'scenario': {
                "requests": [
                    "http://example.com/",
                ],
            }
        }})
        self.obj.settings.merge({"version": "3.2"})
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        functional_switch = xml_tree.find('.//TestPlan/boolProp[@name="TestPlan.functional_mode"]')
        self.assertEqual(functional_switch.text, "true")
        connect_time_flag = xml_tree.find('.//ResultCollector/objProp/value/connectTime')
        self.assertEqual(connect_time_flag.text, "true")
        bytes_flag = xml_tree.find('.//ResultCollector/objProp/value/sentBytes')  # 'sentBytes' since JMeter 3
        self.assertEqual(bytes_flag.text, "true")

    def test_detect_ver_empty(self):
        self.configure({"execution": {
            'scenario': {
                "requests": [
                    "http://example.com/"]}}})
        self.obj.settings.merge({"version": "auto"})
        self.obj.prepare()
        self.assertEqual(JMeter.VERSION, self.obj.tool.version)

    def test_detect_ver_wrong(self):
        self.obj.execution.merge({
            'scenario': {
                "script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"}})
        self.obj.settings.merge({"version": "auto"})
        self.obj.prepare()
        self.assertEqual(JMeter.VERSION, self.obj.tool.version)

    def test_detect_ver_2_13(self):
        self.obj.execution.merge({
            'scenario': {
                "script": RESOURCES_DIR + "/jmeter/jmx/SteppingThreadGroup.jmx"}})
        self.obj.settings.merge({"version": "auto"})
        self.obj.prepare()
        self.assertEqual("2.13", self.obj.tool.version)

    def test_no_detect_2_13(self):
        self.obj.execution.merge({
            'scenario': {
                "script": RESOURCES_DIR + "/jmeter/jmx/SteppingThreadGroup.jmx"}})
        self.obj.prepare()
        self.assertEqual(JMeter.VERSION, self.obj.tool.version)

    def test_jsr223_block(self):
        script = RESOURCES_DIR + "/jmeter/jsr223_script.js"
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "jsr223": {
                            "language": "javascript",
                            "script-file": script,
                            "parameters": "first second"
                        }
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        post_procs = xml_tree.findall(
            ".//hashTree[@type='tg']/hashTree/JSR223PostProcessor[@testclass='JSR223PostProcessor']")
        self.assertEqual(1, len(post_procs))

        jsr = post_procs[0]
        self.assertEqual(script, jsr.find(".//stringProp[@name='filename']").text)
        self.assertEqual("javascript", jsr.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual("first second", jsr.find(".//stringProp[@name='parameters']").text)
        self.assertEqual("true", jsr.find(".//stringProp[@name='cacheKey']").text)

    def test_jsr223_exceptions_2(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "jsr223": {
                            "language": "javascript"
                        }
                    }]
                }
            }
        })
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_jsr223_multiple(self):
        pre_script = RESOURCES_DIR + "/jmeter/jsr223_script.js"
        post_script = RESOURCES_DIR + "/jmeter/bean_script.bhs"
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "jsr223": [{
                            "language": "javascript",
                            "script-file": pre_script,
                            "execute": "before",
                            "compile-cache": False
                        }, {
                            "language": "beanshell",
                            "script-file": post_script,
                            "execute": "after",
                            "compile-cache": True
                        },
                            'vars.put("a", 1)']
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        pre_procs = xml_tree.findall(".//JSR223PreProcessor[@testclass='JSR223PreProcessor']")
        post_procs = xml_tree.findall(".//JSR223PostProcessor[@testclass='JSR223PostProcessor']")
        self.assertEqual(1, len(pre_procs))
        self.assertEqual(2, len(post_procs))

        pre = pre_procs[0]
        self.assertEqual(pre_script, pre.find(".//stringProp[@name='filename']").text)
        self.assertEqual("javascript", pre.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual("false", pre.find(".//stringProp[@name='cacheKey']").text)
        self.assertEqual(None, pre.find(".//stringProp[@name='parameters']").text)

        pre = post_procs[0]
        self.assertEqual(post_script, pre.find(".//stringProp[@name='filename']").text)
        self.assertEqual("beanshell", pre.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual("true", pre.find(".//stringProp[@name='cacheKey']").text)
        self.assertEqual(None, pre.find(".//stringProp[@name='parameters']").text)

        pre = post_procs[1]
        self.assertEqual(None, pre.find(".//stringProp[@name='filename']").text)
        self.assertEqual("groovy", pre.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual(None, pre.find(".//stringProp[@name='parameters']").text)
        self.assertEqual('vars.put("a", 1)', pre.find(".//stringProp[@name='script']").text)

    def test_jsr223_scenario_level_block(self):
        script = RESOURCES_DIR + "/jmeter/jsr223_script.js"
        self.configure({
            "execution": {
                "scenario": {
                    "jsr223": {
                        "language": "javascript",
                        "script-file": script,
                        "parameters": "first second"
                    },
                    "requests": [{
                        "url": "http://blazedemo.com/",
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        post_procs = xml_tree.findall(".//hashTree[@type='tg']/JSR223PostProcessor[@testclass='JSR223PostProcessor']")
        self.assertEqual(1, len(post_procs))

        jsr = post_procs[0]
        self.assertEqual(script, jsr.find(".//stringProp[@name='filename']").text)
        self.assertEqual("javascript", jsr.find(".//stringProp[@name='scriptLanguage']").text)
        self.assertEqual("first second", jsr.find(".//stringProp[@name='parameters']").text)
        self.assertEqual("true", jsr.find(".//stringProp[@name='cacheKey']").text)

    def test_request_content_encoding(self):
        self.configure({
            "execution": {
                "scenario": {
                    'content-encoding': 'cp1251',
                    "requests": [{
                        "url": "http://blazedemo.com/",
                        "body": "S'il vous plaît",
                        "content-encoding": "utf-8",
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.modified_jmx, "rb").read())
        defaults_encoding_prop = xml_tree.find(".//ConfigTestElement/stringProp[@name='HTTPSampler.contentEncoding']")
        self.assertIsNotNone(defaults_encoding_prop)
        self.assertEqual(defaults_encoding_prop.text, 'cp1251')
        sampler_encoding_prop = xml_tree.find(".//HTTPSamplerProxy/stringProp[@name='HTTPSampler.contentEncoding']")
        self.assertIsNotNone(sampler_encoding_prop)
        self.assertEqual(sampler_encoding_prop.text, 'utf-8')

    def test_redirect_empty(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://example.com/",
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        follow_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.follow_redirects']")
        self.assertIsNotNone(follow_redirects)
        self.assertEqual(follow_redirects.text, 'true')
        auto_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.auto_redirects']")
        self.assertIsNotNone(auto_redirects)
        self.assertEqual(auto_redirects.text, 'false')

    def test_redirect_follow(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://example.com/",
                        "follow-redirects": True,
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        follow_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.follow_redirects']")
        self.assertIsNotNone(follow_redirects)
        self.assertEqual(follow_redirects.text, 'true')
        auto_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.auto_redirects']")
        self.assertIsNotNone(auto_redirects)
        self.assertEqual(auto_redirects.text, 'false')

    def test_disable_redirect(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://example.com/",
                        "follow-redirects": False,
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        follow_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.follow_redirects']")
        self.assertIsNotNone(follow_redirects)
        self.assertEqual(follow_redirects.text, 'false')
        auto_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.auto_redirects']")
        self.assertIsNotNone(auto_redirects)
        self.assertEqual(auto_redirects.text, 'false')

    def test_redirect_scenario_level(self):
        self.configure({
            "execution": {
                "scenario": {
                    "follow-redirects": False,
                    "requests": [{
                        "url": "http://example.com/",
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        follow_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.follow_redirects']")
        self.assertIsNotNone(follow_redirects)
        self.assertEqual(follow_redirects.text, 'false')
        auto_redirects = xml_tree.find(".//HTTPSamplerProxy/boolProp[@name='HTTPSampler.auto_redirects']")
        self.assertIsNotNone(auto_redirects)
        self.assertEqual(auto_redirects.text, 'false')

    def test_soapui_script(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": RESOURCES_DIR + "soapui/project.xml",
                    "test-case": "index",
                }
            }
        })
        self.obj.prepare()
        new_sc_name = "TestSuite 1-index"
        self.assertIn(new_sc_name, self.obj.engine.config["scenarios"])

        # update execution.scenario
        self.assertEqual(new_sc_name, self.obj.execution["scenario"])

        # don't parse soapui xml twice
        self.obj.engine.config["scenarios"]["project.xml"]["script"] = ""
        new_sc = self.obj.get_scenario()
        self.assertIn("requests", new_sc)

    def test_soapui_renaming(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": RESOURCES_DIR + "soapui/project.xml",
                    "test-case": "index",
                },
            },
            "scenarios": {
                "TestSuite 1-index": {
                    "hello": "world",
                },
                "TestSuite 1-index-1": {
                    "hello": "world",
                },
            },
        })
        self.obj.prepare()
        self.assertIn("TestSuite 1-index", self.obj.engine.config["scenarios"])
        self.assertIn("TestSuite 1-index-1", self.obj.engine.config["scenarios"])
        self.assertIn("TestSuite 1-index-2", self.obj.engine.config["scenarios"])

    def test_include_scenario_mutual_recursion(self):
        self.configure({
            "execution": {
                "scenario": "scen",
            },
            "scenarios": {
                "scen": {
                    "requests": [{"include-scenario": "subroutine"},
                                 {"include-scenario": "subroutine"}]
                },
                "subroutine": {"requests": ["http://blazedemo.com"]},
            },
        })
        self.obj.prepare()

    def test_include_scenario_mutual_recursion_resources(self):
        self.configure({
            "execution": {
                "scenario": "scen",
            },
            "scenarios": {
                "scen": {
                    "requests": [{"include-scenario": "subroutine"},
                                 {"include-scenario": "subroutine"}]
                },
                "subroutine": {"requests": ["http://blazedemo.com"]},
            },
        })
        self.obj.resource_files()

    def test_resource_files_relpath(self):
        self.configure({
            "execution": {
                "scenario": {
                    "script": RESOURCES_DIR + "/jmeter/jmx/nested/directory/csv.jmx"
                }
            }
        })
        resources = self.obj.get_resource_files()
        self.assertNotIn("a.csv", resources)
        self.assertTrue(any(res.endswith(os.path.join("nested", "directory", "a.csv")) for res in resources))

    def test_stdout_stderr_capture(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                self.obj.log.debug("Check...")
                time.sleep(self.obj.engine.check_interval)
            self.obj.shutdown()
            self.obj.post_process()
        except:
            pass
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "jmeter.out")))
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "jmeter.err")))

    def test_func_aggregator_chosen(self):
        self.configure(json.loads(open(RESOURCES_DIR + "json/get-post.json").read()))
        self.obj.engine.aggregator = FunctionalAggregator()
        self.obj.prepare()
        self.assertEquals('get-post', self.obj.reader.executor_label)

    def test_source_ips(self):
        self.configure({
            "execution": {
                "scenario": {
                    "random-source-ip": True,
                    "requests": [{
                        "url": "http://example.com/",
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        ip_source = xml_tree.find(".//HTTPSamplerProxy/stringProp[@name='HTTPSampler.ipSource']")
        self.assertIsNotNone(ip_source)
        self.assertIsNotNone(ip_source.text)

    def test_source_ips_request_level(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://example.com/",
                        "random-source-ip": True,
                    }]
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        ip_source = xml_tree.find(".//HTTPSamplerProxy/stringProp[@name='HTTPSampler.ipSource']")
        self.assertIsNotNone(ip_source)
        self.assertIsNotNone(ip_source.text)

    def test_diagnostics(self):
        self.configure({
            "execution": {
                "iterations": 1,
                "scenario": {
                    "requests": [{
                        "url": "http://blazedemo.com/"}]}}})
        self.obj.prepare()
        self.obj.env.set({'TEST_MODE': 'log'})
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        diagnostics = self.obj.get_error_diagnostics()
        self.assertIsNotNone(diagnostics)
        diag_str = "\n".join(diagnostics)
        self.assertIn("STDOUT message", diag_str)
        self.assertIn("STDERR message", diag_str)
        self.assertNotIn("LOG DEBUG: 1", diag_str)
        self.assertIn("LOG ERROR: 2", diag_str)
        self.assertIn("LOG DEBUG: 3", diag_str)

    def test_jmeter_version_comp(self):
        self.configure({
            "execution": {
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"
                }
            }
        })
        self.obj.settings.merge({"version": 3.3})
        self.obj.prepare()

    def test_load_defaults(self):
        self.configure({"execution": {}})
        load = self.obj.get_load()
        self.assertEqual(load.throughput, None)
        self.assertEqual(load.ramp_up, None)
        self.assertEqual(load.hold, 0)
        self.assertEqual(load.duration, 0)
        self.assertEqual(load.iterations, 1)

    def test_smartparse_property_expressions(self):
        self.configure({
            "execution": {
                "throughput": "${__foo(bar,baz,barbaz)}",
                "scenario": {
                    "script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"
                }
            }
        })
        load = self.obj.get_load()
        self.assertEqual(load.throughput, 0.0)

    @skipUnless(is_windows(), "Windows-only")
    def test_jmeter_4_windows_jmeter_home_var(self):
        self.configure({
            "execution": {
                "iterations": 1,
                "scenario": {
                    "script": RESOURCES_DIR + "/jmeter/jmx/dummy.jmx"
                }
            }
        })
        self.obj.settings.merge({"version": "4.0"})
        self.obj.prepare()
        self.obj._execute = lambda *args, **kwargs: None
        self.obj.startup()
        jmeter_home = self.obj.env.get("JMETER_HOME")
        self.assertEqual(jmeter_home, get_full_path(self.obj.settings.get("path"), step_up=2))
        self.assertEqual(jmeter_home, get_full_path(RESOURCES_DIR))

    def test_keystore_config(self):
        self.configure({
            "execution": {
                "scenario": {
                    "requests": [{
                        "url": "http://example.com/"
                    }],
                    "keystore-config": {
                        "variable-name": "certalias",
                        "start-index": 0,
                        "end-index": 99,
                        "preload": 'true'
                    }
                }
            }
        })
        self.obj.prepare()
        xml_tree = etree.fromstring(open(self.obj.original_jmx, "rb").read())
        keystore_config = xml_tree.find(".//KeystoreConfig/")
        self.assertIsNotNone(keystore_config)
        keystore_config_clientcert_alias_varname = xml_tree.find(
            ".//KeystoreConfig/stringProp[@name='clientCertAliasVarName']")
        self.assertEqual(keystore_config_clientcert_alias_varname.text, 'certalias')
        keystore_config_start_index = xml_tree.find(".//KeystoreConfig/stringProp[@name='startIndex']")
        self.assertEqual(keystore_config_start_index.text, '0')
        keystore_config_end_index = xml_tree.find(".//KeystoreConfig/stringProp[@name='endIndex']")
        self.assertEqual(keystore_config_end_index.text, '99')
        keystore_config_preload = xml_tree.find(".//KeystoreConfig/stringProp[@name='preload']")
        self.assertEqual(keystore_config_preload.text, 'true')
