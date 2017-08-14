# coding=utf-8
import os
import yaml

from bzt.engine import ScenarioExecutor
from bzt.jmx2yaml import JMX2YAML
from bzt.utils import get_full_path

from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul


class FakeOptions(object):
    def __init__(self, verbose=True, file_name=None, dump_jmx=None, quiet=False, json=False, log=False):
        self.verbose = verbose
        self.file_name = file_name
        self.dump_jmx = dump_jmx
        self.quiet = quiet
        self.json = json
        self.log = log


class TestConverter(BZTestCase):
    def setUp(self):
        super(TestConverter, self).setUp()
        self.engine = EngineEmul()
        self.obj = JMX2YAML(file_name=None, options=FakeOptions())

    def configure(self, src_file, dst_file=None, dump_jmx=None):
        self.obj.src_file = src_file
        self.obj.options = FakeOptions(file_name=dst_file, dump_jmx=dump_jmx)

    def tearDown(self):
        if self.obj.dst_file and os.path.isfile(self.obj.dst_file):
            os.remove(self.obj.dst_file)
        super(TestConverter, self).tearDown()

    @staticmethod
    def same_yaml(file1, file2):
        yml1 = yaml.load(open(file1).read())
        yml2 = yaml.load(open(file2).read())
        return yml1 == yml2

    def _get_tmp(self, prefix='test', suffix='.yml'):
        return self.engine.create_artifact(prefix, suffix)

    def test_objprop(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/http.jmx", self._get_tmp())
        self.sniff_log(self.obj.log)
        self.obj.process()
        self.assertNotIn("Removing unknown element: name (None)", self.log_recorder.warn_buff.getvalue())
        self.assertNotIn("Removing unknown element: value (None)", self.log_recorder.warn_buff.getvalue())

    def test_loadjmx1(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/http.jmx", self._get_tmp())
        self.sniff_log(self.obj.log)
        self.obj.process()
        self.assertIn("Loading jmx file", self.log_recorder.info_buff.getvalue())
        self.assertNotEqual("", self.log_recorder.debug_buff.getvalue())
        self.assertEqual("", self.log_recorder.err_buff.getvalue())

    def test_loadjmx2(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/notfound.jmx")
        self.sniff_log(self.obj.log)
        try:
            self.obj.process()
            self.fail()
        except BaseException as exc:
            self.assertIn("File does not exist", exc.args[0])
        self.assertIn("Loading jmx file", self.log_recorder.info_buff.getvalue())
        self.assertEqual("", self.log_recorder.debug_buff.getvalue())

    def test_loadjmx3(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/broken.jmx")
        self.sniff_log(self.obj.log)
        try:
            self.obj.process()
            self.fail()
        except BaseException as exc:
            self.assertIn("XML parsing failed", exc.args[0])
        self.assertIn("Loading jmx file", self.log_recorder.info_buff.getvalue())
        self.assertIn("Error while processing jmx file", self.log_recorder.err_buff.getvalue())

    def test_loadjmx4(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/http.jmx", self._get_tmp('tmp', 'file'))
        self.sniff_log(self.obj.log)
        self.obj.process()
        self.assertIn("Loading jmx file", self.log_recorder.info_buff.getvalue())
        self.assertIn("Done processing, result saved in", self.log_recorder.info_buff.getvalue())
        self.assertIn("Removing unknown element", self.log_recorder.warn_buff.getvalue())

    def test_export_clean_jmx(self):
        tmp_jmx_name = self._get_tmp('tmp', '.jmx')
        open(tmp_jmx_name, 'w+').close()  # touch file
        self.configure(RESOURCES_DIR + "yaml/converter/disabled.jmx", dump_jmx=tmp_jmx_name)
        self.sniff_log(self.obj.log)
        self.obj.process()
        self.assertIn("Loading jmx file", self.log_recorder.info_buff.getvalue())
        self.assertIn("already exists and will be overwritten", self.log_recorder.warn_buff.getvalue())

    def test_not_jmx(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/not-jmx.xml")
        try:
            self.obj.process()
            self.fail()
        except BaseException as exc:
            self.assertIn("Bad jmx format", exc.args[0])

    def test_clean_disabled_jmx(self):
        self.configure(RESOURCES_DIR + "yaml/converter/disabled.jmx", self._get_tmp())
        self.obj.process()
        disabled_elements = [element for element in self.obj.converter.dialect.tree.iter() if
                             element.get("enabled") == "false"]
        self.assertEquals(0, len(disabled_elements))

    def test_copy_global_csv_dataset(self):
        self.configure(RESOURCES_DIR + "yaml/converter/global_copy.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        datasets_first_tg = yml.get("scenarios").get("Thread Group one").get("data-sources")
        datasets_second_tg = yml.get("scenarios").get("Thread Group two").get("data-sources")

        global_csv_tg_one = [dataset for dataset in datasets_first_tg if dataset.get('path') == 'global.csv']
        global_csv_tg_two = [dataset for dataset in datasets_second_tg if dataset.get('path') == 'global.csv']

        local_csv_tg_one = [dataset for dataset in datasets_first_tg if dataset.get('path') == 'local.csv']
        local_csv_tg_two = [dataset for dataset in datasets_second_tg if dataset.get('path') == 'local.csv']
        self.assertEqual(len(global_csv_tg_one), len(global_csv_tg_two), 1)
        self.assertEqual(len(local_csv_tg_one), 1)
        self.assertEqual(len(local_csv_tg_two), 0)

    def test_parse_csv_dataset(self):
        self.configure(RESOURCES_DIR + "yaml/converter/global_copy.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        datasets = yml.get("scenarios").get("Thread Group one").get("data-sources")
        local_csv = [dataset for dataset in datasets if dataset.get('path') == 'local.csv'][0]
        self.assertEqual(local_csv['loop'], False)
        self.assertEqual(local_csv['delimiter'], ',')
        self.assertEqual(local_csv['quoted'], False)

    def test_copy_global_headers(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/global_copy.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        headers_first_tg = yml.get("scenarios").get("Thread Group one").get("headers", [])
        headers_second_tg = yml.get("scenarios").get("Thread Group two").get("headers", [])
        self.assertEqual(len(headers_first_tg), 3)
        self.assertEqual(len(headers_second_tg), 2)

    def test_cache_cookie_dns_overrides(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/global_copy.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        tg_one = yml.get("scenarios").get('Thread Group one')
        tg_two = yml.get("scenarios").get('Thread Group two')
        cache_first_tg = tg_one.get("store-cache")
        cache_second_tg = tg_two.get("store-cache")
        cookie_first_tg = tg_one.get("store-cookie")
        cookie_second_tg = tg_two.get("store-cookie")
        dns_cache_mgr_first_tg = tg_one.get("use-dns-cache-mgr")
        dns_cache_mgr_second_tg = tg_two.get("use-dns-cache-mgr")
        self.assertEqual(cache_first_tg, True)
        self.assertEqual(cache_second_tg, True)
        self.assertEqual(cookie_first_tg, False)
        self.assertEqual(cookie_second_tg, True)
        self.assertEqual(dns_cache_mgr_first_tg, True)
        self.assertEqual(dns_cache_mgr_second_tg, True)

    def test_think_time_overrides(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/global_copy.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        tg_one = yml.get("scenarios").get('Thread Group one')
        tg_two = yml.get("scenarios").get('Thread Group two')
        request_tg_two = tg_two.get("requests")[0]
        tg_one_timer = tg_one.get("think-time")
        tg_two_timer = tg_two.get("think-time")
        req_timer = request_tg_two.get("think-time")

        self.assertEqual(tg_one_timer, "200ms")
        self.assertEqual(tg_two_timer, "300ms")
        self.assertEqual(req_timer, "100ms")

    def test_request_defaults(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/global_copy.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        tg_one = yml.get("scenarios").get('Thread Group one')
        tg_two = yml.get("scenarios").get('Thread Group two')
        self.assertEqual(tg_one.get("default-address"), "https://127.0.0.2/")
        self.assertEqual(tg_two.get("default-address"), "http://127.0.0.3:2582/resources/")
        self.assertEqual(tg_one.get("timeout"), "500ms")
        self.assertEqual(tg_two.get("timeout"), "100ms")
        self.assertEqual(tg_one.get("retrieve-resources"), True)
        self.assertEqual(tg_two.get("retrieve-resources"), True)
        self.assertEqual(tg_one.get("concurrent-pool-size"), 5)
        self.assertEqual(tg_two.get("concurrent-pool-size"), 10)

    def test_copy_global_request_assertions(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/assertions.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        tg_one = yml.get("scenarios").get("tg1")
        tg_two = yml.get("scenarios").get("tg2")
        tg_one_assertions = tg_one.get("assert")
        self.assertEqual(len(tg_one_assertions), 2)  # global assertion + tg assertion
        tg_two_assertions = tg_two.get("assert")
        self.assertEqual(len(tg_two_assertions), 1)  # global only assertion
        tg_one_req_one_assertion = tg_one.get("requests")[0].get("assert")[0]
        expected = {"subject": "headers", "contains": ["tg1httpreq1", "tg1httpreq12"],
                    "assume-success": False, "not": False, "regexp": False}
        self.assertEqual(tg_one_req_one_assertion, expected)
        tg_one_assertion = tg_one.get("assert")[0]
        expected = {"subject": "body", "contains": ["tg1body_text_not_contains"],
                    "assume-success": False, "not": True, 'regexp': False}
        self.assertEqual(tg_one_assertion, expected)

    def test_broken_request_assertions(self):
        # see comments in broken_resp_asserts.jmx for explanation of cases
        # don't save broken_resp_asserts.jmx by jmeter
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/broken_resp_asserts.jmx", yml)
        self.obj.process()
        yml1 = RESOURCES_DIR + "yaml/converter/broken_resp_asserts.yml"
        yml2 = yml
        self.assertTrue(yml1, yml2)

    def test_copy_global_json_assertions(self):
        self.configure(RESOURCES_DIR + "yaml/converter/assertions.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        tg_one = yml.get("scenarios").get("tg1")
        tg_two = yml.get("scenarios").get("tg2")
        tg_one_assertions = tg_one.get("assert-jsonpath")
        self.assertEqual(len(tg_one_assertions), 1)  # global assertion + tg assertion
        tg_two_assertions = tg_two.get("assert-jsonpath")
        self.assertEqual(len(tg_two_assertions), 1)  # global only assertion
        tg_one_req_one_jp = tg_one.get("requests")[0].get("assert-jsonpath", [])  # no assertions
        self.assertEqual(len(tg_one_req_one_jp), 0)
        tg_two_req_one_jp = tg_two.get("requests")[0].get("assert-jsonpath", [])
        self.assertEqual(len(tg_two_req_one_jp), 1)
        expected = {"expect-null": True, "invert": True, "jsonpath": '$(":input")', "validate": True, "regexp": True}
        self.assertEqual(expected, tg_two_req_one_jp[0])
        #  test concurrency, ramp-up, iterations in execution
        tg_one_exec = yml.get(ScenarioExecutor.EXEC)[0]
        tg_two_exec = yml.get(ScenarioExecutor.EXEC)[1]
        tg_three_exec = yml.get(ScenarioExecutor.EXEC)[2]
        self.assertEqual(tg_one_exec.get("concurrency"), 10)
        self.assertEqual(tg_two_exec.get("concurrency"), 15)
        self.assertEqual(tg_three_exec.get("concurrency"), 1)
        self.assertEqual(tg_one_exec.get("ramp-up"), '10s')
        self.assertEqual(tg_two_exec.get("ramp-up"), '60s')
        self.assertEqual(tg_three_exec.get("ramp-up"), '2s')
        self.assertEqual(tg_one_exec.get("iterations"), 1)
        self.assertEqual(tg_two_exec.get("iterations"), 1)
        self.assertEqual(tg_three_exec.get("iterations"), 100)

    def test_xpath_assertions(self):
        self.configure(RESOURCES_DIR + "yaml/converter/assertions.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        tg = yml.get("scenarios").get("tg3")
        assertions = tg.get("assert-xpath")
        self.assertEqual(len(assertions), 2)
        self.assertEqual(assertions[0], {
            "xpath": "/note/to",
            "ignore-whitespace": False,
            "invert": False,
            "validate-xml": False,
            "use-tolerant-parser": False,
        })
        self.assertEqual(assertions[1], {
            "xpath": "/note/from",
            "ignore-whitespace": True,
            "invert": True,
            "validate-xml": True,
            "use-tolerant-parser": True,
        })

    def test_extractors(self):
        self.configure(RESOURCES_DIR + "yaml/converter/extractors.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        tg_one = yml.get("scenarios").get("tg1")
        tg_two = yml.get("scenarios").get("tg2")
        tg_three = yml.get("scenarios").get("tg3")
        tg_one_extractors = tg_one.get("extract-regexp")
        tg_two_extractors = tg_two.get("extract-regexp")
        self.assertEqual(len(tg_one_extractors), 1)  # global
        self.assertEqual(len(tg_two_extractors), 1)  # global + local - ignored
        tg_one_req_exr = tg_one.get("requests")[0].get("extract-regexp", {})
        self.assertEqual(len(tg_one_req_exr), 2)
        expected = {'template': '1', 'match-no': 1, 'regexp': '*tg1hr1', 'default': 'default'}
        self.assertEqual(expected, tg_one_req_exr.get("test_tg1hr1"))
        # test extract-jsonpath
        tg_one_extractors = tg_one.get("extract-jsonpath")
        tg_two_extractors = tg_two.get("extract-jsonpath")
        self.assertEqual(len(tg_one_extractors), 5)  # 4x global + local
        self.assertEqual(len(tg_two_extractors), 4)  # 4x global
        tg_three_req_exr = tg_three.get("requests")[0].get("extract-jsonpath", {})
        self.assertEqual(len(tg_three_req_exr), 1)  # 1x local
        # test extract-xpath
        tg_three_extractors = tg_three.get("extract-xpath")
        self.assertEqual(len(tg_three_extractors), 2)  # 2 global
        self.assertEqual(tg_three_extractors['bookAuthor'], {
            "xpath": "/books/[@title()='1984']/author",
            "default": "no_author",
            "ignore-whitespace": False,
            "validate-xml": False,
            "use-tolerant-parser": False,
        })
        self.assertEqual(tg_three_extractors['author'], {
            "xpath": "/books/[@title()='Fahrenheit 451']/author",
            "default": "no",
            "ignore-whitespace": True,
            "validate-xml": True,
            "use-tolerant-parser": False,
        })
        self.assertEqual(tg_one_extractors['VAR1'], {
            "jsonpath": "$.foo",
            "default": "DEF_1",
        })
        self.assertEqual(tg_one_extractors['VAR2'], {
            "jsonpath": "$.bar",
            "default": "DEF_2",
        })

    def test_request_body(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/extractors.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        tg_one = yml.get("scenarios").get("tg1")
        tg_two = yml.get("scenarios").get("tg2")
        tg_one_req_one_body = tg_one.get("requests")[0].get("body")
        self.assertEqual(tg_one_req_one_body, "body-string")
        tg_one_req_one_body = tg_one.get("requests")[1].get("body")
        self.assertEqual(tg_one_req_one_body, {"body_param1": "value1", "body_param2": "value2"})
        tg_two_req_one_body = tg_two.get("requests")[0].get("body")
        self.assertEqual(tg_two_req_one_body, None)

    def test_json_body(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/json_body.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        reqs1 = yml.get("scenarios").get("tg1")['requests']
        reqs2 = yml.get("scenarios").get("tg2")['requests']
        bodies = {req['label']: req.get('body', None) for req in reqs1 + reqs2}
        targets = {'r1_1': None, 'r1_2': list, 'r1_3': str, 'r1_4': dict,
                   'r2_1': None, 'r2_2': dict, 'r2_3': str, 'r2_4': str}
        for label in targets:
            self.assertTrue((bodies[label] is None and targets[label] is None)
                            or isinstance(bodies[label], targets[label]))

    def test_duration_throughput(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/duration.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        tg_one = yml.get(ScenarioExecutor.EXEC)[0]
        tg_two = yml.get(ScenarioExecutor.EXEC)[1]
        tg_three = yml.get(ScenarioExecutor.EXEC)[2]
        self.assertEqual("10s", tg_one.get("ramp-up"))
        self.assertEqual("60s", tg_one.get("hold-for"))
        self.assertEqual("10s", tg_one.get("ramp-up"))
        self.assertEqual(100, tg_one.get("throughput"))
        self.assertEqual("10s", tg_two.get("ramp-up"))
        self.assertEqual("20s", tg_two.get("hold-for"))
        self.assertEqual(20, tg_two.get("throughput"))
        self.assertEqual("60s", tg_three.get("ramp-up"))
        self.assertEqual("40s", tg_three.get("hold-for"))
        self.assertEqual(100, tg_three.get("throughput"))

    def test_all(self):
        self.configure(RESOURCES_DIR + "yaml/converter/disabled.jmx")
        self.obj.process()
        yml1 = RESOURCES_DIR + "yaml/converter/disabled.yml"
        yml2 = self.obj.dst_file
        self.assertTrue(self.same_yaml(yml1, yml2))

    def test_params_conversion(self):
        self.configure(RESOURCES_DIR + "yaml/converter/params_conversion.jmx")
        self.sniff_log(self.obj.log)
        self.obj.process()
        yml1 = self.obj.dst_file
        yml2 = RESOURCES_DIR + "yaml/converter/params_conversion.yml"
        self.assertTrue(self.same_yaml(yml1, yml2))
        self.assertNotIn('n1', self.log_recorder.warn_buff.getvalue())
        self.assertNotIn('n2', self.log_recorder.warn_buff.getvalue())
        self.assertIn('n1_101', self.log_recorder.debug_buff.getvalue())
        self.assertIn('n1_011', self.log_recorder.debug_buff.getvalue())
        self.assertIn('n1_001', self.log_recorder.debug_buff.getvalue())

    def test_param_null(self):
        self.configure(RESOURCES_DIR + "yaml/converter/param-null.jmx")
        self.obj.process()

    def test_load_profile_default_values(self):
        yml = self._get_tmp()
        self.configure(RESOURCES_DIR + "yaml/converter/default.jmx", yml)
        self.obj.process()
        yml = yaml.load(open(yml).read())
        execution = yml.get(ScenarioExecutor.EXEC)[0]
        self.assertEqual("60s", execution.get("ramp-up"))
        self.assertEqual("60s", execution.get("hold-for"))
        self.assertEqual(1, execution.get("concurrency"))
        self.assertEqual(1, execution.get("iterations"))

    def test_variables(self):
        self.configure(RESOURCES_DIR + "yaml/converter/vars.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        scenarios = yml.get("scenarios")
        tg_one = scenarios["TG1"]
        self.assertEqual(tg_one.get('variables'),
                         {"tg1_local": "tg1", "global_var": "global", "auth_token": "shouldn't be masked"})
        tg_two = scenarios["TG2"]
        self.assertEqual(tg_two.get('variables'),
                         {"tg2_local": "tg2", "global_var": "global", "auth_token": "shouldn't be masked"})

    def test_no_variables(self):
        self.configure(RESOURCES_DIR + "yaml/converter/default.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        execution = yml.get(ScenarioExecutor.EXEC)[0]
        scenarios = yml.get("scenarios")
        scenario = scenarios[execution.get("scenario")]
        self.assertNotIn("variables", scenario)

    def test_controllers_to_requests(self):
        self.configure(RESOURCES_DIR + "yaml/converter/controllers.jmx")
        self.obj.process()
        yml1 = RESOURCES_DIR + "yaml/converter/controllers.yml"
        yml2 = self.obj.dst_file
        self.assertTrue(self.same_yaml(yml1, yml2))

    def test_jsr223(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/jsr223.jmx")
        try:
            self.obj.process()
            yml = yaml.load(open(self.obj.dst_file).read())
            scenarios = yml.get("scenarios")
            scenario = scenarios["Thread Group"]
            requests = scenario["requests"]
            self.assertEqual(len(requests), 1)
            request = requests[0]
            self.assertIn("jsr223", request)
            jsrs = request["jsr223"]
            self.assertIsInstance(jsrs, list)
            self.assertEqual(len(jsrs), 5)
            self.assertEqual(jsrs[0]["language"], "beanshell")
            self.assertEqual(jsrs[0]["script-text"], "scripty")
            self.assertEqual(jsrs[0]["parameters"], "parames")
            self.assertNotIn('script-file', jsrs[0])
            self.assertEqual(jsrs[1]["language"], "javascript")
            self.assertEqual(jsrs[1]["script-text"], u'console.log("ПРИВЕТ");\nline("2");')
            self.assertEqual(jsrs[1]["parameters"], "a b c")
            self.assertNotIn('script-file', jsrs[1])
            self.assertEqual(jsrs[2]["language"], "javascript")
            self.assertEqual(jsrs[2]["script-file"], "script.js")
            self.assertEqual(jsrs[2]["parameters"], None)
            self.assertNotIn('script-text', jsrs[2])
            self.assertEqual(jsrs[3]["language"], "beanshell")
            self.assertEqual(jsrs[3]["execute"], "before")
            self.assertEqual(jsrs[3]["parameters"], None)
            self.assertEqual(jsrs[3]['script-text'], 'console.log("beanshell aka jsr223");')
            self.assertNotIn('script-file', jsrs[3])
            self.assertEqual(jsrs[4]["language"], "java")
            self.assertEqual(jsrs[4]["execute"], "before")
            self.assertEqual(jsrs[4]["parameters"], None)
            self.assertIn('BlazeDemo.java', jsrs[4]['script-file'])
            self.assertNotIn('script-text', jsrs[4])
            self.assertTrue(os.path.exists(os.path.join(get_full_path(self.obj.dst_file, step_up=1), 'script.js')))
        finally:
            os.remove(os.path.join(get_full_path(self.obj.dst_file, step_up=1), 'script.js'))

    def test_unicode(self):
        self.configure(RESOURCES_DIR + "yaml/converter/unicode.jmx")
        self.obj.process()

    def test_path_without_domain(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/http.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        scenarios = yml.get("scenarios")
        scenario = scenarios["Thread Group"]
        requests = scenario["requests"]
        self.assertEqual(len(requests), 3)
        without_domain = requests[2]
        self.assertEqual(without_domain['url'], '/path')

    def test_request_content_encoding(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/http.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        scenarios = yml.get("scenarios")
        scenario = scenarios["Thread Group"]
        requests = scenario["requests"]
        self.assertEqual(len(requests), 3)
        request = requests[1]
        self.assertEqual(request['content-encoding'], 'utf-8')

    def test_request_redirect_policy(self):
        self.configure(RESOURCES_DIR + "jmeter/jmx/http.jmx")
        self.obj.process()
        yml = yaml.load(open(self.obj.dst_file).read())
        scenarios = yml.get("scenarios")
        scenario = scenarios["Thread Group"]
        requests = scenario["requests"]
        self.assertEqual(len(requests), 3)
        self.assertEqual(requests[0].get('follow-redirects'), True)
        self.assertEqual(requests[1].get('follow-redirects'), True)
        self.assertEqual(requests[2].get('follow-redirects'), False)
