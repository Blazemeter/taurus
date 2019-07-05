import logging
import tempfile

from bzt import TaurusConfigError
from bzt.modules import ConsolidatingAggregator
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.apiritif import ApiritifNoseExecutor
from bzt.modules.apiritif.executor import ApiritifLoadReader, ApiritifFuncReader
from tests import RESOURCES_DIR, ExecutorTestCase
from tests.mocks import EngineEmul


class TestApiritifScriptGeneration(ExecutorTestCase):
    EXECUTOR = ApiritifNoseExecutor

    def test_transactions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "requests": [
                        "url_0",
                        {"transaction": "t_1", "do": [
                            "url_1.0",
                            {"url": "url_1.1", "headers": {"o": "ne", "t": "wo"}}]},
                        {"transaction": "t_2", "do": [
                            {"url": "url_2.0", "think-time": 2},
                            {"transaction": "t_22", "do": [
                                {"url": "url_22.0", "think-time": 3}]}]}]}}]})
        self.obj.prepare()

        exp_file = RESOURCES_DIR + "apiritif/test_transactions.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_keepalive_only(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "keepalive": True,
                    "requests": [
                        "http://blazedemo.com/"]}}]})
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target = apiritif.http.target('')", test_script)

    def test_keepalive(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "keepalive": False,
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target.keep_alive(False)", test_script)

    def test_timeout_default(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertNotIn("timeout=30.0", test_script)

    def test_timeout(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "timeout": "10s",
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?tag=1",
                        {
                            "url": "/?tag=2",
                            "timeout": "2s",
                        }
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target.timeout(10.0)", test_script)
        self.assertNotIn("get('/?tag=1', timeout=10.0", test_script)
        self.assertIn("get('/?tag=2', timeout=2.0", test_script)

    def test_timeout_notarget(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "timeout": "10s",
                    "requests": [
                        "http://blazedemo.com/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("get('http://blazedemo.com/', timeout=10.0", test_script)

    def test_think_time(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        {
                            "url": "/?tag=2",
                            "think-time": "1s500ms",
                        }
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("sleep(1.5)", test_script)

    def test_methods(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        {"url": "/?tag=get",
                         "method": "GET"},
                        {"url": "/?tag=post",
                         "method": "POST"},
                        {"url": "/?tag=put",
                         "method": "PUT"},
                        {"url": "/?tag=patch",
                         "method": "PATCH"},
                        {"url": "/?tag=head",
                         "method": "HEAD"},
                        {"url": "/?tag=delete",
                         "method": "DELETE"},
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("get('/?tag=get'", test_script)
        self.assertIn("post('/?tag=post'", test_script)
        self.assertIn("put('/?tag=put'", test_script)
        self.assertIn("patch('/?tag=patch'", test_script)
        self.assertIn("head('/?tag=head'", test_script)
        self.assertIn("delete('/?tag=delete'", test_script)

    def test_default_address_path_prefix(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://a.blazemeter.com",
                    "base-path": "/api/latest",
                    "requests": [
                        "/user",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target('https://a.blazemeter.com')", test_script)
        self.assertIn("target.base_path('/api/latest')", test_script)

    def test_headers(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "headers": {"X-Foo": "foo"},
                    "requests": [{
                        "url": "/",
                        "headers": {"X-Bar": "bar"}
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("'X-Foo': 'foo'", test_script)
        self.assertIn("'X-Bar': 'bar'", test_script)

    def test_follow_redirects_default(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("target.allow_redirects(True)", test_script)
        self.assertNotIn("allow_redirects=True", test_script)

    def test_follow_redirects(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "follow-redirects": False,
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("allow_redirects=False", test_script)

    def test_body_params(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "body": {
                            "foo": "bar",
                        },
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("params={\n                'foo': 'bar',\n            }", test_script)

    def test_body_json(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "headers": {
                            "Content-Type": "application/json",
                        },
                        "body": {
                            "foo": "bar",
                        },
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("json={\n                'foo': 'bar',\n            }", test_script)

    def test_body_string(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "body": "MY PERFECT BODY"
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("data='MY PERFECT BODY'", test_script)

    def test_body_unknown(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "body": 123
                    }]
                }
            }]
        })
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_plain_assertions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "assert": [
                            "Welcome", "Simple Travel Agency"
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("response.assert_regex_in_body('Welcome')", test_script)
        self.assertIn("response.assert_regex_in_body('Simple Travel Agency')", test_script)

    def test_plain_assertion_kinds(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "assert": [
                            {"contains": ["1"], "regexp": False, "not": False},
                            {"contains": ["2"], "regexp": False, "not": True},
                            {"contains": ["3"], "regexp": True, "not": False},
                            {"contains": ["4"], "regexp": True, "not": True},
                            {"contains": ["5"], "regexp": False, "not": False, "subject": "headers"},
                            {"contains": ["6"], "regexp": False, "not": True, "subject": "headers"},
                            {"contains": ["7"], "regexp": True, "not": False, "subject": "headers"},
                            {"contains": ["8"], "regexp": True, "not": True, "subject": "headers"},
                            {"contains": ["8"], "regexp": True, "not": True, "subject": "headers"},
                            {"contains": ["9"], "not": False, "subject": "http-code"},
                            {"contains": ["10"], "not": True, "subject": "http-code"},
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assert_in_body('1')", test_script)
        self.assertIn("assert_not_in_body('2')", test_script)
        self.assertIn("assert_regex_in_body('3')", test_script)
        self.assertIn("assert_regex_not_in_body('4')", test_script)
        self.assertIn("assert_in_headers('5')", test_script)
        self.assertIn("assert_not_in_headers('6')", test_script)
        self.assertIn("assert_regex_in_headers('7')", test_script)
        self.assertIn("assert_regex_not_in_headers('8')", test_script)
        self.assertIn("assert_status_code('9')", test_script)
        self.assertIn("assert_not_status_code('10')", test_script)

    def test_jsonpath_assertions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-jsonpath": [
                            "$.foo.bar"
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assert_jsonpath('$.foo.bar', expected_value=None)", test_script)

    def test_jsonpath_assertions_kinds(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-jsonpath": [
                            {"jsonpath": "$.1", "invert": False},
                            {"jsonpath": "$.2", "invert": True},
                            {"jsonpath": "$.3", "expected-value": "value"},
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assert_jsonpath('$.1', expected_value=None)", test_script)
        self.assertIn("assert_not_jsonpath('$.2', expected_value=None)", test_script)
        self.assertIn("assert_jsonpath('$.3', expected_value='value')", test_script)

    def test_xpath_assertions(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-xpath": [
                            "//head/title"
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assert_xpath('//head/title', parser_type='html', validate=False)", test_script)

    def test_xpath_assertions_kinds(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "https://api.github.com",
                    "requests": [{
                        "url": "/",
                        "assert-xpath": [
                            {"xpath": "//1", "invert": False},
                            {"xpath": "//2", "invert": True},
                            {"xpath": "//3", "validate-xml": True},
                            {"xpath": "//4", "validate-xml": False, "use-tolerant-parser": False},
                        ]
                    }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assert_xpath('//1', parser_type='html', validate=False)", test_script)
        self.assertIn("assert_not_xpath('//2', parser_type='html', validate=False)", test_script)
        self.assertIn("assert_xpath('//3', parser_type='html', validate=True)", test_script)
        self.assertIn("assert_xpath('//4', parser_type='xml', validate=False)", test_script)

    def test_complex_codegen(self):
        """ This test serves code review purposes, to make changes more visible """
        self.obj.engine.config.load([RESOURCES_DIR + 'apiritif/test_codegen.yml'])
        self.configure(self.obj.engine.config['execution'][0])
        self.obj.settings['verbose'] = True
        self.obj.prepare()
        exp_file = RESOURCES_DIR + 'apiritif/test_codegen.py'
        # import shutil; shutil.copy2(self.obj.script, exp_file)  # keep this comment to ease updates
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_jmeter_functions_time(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?time=${__time()}",
                        "/?time=${__time(MM/dd/yy)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("'/?time={}'.format(apiritif.format_date())", test_script)
        self.assertIn("'/?time={}'.format(apiritif.format_date('MM/dd/yy'))", test_script)

    def test_jmeter_functions_random(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?random=${__Random(1, 10)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("'/?random={}'.format(apiritif.random_uniform(1, 10))", test_script)

    def test_jmeter_functions_random_string(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/?rs=${__RandomString(3)}",
                        "/?rs=${__RandomString(4,abcdef)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("'/?rs={}'.format(apiritif.random_string(3))", test_script)
        self.assertIn("'/?rs={}'.format(apiritif.random_string(4, 'abcdef'))", test_script)

    def test_jmeter_functions_base64_encode(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "headers": {
                        "Authorization": "Basic ${__base64Encode(user:pass)}",
                    },
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("base64_encode('user:pass')", test_script)

    def test_jmeter_functions_base64_decode(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "headers": {
                        "Additional": "${__base64Decode(dGVzdCBzdHJpbmc=)}",
                    },
                    "requests": [
                        "/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("base64_decode('dGVzdCBzdHJpbmc=')", test_script)

    def test_jmeter_functions_urlencode(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/${__urlencode(Foo Bar Baz)}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("encode_url('Foo Bar Baz')", test_script)

    def test_jmeter_functions_uuid(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/${__UUID()}",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("uuid()", test_script)

    def test_load_reader(self):
        reader = ApiritifLoadReader(self.obj.log)

        # add empty reader
        with tempfile.NamedTemporaryFile() as f_name:
            reader.register_file(f_name.name)
            items = list(reader.datapoints(True))

        self.assertEqual(len(items), 0)
        self.assertFalse(reader.read_records)
        reader.register_file(RESOURCES_DIR + "jmeter/jtl/tranctl.jtl")
        items = list(reader.datapoints(True))
        self.assertEqual(len(items), 1)
        items = list(reader.datapoints(True))
        self.assertEqual(len(items), 0)
        reader.register_file(RESOURCES_DIR + "jmeter/jtl/tranctl.jtl")
        reader.register_file(RESOURCES_DIR + "jmeter/jtl/tranctl.jtl")
        items = list(reader.datapoints(True))
        self.assertTrue(reader.read_records)
        self.assertEqual(len(items), 1)

    def test_load_reader_real2(self):
        reader1 = ApiritifLoadReader(self.obj.log)
        reader1.engine = EngineEmul()
        reader1.register_file(RESOURCES_DIR + "jmeter/jtl/apiritif-results/apiritif-0.csv")
        reader1.register_file(RESOURCES_DIR + "jmeter/jtl/apiritif-results/apiritif-1.csv")

        reader2 = ApiritifLoadReader(self.obj.log)
        reader2.engine = EngineEmul()
        reader2.register_file(RESOURCES_DIR + "jmeter/jtl/apiritif-results/apiritif--10.csv")
        reader2.register_file(RESOURCES_DIR + "jmeter/jtl/apiritif-results/apiritif--11.csv")

        reader = ConsolidatingAggregator()
        reader.engine = EngineEmul()
        reader.add_underling(reader1)
        reader.add_underling(reader2)

        items = list(reader.datapoints())
        self.assertEqual(0, len(items))

        all_items = []
        while True:
            items = list(reader.datapoints())
            all_items.extend(items)
            if not items:
                break

            for point in items:
                cnc = point[DataPoint.CURRENT][''][KPISet.CONCURRENCY]
                logging.info("%s: %s", point[DataPoint.TIMESTAMP], cnc)
                self.assertLessEqual(cnc, 4)
                cnc1 = point[DataPoint.CUMULATIVE][''][KPISet.CONCURRENCY]
                self.assertLessEqual(cnc1, 4)

        self.assertEqual(4, all_items[-1][DataPoint.CURRENT][''][KPISet.CONCURRENCY])

    def test_func_reader(self):
        reader = ApiritifFuncReader(self.obj.engine, self.obj.log)
        items = list(reader.read())
        self.assertEqual(len(items), 0)
        reader.register_file(RESOURCES_DIR + "apiritif/transactions.ldjson")
        reader.register_file(RESOURCES_DIR + "apiritif/transactions.ldjson")
        items = list(reader.read())
        self.assertEqual(len(items), 18)

    def test_data_sources(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "variables": {"cn": "cv"},
                    "default-address": "http://localhost:8000/",
                    "requests": ["${an}", "${bn}", "${cn}"],
                    "data-sources": [
                        "first-file.csv", {
                            "path": "/second/file.csv",
                            "delimiter": "-",
                            "loop": True,
                            "quoted": False,
                            "variable-names": "bn, bbn"}]}}]})

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "/apiritif/test_data_sources.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_vars(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "variables": {"an": "av"},
                    "default-address": "http://localhost:8000/",
                    "requests": [
                        "${an}",
                        {"set-variables": {
                            "an": "another_path1",
                            "bn": "another_path2"}},
                        "${an}"],
                }}]})

        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "/apiritif/test_vars.py"
        self.assertIn("test_2_set_variables", self.obj.engine.aggregator.ignored_labels)
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_codegen_requests(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "requests": [{
                        "url": "http://localhost:8000/",
                        "label": "apiritif",
                    }]
                }
            }]
        })
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "/apiritif/test_codegen_requests.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_generator_crash(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "variables": {
                        "product_id": "5b6c",
                    },
                    "requests": [{
                        "url": "/",
                        "method": "POST",
                        "body": {
                            "product": "${product_id}"  # notice the space
                        }
                    }]
                }
            }]
        })
        self.obj.prepare()  # Unparser shouldn't crash with AttributeError because of malformed AST
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("data=[('product', self.vars['product_id'])]", test_script)

    def test_inherit_test_case(self):
        self.configure({
            "execution": [{
                "test-mode": "apiritif",
                "scenario": {
                    "requests": [
                        "http://example.com/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.obj.log.info(test_script)
        self.assertIn("class TestAPI(unittest.TestCase", test_script)

