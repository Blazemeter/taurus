import logging
import os
import time

from bzt import TaurusConfigError
from bzt.modules.apiritif import ApiritifExecutor, ApiritifResultsReader
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestApiritifExecutor(BZTestCase):
    def setUp(self):
        self.obj = ApiritifExecutor()
        self.obj.engine = EngineEmul()

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution.merge(self.obj.engine.config["execution"][0])

    def test_basic(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "script": __dir__() + "/../apiritif/test_api_example.py"
                }
            }]
        })
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_requests(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "default-address": "http://blazedemo.com",
                    "requests": [
                        "/",
                        "/reserve.php",
                    ]
                }
            }]
        })
        self.obj.prepare()
        self.assertTrue(self.obj.generated_script)
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, "test_api.py")))
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_keepalive_default(self):
        self.configure({
            "execution": [{
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
        self.assertIn("target.keep_alive(True)", test_script)

    def test_keepalive(self):
        self.configure({
            "execution": [{
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
        self.assertIn("timeout=30.0", test_script)

    def test_timeout(self):
        self.configure({
            "execution": [{
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
        self.assertIn("get('/?tag=1', timeout=10.0", test_script)
        self.assertIn("get('/?tag=2', timeout=2.0", test_script)

    def test_think_time(self):
        self.configure({
            "execution": [{
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
        self.assertIn("time.sleep(1.5)", test_script)

    def test_methods(self):
        self.configure({
            "execution": [{
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
        self.assertIn("allow_redirects=True", test_script)

    def test_follow_redirects(self):
        self.configure({
            "execution": [{
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
        self.assertIn("params={'foo': 'bar'}", test_script)

    def test_body_json(self):
        self.configure({
            "execution": [{
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
        self.assertIn("json={'foo': 'bar'}", test_script)

    def test_body_string(self):
        self.configure({
            "execution": [{
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

    def test_assertions_exec(self):
        self.configure({
            'execution': [{
                'iterations': 1,
                'scenario': {
                    'default-address': "http://blazedemo.com",
                    'requests': [
                        {'assert': [
                            {'contains': [200],
                             'subject': 'http-code'},
                            {'contains': ['Welcome to the Simple Travel Agency!'],
                             'subject': 'body'}],
                         'url': '/'},
                        {'assert-xpath': [{'use-tolerant-parser': True,
                                           'validate-xml': False,
                                           'xpath': '//head/title'}],
                         'url': '/'}]}}]})
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

        reader = ApiritifResultsReader(self.obj.report_path, self.obj.engine, logging.getLogger(''), [])
        samples = list(reader.read(last_pass=True))

    def test_assertion_jsonpath_exec(self):
        self.configure({
            'execution': [{
                'iterations': 1,
                'scenario': {
                    'default-address': "https://api.github.com",
                    'requests': [
                        {'assert-jsonpath': [{'expected-value': 'Linus Gustav Larsson Thiel',
                                              'jsonpath': '$.name'}],
                         'headers': {'User-Agent': "Biggie/Smalls"},
                         'url': '/users/linus'}]}}]})
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

        reader = ApiritifResultsReader(self.obj.report_path, self.obj.engine, logging.getLogger(''), [])
        samples = list(reader.read(last_pass=True))
        self.assertEqual(1, len(samples))

