import os
import time

from bzt import TaurusConfigError
from bzt.modules.apiritif import ApiritifExecutor
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
                    "requests": [
                        "http://blazedemo.com/",
                        "https://api.github.com/",
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
                    "requests": [
                        "http://blazedemo.com/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("keep_alive = True", test_script)

    def test_keepalive(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "keepalive": False,
                    "requests": [
                        "http://blazedemo.com/",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("keep_alive = False", test_script)

    def test_timeout_default(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [
                        "http://blazedemo.com/",
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
                    "requests": [
                        "http://blazedemo.com/?tag=1",
                        {
                            "url": "http://blazedemo.com/?tag=2",
                            "timeout": "2s",
                        }
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("get('http://blazedemo.com/?tag=1', timeout=10.0", test_script)
        self.assertIn("get('http://blazedemo.com/?tag=2', timeout=2.0", test_script)

    def test_think_time(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [
                        {
                            "url": "http://blazedemo.com/?tag=2",
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
                    "requests": [
                        {"url": "http://blazedemo.com/?tag=get",
                         "method": "GET"},
                        {"url": "http://blazedemo.com/?tag=post",
                         "method": "POST"},
                        {"url": "http://blazedemo.com/?tag=put",
                         "method": "PUT"},
                        {"url": "http://blazedemo.com/?tag=patch",
                         "method": "PATCH"},
                        {"url": "http://blazedemo.com/?tag=head",
                         "method": "HEAD"},
                        {"url": "http://blazedemo.com/?tag=delete",
                         "method": "DELETE"},
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("get('http://blazedemo.com/?tag=get'", test_script)
        self.assertIn("post('http://blazedemo.com/?tag=post'", test_script)
        self.assertIn("put('http://blazedemo.com/?tag=put'", test_script)
        self.assertIn("patch('http://blazedemo.com/?tag=patch'", test_script)
        self.assertIn("head('http://blazedemo.com/?tag=head'", test_script)
        self.assertIn("delete('http://blazedemo.com/?tag=delete'", test_script)

    def test_default_address_path_prefix(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "default-address": "https://a.blazemeter.com",
                    "path-prefix": "/api/latest",
                    "requests": [
                        "/user",
                    ]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("self.default_address = 'https://a.blazemeter.com'", test_script)
        self.assertIn("self.path_prefix = '/api/latest'", test_script)

    def test_headers(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "headers": {"X-Foo": "foo"},
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
        self.assertIn("assertRegexInBody('Welcome')", test_script)
        self.assertIn("assertRegexInBody('Simple Travel Agency')", test_script)

    def test_plain_assertion_kinds(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [{
                        "url": "http://blazedemo.com/",
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
        self.assertIn("assertInBody('1')", test_script)
        self.assertIn("assertNotInBody('2')", test_script)
        self.assertIn("assertRegexInBody('3')", test_script)
        self.assertIn("assertRegexNotInBody('4')", test_script)
        self.assertIn("assertInHeaders('5')", test_script)
        self.assertIn("assertNotInHeaders('6')", test_script)
        self.assertIn("assertRegexInHeaders('7')", test_script)
        self.assertIn("assertRegexNotInHeaders('8')", test_script)
        self.assertIn("assertStatusCode('9')", test_script)
        self.assertIn("assertNotStatusCode('10')", test_script)

    def test_jsonpath_assertions(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [{
                        "url": "https://api.github.com/",
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
        self.assertIn("assertJSONPath('$.foo.bar', expected_value=None)", test_script)

    def test_jsonpath_assertions_kinds(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [{
                        "url": "https://api.github.com/",
                        "assert-jsonpath": [
                            {
                                "jsonpath": "$.1",
                                "invert": False,
                            },
                            {
                                "jsonpath": "$.2",
                                "invert": True,
                            },
                            {
                                "jsonpath": "$.3",
                                "expected-value": "value",
                            }
                        ]
                     }]
                }
            }]
        })
        self.obj.prepare()
        with open(self.obj.script) as fds:
            test_script = fds.read()
        self.assertIn("assertJSONPath('$.1', expected_value=None)", test_script)
        self.assertIn("assertNotJSONPath('$.2', expected_value=None)", test_script)
        self.assertIn("assertJSONPath('$.3', expected_value='value')", test_script)

    def test_xpath_assertions(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [{
                        "url": "https://api.github.com/",
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
        self.assertIn("assertXPath('//head/title', parser_type='html', validate=False)", test_script)

    def test_xpath_assertions_kinds(self):
        self.configure({
            "execution": [{
                "scenario": {
                    "requests": [{
                        "url": "https://api.github.com/",
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
        self.assertIn("assertXPath('//1', parser_type='html', validate=False)", test_script)
        self.assertIn("assertNotXPath('//2', parser_type='html', validate=False)", test_script)
        self.assertIn("assertXPath('//3', parser_type='html', validate=True)", test_script)
        self.assertIn("assertXPath('//4', parser_type='xml', validate=False)", test_script)
