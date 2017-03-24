import os
import time

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
