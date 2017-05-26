import os
import sys
import json
import shutil

from bzt import TaurusConfigError
from bzt.modules.proxy2jmx import Proxy2JMX, BZAProxy
from bzt.modules.selenium import SeleniumExecutor
from bzt.utils import is_windows, get_full_path
from tests import BZTestCase
from tests.mocks import EngineEmul
from os.path import join


class BZAProxyEmul(BZAProxy):
    def __init__(self, service):
        super(BZAProxyEmul, self).__init__()
        self.service = service

    def _request(self, url, data=None, headers=None, method=None, raw_result=False):
        response = self.service.responses.pop(0)
        resp = response.text
        if raw_result:
            return resp
        else:
            return json.loads(resp) if len(resp) else {}


class Proxy2JMXEmul(Proxy2JMX):
    responses = []

    def __init__(self):
        super(Proxy2JMXEmul, self).__init__()
        self.proxy = BZAProxyEmul(self)


class ResponseEmul(object):
    def __init__(self, status_code, text):
        self.status_code = status_code
        self.text = text


class TestProxy2JMX(BZTestCase):
    def setUp(self):
        super(TestProxy2JMX, self).setUp()
        self.obj = Proxy2JMXEmul()
        self.obj.engine = EngineEmul()
        res_dir = join(get_full_path(__file__, step_up=3), 'bzt', 'resources')
        src = join(res_dir, 'chrome-loader.c')
        dst_loader = join(res_dir, 'chrome-loader.exe')
        shutil.copy2(src, dst_loader)

    def tearDown(self):
        res_dir = join(get_full_path(__file__, step_up=3), 'bzt', 'resources')
        dst_loader = join(res_dir, 'chrome-loader.exe')
        os.remove(dst_loader)
        super(TestProxy2JMX, self).tearDown()

    def test_no_token(self):
        self.obj.settings = self.obj.engine.config.get('recorder')
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_full(self):
        self.obj.api_delay = 1
        self.obj.responses = [
            ResponseEmul(200, '{"result" : {}}'),
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1"}}'),
            ResponseEmul(200, ''),
            ResponseEmul(200, ''),  # startup: startRecording
            ResponseEmul(200, ''),  # shutdown: stopRecording
            ResponseEmul(200, 'regular jmx contents'),
            ResponseEmul(200, '{"result" : {"smartjmx": "unavailable"}}'),
            ResponseEmul(200, '{"result" : {"smartjmx": "available"}}'),
            ResponseEmul(200, 'only one string')]

        self.obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        self.obj.settings = self.obj.engine.config.get('modules').get('recorder')

        self.obj.prepare()
        self.assertEqual(self.obj.proxy_addr, 'http://host1:port1')
        self.obj.engine.provisioning.executors = [SeleniumExecutor()]
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

        with open(self.obj.engine.artifacts_dir + '/generated.smart.jmx') as fd:
            lines = fd.readlines()
            self.assertEqual(len(lines), 1)
            self.assertEqual(lines[0].strip(), 'only one string')

        with open(self.obj.engine.artifacts_dir + '/generated.simple.jmx') as fd:
            lines = fd.readlines()
            self.assertEqual(len(lines), 1)
            self.assertEqual(lines[0].strip(), 'regular jmx contents')

    def test_existing_proxy(self):
        self.obj.api_delay = 1
        self.obj.responses = [
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1", "status": "active"}}'),
            ResponseEmul(200, ''),  # stopRecording
            ResponseEmul(200, '')]  # clearRecording

        self.obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        self.obj.settings = self.obj.engine.config.get('modules').get('recorder')

        self.obj.prepare()
        self.assertEqual(self.obj.proxy_addr, 'http://host1:port1')

    def _check_linux(self):
        self.obj.startup()
        required_env = {
            'DESKTOP_SESSION': None, 'HTTP_PROXY': 'http://host1:port1', 'https_proxy': 'http://host1:port1',
            'GNOME_DESKTOP_SESSION_ID': None, 'http_proxy': 'http://host1:port1', 'XDG_CURRENT_DESKTOP': None,
            'HTTPS_PROXY': 'http://host1:port1', 'CHROMIUM_USER_FLAGS': '--proxy-server=http://host1:port1',
            'KDE_FULL_SESSION': None}
        additional_env = self.obj.engine.provisioning.executors[0].additional_env
        self.assertEqual(additional_env, required_env)

    def _check_windows(self):
        art_dir = self.obj.engine.artifacts_dir
        os.environ['LOCALAPPDATA'] = art_dir
        os.mkdir(join(art_dir, 'Chromium'))
        os.mkdir(join(art_dir, 'Chromium', 'Application'))
        os.mkdir(join(art_dir, 'chromedriver'))
        res_dir = join(get_full_path(__file__, step_up=3), 'bzt', 'resources')
        src = join(res_dir, 'chrome-loader.c')

        dst_chrome = join(art_dir, 'Chromium', 'Application', 'chrome.exe')
        dst_chromedriver = join(art_dir, 'chromedriver', 'chromedriver.exe')

        shutil.copy2(src, dst_chrome)
        shutil.copy2(src, dst_chromedriver)

        required_env = {
            'PATH_TO_CHROME': dst_chrome,
            'ADDITIONAL_CHROME_PARAMS': '--proxy-server="http://host1:port1"',
            'CHROME_LOADER_LOG': join(self.obj.engine.artifacts_dir, 'chrome-loader.log')}

        os.environ['PATH'] = join(art_dir, 'chromedriver') + os.pathsep + os.getenv('PATH')

        self.obj.startup()

        loader_dir = set(os.listdir(join(art_dir, 'chrome-loader')))
        self.assertEqual(loader_dir, {'chrome.exe', 'chromedriver.exe'})

        additional_env = self.obj.engine.provisioning.executors[0].additional_env
        required_env = {str(key.upper()): str(required_env[key]) for key in required_env}
        additional_env = {str(key.upper()): str(additional_env[key]) for key in additional_env}

        self.assertTrue(additional_env['PATH'].startswith(join(self.obj.engine.artifacts_dir, 'chrome-loader')))
        for key in required_env:
            self.assertIn(key, additional_env)
            self.assertEqual(required_env[key], additional_env[key])

    def test_chrome_proxy(self):
        self.obj.responses = [
            ResponseEmul(200, '{"result" : {}}'),
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1"}}'),
            ResponseEmul(200, ''),
            ResponseEmul(200, ''),  # startup: startRecording
            ResponseEmul(200, ''),  # shutdown: stopRecording
            ResponseEmul(200, 'regular jmx contents'),
            ResponseEmul(200, '{"result" : {"smartjmx": "unavailable"}}'),
            ResponseEmul(200, '{"result" : {"smartjmx": "available"}}'),
            ResponseEmul(200, 'only one string')]

        self.obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        self.obj.settings = self.obj.engine.config.get('modules').get('recorder')
        self.sniff_log(self.obj.log)

        self.obj.prepare()
        self.obj.engine.provisioning.executors = [SeleniumExecutor()]
        is_linux = 'linux' in sys.platform.lower()
        if is_linux:
            self._check_linux()
        elif is_windows():
            self._check_windows()
        else:   # MacOS, for future and manual testing
            self.assertIn("Your system doesn't support settings of proxy", self.log_recorder.warn_buff.getvalue())

        self.obj.shutdown()
        self.obj.post_process()
