import os
import sys
import shutil

from bzt import TaurusConfigError
from bzt.modules.proxy2jmx import Proxy2JMX
from bzt.modules.selenium import SeleniumExecutor
from bzt.utils import is_windows, get_full_path
from tests import BZTestCase
from tests.mocks import EngineEmul, RecordingHandler
from os.path import join


class Proxy2JMXEmul(Proxy2JMX):
    responses = []

    def api_request(self, path='', method='GET', check=True):
        return self.responses.pop(0)


class ResponseEmul(object):
    def __init__(self, status_code, text):
        self.status_code = status_code
        self.text = text


class TestProxy2JMX(BZTestCase):
    def setUp(self):
        self.obj = Proxy2JMXEmul()
        self.obj.engine = EngineEmul()

    def test_no_token(self):
        self.obj.settings = self.obj.engine.config.get('recorder')
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_full(self):
        self.obj.api_delay = 1
        self.obj.responses = [
            ResponseEmul(404, ''),
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1"}}'),
            ResponseEmul(200, ''),
            ResponseEmul(200, ''),  # startup: startRecording
            ResponseEmul(200, ''),  # shutdown: stopRecording
            ResponseEmul(200, '{"result" : {"smartjmx": "unavailable"}}'),
            ResponseEmul(200, '{"result" : {"smartjmx": "available"}}'),
            ResponseEmul(200, 'only one string')]

        self.obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        self.obj.settings = self.obj.engine.config.get('modules').get('recorder')

        self.obj.prepare()
        self.assertEqual(self.obj.proxy, 'http://host1:port1')
        self.obj.engine.provisioning.executors = [SeleniumExecutor()]
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()
        res_file = self.obj.engine.artifacts_dir + '/generated.jmx'
        with open(res_file) as fd:
            lines = fd.readlines()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0].strip(), 'only one string')

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
        self.assertEqual(self.obj.proxy, 'http://host1:port1')

    def test_variables_setting(self):
        self.obj.responses = [
            ResponseEmul(404, ''),
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1"}}'),
            ResponseEmul(200, ''),
            ResponseEmul(200, ''),  # startup: startRecording
            ResponseEmul(200, ''),  # shutdown: stopRecording
            ResponseEmul(200, '{"result" : {"smartjmx": "unavailable"}}'),
            ResponseEmul(200, '{"result" : {"smartjmx": "available"}}'),
            ResponseEmul(200, 'only one string')]

        self.obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        self.obj.settings = self.obj.engine.config.get('modules').get('recorder')
        handler = RecordingHandler()
        self.obj.log.addHandler(handler)

        self.obj.prepare()
        self.obj.engine.provisioning.executors = [SeleniumExecutor()]
        self.obj.startup()
        self.obj.log.removeHandler(handler)
        is_linux = 'linux' in sys.platform.lower()
        if is_linux:
            self.obj.startup()
            required_env = {
                'DESKTOP_SESSION': None, 'HTTP_PROXY': 'http://host1:port1', 'https_proxy': 'http://host1:port1',
                'GNOME_DESKTOP_SESSION_ID': None, 'http_proxy': 'http://host1:port1', 'XDG_CURRENT_DESKTOP': None,
                'HTTPS_PROXY': 'http://host1:port1', 'CHROMIUM_USER_FLAGS': '--proxy-server=http://host1:port1',
                'KDE_FULL_SESSION': None}
            additional_env = self.obj.engine.provisioning.executors[0].additional_env
            self.assertEqual(additional_env, required_env)
        elif is_windows():
            local_data = self.obj.engine.artifacts_dir
            os.environ['LOCALAPPDATA'] = local_data
            os.mkdir(join(local_data, 'Chromium'))
            os.mkdir(join(local_data, 'Chromium', 'Application'))
            src = join(get_full_path(__file__, step_up=3), 'bzt', 'resources', 'chrome.exe', 'loader.exe')
            dst = join(local_data, 'Chromium', 'Application', 'chrome.exe')
            shutil.copy2(src, dst)
            required_env = {
                'PATH_TO_CHROME': dst,
                'ADDITIONAL_CHROME_PARAMS': '--proxy-server="http://host1:port1"',
                'CHROME_LOADER_LOG': join(self.obj.engine.artifacts_dir, 'chrome-loader.log'),
                'PATH': join(self.obj.engine.artifacts_dir, 'chrome-loader') + os.path.pathsep + os.getenv('PATH')}
            self.obj.startup()

            # todo: check if chrome.exe has been copied to chrome-loader dir
            # todo: create fake chromedriver + add it to path + check _prepare_chrome_loader
            # todo: check if chromedirver has been copied to chrome-loader dir

            additional_env = self.obj.engine.provisioning.executors[0].additional_env
            self.assertEqual(additional_env, required_env)
        else:
            self.assertIn("Your system doesn't support settings of proxy", handler.warn_buff.getvalue())

        self.obj.shutdown()
        self.obj.post_process()
