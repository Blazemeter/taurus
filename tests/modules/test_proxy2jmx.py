from bzt import TaurusConfigError
from bzt.modules.proxy2jmx import Proxy2JMX
from bzt.modules.selenium import SeleniumExecutor
from tests import BZTestCase
from tests.mocks import EngineEmul


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
        self.obj.engine.config.merge({})
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
        pass
