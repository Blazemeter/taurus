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
    def __init__(self, status_code, content):
        self.status_code = status_code
        self.content = content


class TestProxy2JMX(BZTestCase):
    def test_no_token(self):
        obj = Proxy2JMXEmul()
        obj.engine = EngineEmul()
        obj.engine.config.merge({})
        obj.settings = obj.engine.config.get('recorder')
        self.assertRaises(TaurusConfigError, obj.prepare)

    def test_full(self):
        obj = Proxy2JMXEmul()
        obj.api_delay = 1
        obj.responses = [
            ResponseEmul(404, ''),
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1"}}'),
            ResponseEmul(200, ''),
            ResponseEmul(200, ''),  # startup: startRecording
            ResponseEmul(200, ''),  # shutdown: stopRecording
            ResponseEmul(200, '{"result" : {"smartjmx": "unavailable"}}'),
            ResponseEmul(200, '{"result" : {"smartjmx": "available"}}'),
            ResponseEmul(200, 'only one string')
        ]
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        obj.settings = obj.engine.config.get('modules').get('recorder')

        obj.prepare()
        self.assertEqual(obj.proxy, 'http://host1:port1')
        obj.engine.provisioning.executors = [SeleniumExecutor()]
        obj.startup()
        obj.shutdown()
        obj.post_process()
        res_file = obj.engine.artifacts_dir + '/generated.jmx'
        with open(res_file) as fd:
            lines = fd.readlines()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0].strip(), 'only one string')

    def test_existing_proxy(self):
        obj = Proxy2JMXEmul()
        obj.api_delay = 1
        obj.responses = [
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1", "status": "active"}}'),
            ResponseEmul(200, ''),  # stopRecording
            ResponseEmul(200, '')  # clearRecording
        ]
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        obj.settings = obj.engine.config.get('modules').get('recorder')

        obj.prepare()
        self.assertEqual(obj.proxy, 'http://host1:port1')
