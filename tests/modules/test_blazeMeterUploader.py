import logging
import os
import shutil
from tests import BZTestCase, random_datapoint
from bzt.modules.blazemeter import BlazeMeterUploader, BlazeMeterClient
from tests.mocks import EngineEmul
from io import BytesIO
import bzt.modules.blazemeter


class TestBlazeMeterUploader(BZTestCase):
    def test_check(self):
        client = BlazeMeterClientEmul(logging.getLogger(''))
        client.results.append({"marker": "ping", 'result': {}})
        client.results.append({"marker": "tests", 'result': {}})
        client.results.append({"marker": "test-create", 'result': {'id': 'unittest1'}})
        client.results.append(
            {"marker": "sess-start", 'result': {'session': {'id': 'sess1', 'userId': 1}, 'signature': ''}})
        client.results.append({"marker": "first push", 'result': {'session': {}}})
        # client.results.append(None)  # first check error stats
        client.results.append(
            {"marker": "second push", 'result': {'session': {"statusCode": 140, 'status': 'ENDED'}}})
        # client.results.append(None)  # second check error stats
        client.results.append({"marker": "post-proc push", 'result': {'session': {}}})
        client.results.append({"marker": "upload1", "result": True})  # post-proc error stats
        client.results.append({"marker": "terminate", 'result': {'session': {}}})

        obj = BlazeMeterUploader()
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        shutil.copy(__file__, obj.engine.artifacts_dir + os.path.basename(__file__))
        obj.client = client
        obj.prepare()
        obj.startup()
        for x in range(0, 31):
            obj.aggregated_second(random_datapoint(x))
        obj.check()
        for x in range(32, 65):
            obj.aggregated_second(random_datapoint(x))
        try:
            obj.check()
            self.fail()
        except KeyboardInterrupt:
            pass
        obj.aggregated_second(random_datapoint(10))
        obj.shutdown()
        obj.post_process()

    def test_ping(self):
        obj = BlazeMeterClient(logging.getLogger(''))
        obj.address = "https://a.blazemeter.com"
        obj.ping()


class BlazeMeterClientEmul(BlazeMeterClient):
    def __init__(self, parent_logger):
        super(BlazeMeterClientEmul, self).__init__(parent_logger)
        self.results = []

    def _request(self, url, data=None, headers=None, checker=None):
        self.log.debug("Request %s: %s", url, data)
        res = self.results.pop(0)
        self.log.debug("Response: %s", res)
        return res


class TestBlazeMeterClientUnicode(BZTestCase):
    def test_unicode_request(self):
        """
        test UnicodeDecodeError in BlazeMeterClient._request()

        """

        blazemeter_client = BlazeMeterClient(logging.getLogger(''))
        blazemeter_client.address = "http://127.0.0.1:58000"
        blazemeter_client.active_session_id = "ffff"
        self.token = "faketoken"
        normal_urlopen = bzt.modules.blazemeter.urlopen
        bzt.modules.blazemeter.urlopen = dummy_urlopen
        blazemeter_client.upload_file("tests/data/unicode_file")
        bzt.modules.blazemeter.urlopen = normal_urlopen


class DummyHttpResponse():
    def __init__(self):
        self.fake_socket = BytesIO()
        self.fake_socket.write(open("tests/data/unicode_file", 'rb').read())

    def read(self):
        self.fake_socket.seek(0)
        return self.fake_socket.read(1024)


def dummy_urlopen(*args, **kwargs):
    return DummyHttpResponse()
