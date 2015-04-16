import logging
import os
import shutil
import time
from bzt.modules.blazemeter import BlazeMeterUploader, BlazeMeterClient
from tests import BZTestCase, random_datapoint
from tests.mocks import EngineEmul
from six.moves import BaseHTTPServer
from six.moves import urllib
import threading


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
        obj.aggregated_second(random_datapoint(0))
        obj.aggregated_second(random_datapoint(1))
        obj.aggregated_second(random_datapoint(2))
        obj.aggregated_second(random_datapoint(3))
        obj.aggregated_second(random_datapoint(4))
        obj.check()
        obj.aggregated_second(random_datapoint(5))
        obj.aggregated_second(random_datapoint(6))
        obj.aggregated_second(random_datapoint(7))
        obj.aggregated_second(random_datapoint(8))
        obj.aggregated_second(random_datapoint(9))
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


class BlazeMeterClientTestFail(BZTestCase):
    def test_unicode_request(self):
        """
        test post and get requests

        """

        fake_srv = threading.Thread(target=run_fake_server)
        fake_srv.start()
        # give time to bind socket and start serve
        time.sleep(4)
        # TODO: use port 0
        blazemeter_client = BlazeMeterClient(logging.getLogger(''))
        blazemeter_client.address = "http://127.0.0.1:58000"
        blazemeter_client.active_session_id = "ffff"
        self.token = "faketoken"
        try:
            blazemeter_client.upload_file("tests/data/unicode_file")
        except UnicodeDecodeError:
            # do one http request to shutdown server
            urllib.request.urlopen('http://127.0.0.1:58000')
            raise


class CustomHttpHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(self):
        self.send_response(200)
        self.end_headers()
        fake_reply = open("tests/data/unicode_file", 'rb').read()
        self.wfile.write(fake_reply)

    def do_GET(self):
        self.send_response(200)
        self.end_headers()


def run_fake_server(server=BaseHTTPServer.HTTPServer,
                    handler=CustomHttpHandler):
    """
    handle one request and quit
    """
    server_address = ('', 58000)
    httpd = server(server_address, handler)
    httpd.handle_request()