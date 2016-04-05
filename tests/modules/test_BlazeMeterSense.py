import requests

from tests import BZTestCase
from bzt.modules.sense import BlazeMeterSenseReporter
from tests.mocks import EngineEmul


class TestSenseReporter(BZTestCase):
    def test_works(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings['token'] = 'faketoken'
        obj.engine = EngineEmul()

        adapter = RecordingAdapter([
            MockResponse(200, {'OnlineID': 'abc123'}),
            MockResponse(200, None),
        ])
        obj.sense.session.mount('https://', adapter)

        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()
        self.assertEquals(2, len(adapter.requests))
        first, second = adapter.requests
        self.assertEqual(first.path_url, '/api/active/receiver/start/')
        self.assertEqual(second.path_url, '/api/active/receiver/stop/')


class RecordingAdapter(requests.adapters.HTTPAdapter):
    def __init__(self, responses=()):
        super(RecordingAdapter, self).__init__()
        self.responses = list(responses)
        self.requests = []

    def send(self, request, *args, **kwargs):
        self.requests.append(request)
        return self.responses.pop(0)


class MockResponse(requests.Response):
    def __init__(self, status_code, json_data):
        super(MockResponse, self).__init__()
        self.json_data = json_data
        self.status_code = status_code

    def json(self):
        return self.json_data
