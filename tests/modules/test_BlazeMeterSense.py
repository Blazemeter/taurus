import logging
import os
import time

import requests

from tests import BZTestCase, random_datapoint
from bzt.modules.monitoring import LocalClient
from bzt.modules.sense import BlazeMeterSenseReporter, BlazeMeterSenseClient
from tests.mocks import EngineEmul


class TestSenseReporter(BZTestCase):
    def test_start_stop(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'browser-open': False,
        })

        adapter = RecordingAdapter([
            MockResponse(200, {'OnlineID': 'abc123'}),
            MockResponse(),
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

    def test_no_token(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        self.assertRaises(ValueError, obj.prepare)

    def test_online_disabled(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'online-enabled': False,
            'browser-open': False,
        })

        adapter = RecordingAdapter()
        obj.sense.session.mount('https://', adapter)

        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()
        self.assertEquals(0, len(adapter.requests))

    def test_sends_online_data(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'browser-open': False,
        })

        adapter = RecordingAdapter([
            MockResponse(200, {'OnlineID': 'abc123'}),  # /api/active/receiver/start/
            MockResponse(),  # api/active/receiver/data/
        ])
        obj.sense.session.mount('https://', adapter)

        obj.prepare()
        obj.startup()
        self.assertEquals(1, len(adapter.requests))

        for x in range(5):
            obj.aggregated_second(random_datapoint(x))
        obj.check()
        self.assertEquals(2, len(adapter.requests))

    def test_sends_online_data_in_shutdown(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'browser-open': False,
        })

        adapter = RecordingAdapter([
            MockResponse(200, {'OnlineID': 'abc123'}),
            MockResponse(),
            MockResponse(),
            MockResponse(),
        ])
        obj.sense.session.mount('https://', adapter)

        obj.prepare()
        obj.startup()

        for x in range(7):  # 5 in first batch, 2 in last one
            obj.aggregated_second(random_datapoint(x))

        obj.check()  # send first batch
        self.assertEquals(2, len(adapter.requests))

        obj.shutdown()  # send last batch and end_online
        self.assertEquals(4, len(adapter.requests))

    def _get_monitoring_client(self):
        return LocalClient(logging.getLogger(''),
                          None,
                          {'metrics': ['cpu', 'mem', 'disk-space']})

    def test_sends_results(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'browser-open': False,
            'online-enabled': False,
            'test-title': 'Test Test',
        })

        adapter = RecordingAdapter([
            # upload results
            MockResponse(200, [{'QueueID': '123cvb'}]),
            # wait until files are processed
            MockResponse(200, [{'status': BlazeMeterSenseClient.STATUS_DONE, 'TestID': 'testid'}]),
            # set title
            MockResponse(),
        ])
        obj.sense.session.mount('https://', adapter)

        obj.prepare()
        obj.startup()

        monitor = self._get_monitoring_client()
        for x in range(30):
            obj.aggregated_second(random_datapoint(x))
            obj.monitoring_data(monitor.get_data())
            time.sleep(0.1)

        obj.check()
        obj.shutdown()
        obj.post_process()

        self.assertTrue(os.path.exists(obj.results_file))
        self.assertTrue(os.path.exists(obj.monitoring_file))
        upload_request = adapter.requests[0]
        self.assertIn('results.ldjson', str(upload_request.body))
        self.assertIn('monitoring.ldjson', str(upload_request.body))

    def test_set_title_color(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'browser-open': False,
            'online-enabled': False,
            'test-title': 'MyTitle',
            'test-color': 'MyColor',
        })

        adapter = RecordingAdapter([
            # upload results
            MockResponse(200, [{'QueueID': '123cvb'}]),
            # wait until files are processed
            MockResponse(200, [{'status': BlazeMeterSenseClient.STATUS_DONE, 'TestID': 'testid'}]),
            # set title
            MockResponse(),
            # set color
            MockResponse(),
        ])
        obj.sense.session.mount('https://', adapter)
        monitor = self._get_monitoring_client()

        obj.prepare()
        obj.startup()
        for x in range(5):
            obj.aggregated_second(random_datapoint(x))
            obj.monitoring_data(monitor.get_data())
            time.sleep(0.1)
        obj.check()
        obj.shutdown()
        obj.post_process()

        color_req = adapter.requests[2]
        title_req = adapter.requests[3]
        self.assertIn('MyTitle', title_req.path_url)
        self.assertIn('MyColor', color_req.path_url)

    def test_upload_failed(self):
        obj = BlazeMeterSenseReporter()
        obj.engine = EngineEmul()
        obj.settings.merge({
            'token': 'faketoken',
            'browser-open': False,
            'online-enabled': False,
        })

        adapter = RecordingAdapter([
            MockResponse(451),
        ])
        obj.sense.session.mount('https://', adapter)
        monitor = self._get_monitoring_client()

        obj.prepare()
        obj.startup()
        for x in range(5):
            obj.aggregated_second(random_datapoint(x))
            obj.monitoring_data(monitor.get_data())
            time.sleep(0.1)
        obj.check()
        obj.shutdown()
        self.assertRaises(RuntimeError, obj.post_process)


class RecordingAdapter(requests.adapters.HTTPAdapter):
    def __init__(self, responses=()):
        super(RecordingAdapter, self).__init__()
        self.responses = list(responses)
        self.requests = []

    def send(self, request, *args, **kwargs):
        self.requests.append(request)
        return self.responses.pop(0)


class MockResponse(requests.Response):
    def __init__(self, status_code=200, json_data=None):
        super(MockResponse, self).__init__()
        self.json_data = json_data
        self.status_code = status_code

    def json(self):
        return self.json_data
