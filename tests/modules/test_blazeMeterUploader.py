import logging
import os
import shutil
from io import BytesIO
import time

from bzt.modules.aggregator import DataPoint, KPISet
from tests import BZTestCase, random_datapoint, __dir__
from bzt.six import URLError
from bzt.modules.blazemeter import BlazeMeterUploader, BlazeMeterClient, BlazeMeterClientEmul, ResultsFromBZA
from tests.mocks import EngineEmul
import bzt.modules.blazemeter


class TestBlazeMeterUploader(BZTestCase):
    def test_check(self):
        client = BlazeMeterClientEmul(logging.getLogger(''))
        client.results.append({"marker": "ping", 'result': {}})
        client.results.append({"marker": "projects", 'result': []})

        client.results.append({"marker": "project-create", 'result': {
            "id": time.time(),
            "name": "boo",
            "userId": time.time(),
            "description": None,
            "created": time.time(),
            "updated": time.time(),
            "organizationId": None
        }})
        client.results.append({"marker": "tests", 'result': {}})
        client.results.append({"marker": "test-create", 'result': {'id': 'unittest1'}})
        client.results.append(
            {"marker": "sess-start", 'result': {'session': {'id': 'sess1', 'userId': 1}, 'signature': ''}})
        client.results.append({"marker": "first push", 'result': {'session': {}}})
        # client.results.append(None)  # first check error stats
        client.results.append({"marker": "mon push", "result": True})
        client.results.append({"marker": "second push", 'result': {'session': {"statusCode": 140, 'status': 'ENDED'}}})
        # client.results.append(None)  # second check error stats
        client.results.append({"marker": "post-proc push", 'result': {'session': {}}})
        client.results.append({"marker": "upload1", "result": True})  # post-proc error stats
        client.results.append({"marker": "terminate", 'result': {'session': {}}})

        obj = BlazeMeterUploader()
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        shutil.copy(__file__, obj.engine.artifacts_dir + os.path.basename(__file__))
        obj.client = client
        obj.prepare()
        obj.startup()
        for x in range(0, 31):
            obj.aggregated_second(random_datapoint(x))
        mon = [{"ts": 1, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100, "other": 0}]
        obj.monitoring_data(mon)
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

    def test_monitoring_buffer_downsample(self):
        obj = BlazeMeterUploader()
        for i in range(1000):
            mon = [{"ts": i, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100, "other": 0}]
            obj.monitoring_data(mon)
        self.assertLessEqual(len(obj.monitoring_buffer), obj.MONITORING_BUFFER_LIMIT)


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
        blazemeter_client.upload_file(__dir__() + "/../data/unicode_file")
        bzt.modules.blazemeter.urlopen = normal_urlopen

    def test_binary_unicode_error(self):
        client = BlazeMeterClient(logging.getLogger(''))
        client.address = u"http://127.0.0.1:58000"
        client.active_session_id = "ffff"
        self.token = "faketoken"
        with open(__dir__() + "/../data/jmeter-dist-2.13.zip", 'rb') as fds:
            zip_content = fds.read()
        # actually, we're testing that UnicodeDecodeError is not raised
        self.assertRaises(URLError, client.upload_file, "jtls_and_more.zip", zip_content)


class DummyHttpResponse(object):
    def __init__(self):
        self.fake_socket = BytesIO()
        self.fake_socket.write(open(__dir__() + "/../data/unicode_file", 'rb').read())

    def read(self):
        self.fake_socket.seek(0)
        return self.fake_socket.read(1024)


def dummy_urlopen(*args, **kwargs):
    del args, kwargs
    return DummyHttpResponse()


class TestResultsFromBZA(BZTestCase):
    def test_datapoint(self):
        client = BlazeMeterClientEmul(logging.getLogger(""))
        client.results.append({
            "api_version": 2,
            "error": None,
            "result": [
                {
                    "sessions": [
                        "r-t-5746a8e38569a"
                    ],
                    "id": "ALL",
                    "name": "ALL"
                },
                {
                    "sessions": [
                        "r-t-5746a8e38569a"
                    ],
                    "id": "e843ff89a5737891a10251cbb0db08e5",
                    "name": "http://blazedemo.com/"
                }
            ]
        })
        client.results.append({
            "api_version": 2,
            "error": None,
            "result": [
                {
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "label": "ALL",
                    "kpis": [
                        {
                            "n": 1,
                            "na": 1,
                            "ec": 0,
                            "p90": 0,
                            "t_avg": 817,
                            "lt_avg": 82,
                            "by_avg": 0,
                            "n_avg": 1,
                            "ec_avg": 0,
                            "ts": 1464248743
                        }
                    ]
                }
            ]
        })
        client.results.append({
            "api_version": 2,
            "error": None,
            "result": [
                {
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "samples": 152,
                    "avgResponseTime": 786,
                    "90line": 836,
                    "95line": 912,
                    "99line": 1050,
                    "minResponseTime": 531,
                    "maxResponseTime": 1148,
                    "avgLatency": 81,
                    "geoMeanResponseTime": None,
                    "stDev": 108,
                    "duration": 119,
                    "avgBytes": 0,
                    "avgThroughput": 1.2773109243697,
                    "medianResponseTime": 0,
                    "errorsCount": 0,
                    "errorsRate": 0,
                    "hasLabelPassedThresholds": None
                },
                {
                    "labelId": "e843ff89a5737891a10251cbb0db08e5",
                    "labelName": "http://blazedemo.com/",
                    "samples": 152,
                    "avgResponseTime": 786,
                    "90line": 836,
                    "95line": 912,
                    "99line": 1050,
                    "minResponseTime": 531,
                    "maxResponseTime": 1148,
                    "avgLatency": 81,
                    "geoMeanResponseTime": None,
                    "stDev": 108,
                    "duration": 119,
                    "avgBytes": 0,
                    "avgThroughput": 1.2773109243697,
                    "medianResponseTime": 0,
                    "errorsCount": 0,
                    "errorsRate": 0,
                    "hasLabelPassedThresholds": None
                }
            ]
        })

        obj = ResultsFromBZA(client)
        obj.master_id = 0

        res = list(obj.datapoints(True))
        cumulative_ = res[0][DataPoint.CUMULATIVE]
        total = cumulative_['']
        percentiles_ = total[KPISet.PERCENTILES]
        self.assertEquals(1050, percentiles_['99.0'])
