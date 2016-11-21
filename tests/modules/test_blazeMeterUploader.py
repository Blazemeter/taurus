import json
import logging
import math
import os
import shutil
import time
from io import BytesIO

import bzt.modules.blazemeter
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.blazemeter import BlazeMeterUploader, BlazeMeterClient, BlazeMeterClientEmul, ResultsFromBZA
from bzt.modules.blazemeter import MonitoringBuffer
from bzt.six import URLError, iteritems, viewvalues
from tests import BZTestCase, random_datapoint, __dir__
from tests.mocks import EngineEmul


class TestBlazeMeterUploader(BZTestCase):
    def test_some_errors(self):
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
            {"marker": "sess-start",
             "result": {
                 'session': {'id': 'sess1', 'userId': 1},
                 'master': {'id': 'master1', 'userId': 1},
                 'signature': ''}})
        client.results.append({"marker": "post-proc push", 'result': {'session': {}}})
        client.results.append({"marker": "upload1", "result": True})  # post-proc error stats
        client.results.append({"marker": "terminate", 'result': {'session': {}}})
        client.results.append({"marker": "terminate2", 'result': {'session': {}}})
        client.results.append({"marker": "sess-e", "result": {'session': {'id': 'sess1', 'note': 'n'}}})
        client.results.append({"marker": "sess-e", "result": {'session': {}}})
        client.results.append({"marker": "sess-e", "result": {'master': {'id': 'sess1', 'note': 'n'}}})
        client.results.append({"marker": "sess-e", "result": {'master': {}}})
        client.results.append({"marker": "upload-file", "result": {}})

        obj = BlazeMeterUploader()
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.client = client
        obj.prepare()
        obj.startup()
        obj.engine.stopping_reason = ValueError('wrong value')
        obj.aggregated_second(random_datapoint(10))
        obj.kpi_buffer[-1][DataPoint.CUMULATIVE][''][KPISet.ERRORS] = [
            {'msg': 'Forbidden', 'cnt': 10, 'type': KPISet.ERRTYPE_ASSERT, 'urls': [], KPISet.RESP_CODES: '111'},
            {'msg': 'Allowed', 'cnt': 20, 'type': KPISet.ERRTYPE_ERROR, 'urls': [], KPISet.RESP_CODES: '222'}]
        obj.post_process()

        # check for note appending in _postproc_phase3()
        reqs = obj.client.requests[-4:]
        self.assertIn('api/latest/sessions/sess1', reqs[0]['url'])
        self.assertIn('api/latest/sessions/sess1', reqs[1]['url'])
        self.assertIn('api/latest/masters/master1', reqs[2]['url'])
        self.assertIn('api/latest/masters/master1', reqs[3]['url'])
        self.assertIn('ValueError: wrong value', reqs[1]['data'])
        self.assertIn('ValueError: wrong value', reqs[3]['data'])

        self.assertEqual(0, len(client.results))
        data = json.loads(client.requests[6]['data'])
        self.assertEqual(1, len(data['labels']))
        total_item = data['labels'][0]
        self.assertEqual('ALL', total_item['name'])
        self.assertEqual(total_item['assertions'], [{
            'failureMessage': 'Forbidden',
            'failures': 10,
            'name': 'All Assertions'}])
        self.assertEqual(total_item['errors'], [{
            'm': 'Allowed',
            'count': 20,
            'rc': '222'}])

    def test_no_notes_for_public_reporting(self):
        client = BlazeMeterClientEmul(logging.getLogger(''))
        client.results.append({"marker": "ping", 'result': {}})
        client.results.extend([{'result': {}} for _ in range(6)])

        obj = BlazeMeterUploader()
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = ''  # public reporting
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.client = client
        obj.prepare()

        client.session_id = 'sess1'
        client.master_id = 'master1'

        obj.engine.stopping_reason = ValueError('wrong value')
        obj.aggregated_second(random_datapoint(10))
        obj.kpi_buffer[-1][DataPoint.CUMULATIVE][''][KPISet.ERRORS] = [
            {'msg': 'Forbidden', 'cnt': 10, 'type': KPISet.ERRTYPE_ASSERT, 'urls': [], KPISet.RESP_CODES: '111'},
            {'msg': 'Allowed', 'cnt': 20, 'type': KPISet.ERRTYPE_ERROR, 'urls': [], KPISet.RESP_CODES: '222'}]
        obj.send_monitoring = obj.send_custom_metrics = obj.send_custom_tables = False
        obj.post_process()

        # check for note appending in _postproc_phase3()
        reqs = [{'url': '', 'data': ''} for _ in range(4)]     # add template for minimal size
        reqs = (reqs + obj.client.requests)[-4:]
        self.assertNotIn('api/latest/sessions/sess1', reqs[0]['url'])
        self.assertNotIn('api/latest/sessions/sess1', reqs[1]['url'])
        self.assertNotIn('api/latest/masters/master1', reqs[2]['url'])
        self.assertNotIn('api/latest/masters/master1', reqs[3]['url'])
        if reqs[1]['data']:
            self.assertNotIn('ValueError: wrong value', reqs[1]['data'])
        if reqs[3]['data']:
            self.assertNotIn('ValueError: wrong value', reqs[3]['data'])

    def test_check(self):
        client = BlazeMeterClientEmul(logging.getLogger(''))
        client.timeout = 1
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
            {"marker": "sess-start",
             "result": {
                 'session': {'id': 'sess1', 'userId': 1},
                 'master': {'id': 'master1', 'userId': 1},
                 'signature': ''}})
        client.results.append({"marker": "first push", 'result': {'session': {}}})
        client.results.append(IOError("monitoring push expected fail"))
        client.results.append({"marker": "mon push", "result": True})
        client.results.append(IOError("custom metric push expected fail"))
        client.results.append({"marker": "custom metrics push", "result": True})
        client.results.append({"marker": "second push", 'result': {'session': {"statusCode": 140, 'status': 'ENDED'}}})
        client.results.append({"marker": "post-proc push", 'result': {'session': {}}})
        client.results.append({"marker": "post process monitoring push", "result": True})
        client.results.append({"marker": "post process custom metrics push", "result": True})
        client.results.append({"marker": "artifacts push", 'result': True})
        client.results.append({"marker": "logs push", 'result': True})
        client.results.append({"marker": "terminate", 'result': {'session': {}}})

        obj = BlazeMeterUploader()
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.settings['send-custom-metrics'] = True
        obj.settings['send-custom-tables'] = True
        obj.engine = EngineEmul()
        shutil.copy(__file__, os.path.join(obj.engine.artifacts_dir, os.path.basename(__file__)))
        obj.client = client
        obj.prepare()
        obj.startup()
        for x in range(0, 31):
            obj.aggregated_second(random_datapoint(x))
        mon = [{"ts": 1, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100, "other": 0},
               {"ts": 1, "source": "chrome", "memory": 32, "cpu": 23}]
        obj.monitoring_data(mon)
        obj.check()
        for x in range(32, 65):
            obj.aggregated_second(random_datapoint(x))
        self.assertRaises(KeyboardInterrupt, obj.check)
        obj.aggregated_second(random_datapoint(10))
        obj.shutdown()
        log_file = obj.engine.create_artifact('log', '.tmp')
        obj.engine.log.parent.handlers.append(logging.FileHandler(log_file))
        obj.engine.config.get('modules').get('shellexec').get('env')['TAURUS_INDEX_ALL'] = 1
        obj.post_process()
        self.assertEqual(0, len(client.results))

    def test_ping(self):
        obj = BlazeMeterClient(logging.getLogger(''))
        obj.address = "https://a.blazemeter.com"
        obj.ping()

    def test_monitoring_buffer_limit_option(self):
        obj = BlazeMeterUploader()
        obj.engine = EngineEmul()
        obj.client = BlazeMeterClientEmul(logging.getLogger(''))
        obj.client.results.append({"marker": "ping", 'result': {}})
        obj.settings["monitoring-buffer-limit"] = 100
        obj.prepare()
        for i in range(1000):
            mon = [{"ts": i, "source": "local", "cpu": float(i) / 1000 * 100, "mem": 2, "bytes-recv": 100, "other": 0}]
            obj.monitoring_data(mon)
            for source, buffer in iteritems(obj.monitoring_buffer.data):
                self.assertLessEqual(len(buffer), 100)
        self.assertEqual(0, len(obj.client.results))


class TestBlazeMeterClientUnicode(BZTestCase):
    def test_unicode_request(self):
        """
        test UnicodeDecodeError in BlazeMeterClient._request()
        """

        blazemeter_client = BlazeMeterClient(logging.getLogger(''))
        blazemeter_client.address = "http://127.0.0.1:58000"
        blazemeter_client.session_id = "ffff"
        self.token = "faketoken"
        normal_urlopen = bzt.modules.blazemeter.urlopen
        bzt.modules.blazemeter.urlopen = dummy_urlopen
        blazemeter_client.upload_file(__dir__() + "/../data/unicode_file")
        bzt.modules.blazemeter.urlopen = normal_urlopen

    def test_binary_unicode_error(self):
        client = BlazeMeterClient(logging.getLogger(''))
        client.address = u"http://127.0.0.1:58000"
        client.session_id = "ffff"
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
        self.assertEquals(1.05, percentiles_['99.0'])


class TestMonitoringBuffer(BZTestCase):
    def to_rad(self, deg):
        return deg * math.pi / 180

    def test_harmonic(self):
        ITERATIONS = 50
        SIZE_LIMIT = 10
        mon_buffer = MonitoringBuffer(SIZE_LIMIT, logging.getLogger(''))
        for i in range(ITERATIONS):
            cpu = math.sin(self.to_rad(float(i) / ITERATIONS * 180))
            mon = [{"ts": i, "source": "local", "cpu": cpu}]
            mon_buffer.record_data(mon)
            self.assertLessEqual(len(mon_buffer.data['local']), SIZE_LIMIT)

    def test_downsample_theorem(self):
        # Theorem: average interval size in monitoring buffer will always
        # be less or equal than ITERATIONS / BUFFER_LIMIT
        mon_buffer = MonitoringBuffer(100, logging.getLogger(''))
        for i in range(5000):
            mon = [{"ts": i, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100, "other": 0}]
            mon_buffer.record_data(mon)
            for source, buffer in iteritems(mon_buffer.data):
                self.assertLessEqual(len(buffer), 100)
                sizes = [item['interval'] for item in viewvalues(buffer)]
                avg_size = float(sum(sizes)) / len(sizes)
                expected_size = 5000 / 100
                self.assertLessEqual(avg_size, expected_size * 1.20)

    def test_sources(self):
        mon_buffer = MonitoringBuffer(10, logging.getLogger(''))
        for i in range(100):
            mon = [
                {"ts": i, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100},
                {"ts": i, "source": "server-agent", "cpu": 10, "mem": 20},
            ]
            mon_buffer.record_data(mon)
            for source, buffer in iteritems(mon_buffer.data):
                self.assertLessEqual(len(buffer), 10)

    def test_unpack(self):
        ITERATIONS = 200
        SIZE_LIMIT = 10
        mon_buffer = MonitoringBuffer(SIZE_LIMIT, logging.getLogger(''))
        for i in range(ITERATIONS):
            mon = [{"ts": i, "source": "local", "cpu": 1}]
            mon_buffer.record_data(mon)
        unpacked = sum(item['interval'] for item in viewvalues(mon_buffer.data['local']))
        self.assertEqual(unpacked, ITERATIONS)
