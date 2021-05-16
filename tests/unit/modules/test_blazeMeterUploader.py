import json
import logging
import math
import os
import shutil
import time
from tempfile import mkstemp
from io import BytesIO
from urllib.error import HTTPError

from bzt import TaurusException
from bzt.bza import Master, Session
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator
from bzt.modules.blazemeter import BlazeMeterUploader
from bzt.modules.blazemeter.blazemeter_reporter import MonitoringBuffer
from bzt.utils import iteritems, viewvalues
from tests.unit import BZTestCase, random_datapoint, RESOURCES_DIR, ROOT_LOGGER, EngineEmul, BZMock
from tests.unit.mocks import MockReader


class TestBlazeMeterUploader(BZTestCase):
    def test_some_errors(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&name=Proj+name': {"result": []},
            'https://a.blazemeter.com/api/v4/sessions/1': {"result": {'id': 1, "note": "somenote"}},
            'https://a.blazemeter.com/api/v4/masters/1': {"result": {'id': 1, "note": "somenote"}},
        })
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests/1/start-external': {"result": {
                "session": {'id': 1, "testId": 1, "userId": 1},
                "master": {'id': 1},
                "signature": "sign"
            }},
            'https://data.blazemeter.com/api/v4/image/1/files?signature=sign': {"result": True},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1' +
            '&pq=0&target=labels_bulk&update=1': {},
            'https://a.blazemeter.com/api/v4/sessions/1/stop': {"result": True},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=engine_health&update=1': {
                'result': {'session': {}}}
        })

        mock.mock_patch.update({
            'https://a.blazemeter.com/api/v4/sessions/1': {"result": {"id": 1, "note": "somenote"}},
            'https://a.blazemeter.com/api/v4/masters/1': {"result": {"id": 1, "note": "somenote"}},
        })

        obj = BlazeMeterUploader()
        mock.apply(obj._user)
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.prepare()
        obj.startup()
        obj.engine.stopping_reason = ValueError('wrong value')
        obj.aggregated_second(random_datapoint(10))
        obj.kpi_buffer[-1][DataPoint.CUMULATIVE][''][KPISet.ERRORS] = [
            {'msg': 'Forbidden', 'cnt': 10, 'type': KPISet.ERRTYPE_ASSERT, 'urls': [], KPISet.RESP_CODES: '111',
             'tag': None},
            {'msg': 'Allowed', 'cnt': 20, 'type': KPISet.ERRTYPE_ERROR, 'urls': [], KPISet.RESP_CODES: '222'},
            {'msg': 'Not Found', 'cnt': 10, 'type': KPISet.ERRTYPE_SUBSAMPLE, 'urls': {'/non': '404'},
             KPISet.RESP_CODES: '404', 'tag': None}
        ]
        obj.post_process()
        obj.log.info("Requests: %s", mock.requests)

        # check for note appending in _postproc_phase3()
        reqs = mock.requests[-4:]
        self.assertIn('api/v4/sessions/1', reqs[0]['url'])
        self.assertIn('api/v4/sessions/1', reqs[1]['url'])
        self.assertIn('api/v4/masters/1', reqs[2]['url'])
        self.assertIn('api/v4/masters/1', reqs[3]['url'])
        self.assertIn('ValueError: wrong value', str(reqs[1]['data']))
        self.assertIn('ValueError: wrong value', str(reqs[3]['data']))

        labels = mock.requests[8]['data']
        if not isinstance(labels, str):
            labels = labels.decode("utf-8")
        obj.log.info("Labels: %s", labels)
        data = json.loads(str(labels))
        self.assertEqual(1, len(data['labels']))
        total_item = data['labels'][0]
        self.assertEqual('ALL', total_item['name'])
        self.assertEqual(total_item['assertions'],
                         [{'failureMessage': 'Forbidden', 'failures': 10, 'name': 'All Assertions'}])
        self.assertEqual(total_item['errors'], [{'m': 'Allowed', 'count': 20, 'rc': '222'}])
        self.assertEqual(total_item['failedEmbeddedResources'],
                         [{'url': '/non', 'count': 10, 'rc': '404', 'rm': 'Not Found'}])

    def test_no_notes_for_public_reporting(self):
        mock = BZMock()
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/sessions/1/terminate-external': {},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=None&test_id=1&user_id=1&pq=0&target=labels_bulk&update=1': {},
        })

        obj = BlazeMeterUploader()
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = ''  # public reporting
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        mock.apply(obj._user)
        obj.prepare()

        obj._session = Session(obj._user, {'id': 1, 'testId': 1, 'userId': 1})
        obj._master = Master(obj._user, {'id': 1})

        obj.engine.stopping_reason = ValueError('wrong value')
        obj.aggregated_second(random_datapoint(10))
        obj.kpi_buffer[-1][DataPoint.CUMULATIVE][''][KPISet.ERRORS] = [
            {'msg': 'Forbidden', 'cnt': 10, 'type': KPISet.ERRTYPE_ASSERT, 'urls': [], KPISet.RESP_CODES: '111',
             'tag': ""},
            {'msg': 'Allowed', 'cnt': 20, 'type': KPISet.ERRTYPE_ERROR, 'urls': [], KPISet.RESP_CODES: '222'}]
        obj.send_monitoring = False
        obj.post_process()

        # TODO: looks like this whole block of checks is useless
        # check for note appending in _postproc_phase3()
        reqs = [{'url': '', 'data': ''} for _ in range(4)]  # add template for minimal size
        reqs = (reqs + mock.requests)[-4:]
        self.assertNotIn('api/v4/sessions/1', reqs[0]['url'])
        self.assertNotIn('api/v4/sessions/1', reqs[1]['url'])
        self.assertNotIn('api/v4/masters/1', reqs[2]['url'])
        self.assertNotIn('api/v4/masters/1', reqs[3]['url'])
        if reqs[1]['data']:
            self.assertNotIn('ValueError: wrong value', reqs[1]['data'])
        if reqs[3]['data']:
            self.assertNotIn('ValueError: wrong value', reqs[3]['data'])

    def test_check(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&name=Proj+name': {"result": []},
        })
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {
                "id": 1,
                "name": "boo",
                "userId": 2,
                "description": None,
                "created": time.time(),
                "updated": time.time(),
                "organizationId": None
            }},
            'https://a.blazemeter.com/api/v4/tests': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests/1/start-external': {"result": {
                'session': {'id': 1, 'userId': 1, 'testId': 1},
                'master': {'id': 1, 'userId': 1},
                'signature': 'sign'}},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=labels_bulk&update=1': [
                {},
                {"result": {'session': {"statusCode": 140, 'status': 'ENDED'}}},
                {},
            ],
            'https://data.blazemeter.com/api/v4/image/1/files?signature=sign': [
                IOError("monitoring push expected fail"),
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
            ],
            'https://a.blazemeter.com/api/v4/sessions/1/stop': {},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=engine_health&update=1':
                {"result": {'session': {}}}
        })

        obj = BlazeMeterUploader()
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        shutil.copy(__file__, os.path.join(obj.engine.artifacts_dir, os.path.basename(__file__)))
        mock.apply(obj._user)
        obj._user.timeout = 0.1
        obj.prepare()
        obj.startup()
        for x in range(0, 31):
            obj.aggregated_second(random_datapoint(x))
        mon = [{"ts": 1, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100, "other": 0}]
        obj.monitoring_data(mon)
        obj.check()
        for x in range(32, 65):
            obj.aggregated_second(random_datapoint(x))
        obj.last_dispatch = time.time() - 2 * obj.send_interval
        self.assertRaises(KeyboardInterrupt, obj.check)
        obj.aggregated_second(random_datapoint(10))
        obj.shutdown()
        log_file = obj.engine.create_artifact('log', '.tmp')
        handler = logging.FileHandler(log_file)
        obj.engine.log.parent.addHandler(handler)
        obj.engine.config.get('modules').get('shellexec').get('env')['TAURUS_INDEX_ALL'] = 1
        obj.post_process()
        self.assertEqual(20, len(mock.requests))
        obj.engine.log.parent.removeHandler(handler)

    def test_extend_datapoints(self):
        # check reported data format conversion for test state filtering on BM side

        def get_mock(origin_func, store):
            # generate replacement for BlazemeterUploader._dpoint_serializer.get_kpi_body
            def mock_get_kpi_body(data, isfinal):
                store.append(data)                  # save received data for verifying
                return origin_func(data, isfinal)   # call original get_kpi_body as well
            return mock_get_kpi_body

        mock = BZMock()
        mock.mock_get.update({
            '1': {"result": []},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Test': {"result": []},
            '3': {"result": []},
        })
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests/1/start-external': {"result": {
                'session': {'id': 1, 'userId': 1, 'testId': 1},
                'master': {'id': 1, 'userId': 1},
                'signature': 'sign'}},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=labels_bulk&update=1': [
                {},
                {"result": {'session': {"statusCode": 140, 'status': 'ENDED'}}},
                {},
            ],
            'https://data.blazemeter.com/api/v4/image/1/files?signature=sign': [
                IOError("monitoring push expected fail"),
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
                {"result": True},
            ],
            'https://a.blazemeter.com/api/v4/sessions/1/stop': {},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=engine_health&update=1':
                {"result": {'session': {}}}
        })

        obj = BlazeMeterUploader()
        sent_data_points = []
        obj._dpoint_serializer.get_kpi_body = get_mock(obj._dpoint_serializer.get_kpi_body, sent_data_points)
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        aggregator = ConsolidatingAggregator()
        aggregator.engine = obj.engine
        aggregator.settings['extend-aggregation'] = True
        reader = MockReader()
        watcher = MockReader()

        reader.buffer_scale_idx = '100.0'
        # data format: t_stamp, label, conc, r_time, con_time, latency, r_code, error, trname, byte_count
        reader.data.append((1, "a", 1, 1, 1, 1, 200, None, '', 1))
        reader.data.append((2, "b", 1, 2, 2, 2, 200, 'OK', '', 2))
        reader.data.append((2, "b", 1, 3, 3, 3, 404, "Not Found", '', 3))
        reader.data.append((2, "c", 1, 4, 4, 4, 200, None, '', 4))
        reader.data.append((3, "d", 1, 5, 5, 5, 200, None, '', 5))
        reader.data.append((5, "b", 1, 6, 6, 6, 200, None, '', 6))
        reader.data.append((5, "c", 1, 7, 7, 7, 200, None, '', 7))
        original_labels = list(d[1] for d in reader.data)

        aggregator.add_underling(reader)
        aggregator.add_listener(watcher)
        obj.engine.aggregator = aggregator

        mock.apply(obj._user)
        obj._user.timeout = 0.001

        obj.engine.aggregator.prepare()
        obj.prepare()

        obj.engine.aggregator.startup()
        obj.startup()

        obj.engine.aggregator.check()
        obj.check()

        obj.engine.aggregator.shutdown()
        obj.shutdown()

        obj.engine.aggregator.post_process()
        obj.post_process()

        sent_data_points = sent_data_points[0] + sent_data_points[1]

        state_labels = [0, 1, 2]
        for dp in sent_data_points:
            for data in dp['cumulative'], dp['current']:
                for label in data:
                    self.assertIn(label, original_labels + [''])
                    self.assertIsInstance(data[label], dict)
                    for key in data[label]:
                        self.assertIn(key, state_labels)

    def test_monitoring_buffer_limit_option(self):
        obj = BlazeMeterUploader()
        obj.engine = EngineEmul()
        mock = BZMock(obj._user)
        obj.settings["monitoring-buffer-limit"] = 100
        obj.prepare()
        for i in range(1000):
            mon = [{"ts": i, "source": "local", "cpu": float(i) / 1000 * 100, "mem": 2, "bytes-recv": 100, "other": 0}]
            obj.monitoring_data(mon)
            for source, buffer in iteritems(obj.monitoring_buffer.data):
                self.assertLessEqual(len(buffer), 100)
        self.assertEqual(1, len(mock.requests))

    def test_direct_feeding(self):
        obj = BlazeMeterUploader()
        self.sniff_log(obj.log)
        obj.engine = EngineEmul()
        mock = BZMock(obj._user)
        mock.mock_post.update({
            'https://data.blazemeter.com/submit.php?session_id=direct&signature=sign&test_id=None&user_id=None&pq=0&target=labels_bulk&update=1': {},
            'https://data.blazemeter.com/api/v4/image/direct/files?signature=sign': {"result": True},
            'https://a.blazemeter.com/api/v4/sessions/direct/stop': {"result": True},
            'https://data.blazemeter.com/submit.php?session_id=direct&signature=sign&test_id=None&user_id=None&pq=0&target=engine_health&update=1': {
                'result': {'session': {}}}
        })
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/sessions/direct': {"result": {}}
        })
        mock.mock_patch.update({
            'https://a.blazemeter.com/api/v4/sessions/direct': {"result": {}}
        })
        obj.parameters['session-id'] = 'direct'
        obj.parameters['signature'] = 'sign'
        obj.settings['token'] = 'FakeToken'
        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.engine.stopping_reason = TaurusException("To cover")
        obj.post_process()
        self.assertNotIn("Failed to finish online", self.log_recorder.warn_buff.getvalue())
        self.assertEquals('direct', obj._session['id'])
        self.assertEqual(9, len(mock.requests), "Requests were: %s" % mock.requests)

    def test_anonymous_feeding(self):
        obj = BlazeMeterUploader()
        obj.engine = EngineEmul()
        obj.browser_open = False
        mock = BZMock(obj._user)
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/sessions': {"result": {
                "signature": "sign",
                "publicTokenUrl": "publicUrl",
                "session": {"id": 1, "testId": 1, "userId": 1},
                "master": {"id": 1},
            }},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=labels_bulk&update=1': {},
            'https://data.blazemeter.com/api/v4/image/1/files?signature=sign': {"result": True},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=engine_health&update=1': {
                'result': {'session': {}}},
        })
        obj.prepare()
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()
        self.assertEquals(1, obj._session['id'])
        self.assertEqual(6, len(mock.requests), "Requests were: %s" % mock.requests)

    def test_401(self):
        obj = BlazeMeterUploader()
        obj.engine = EngineEmul()
        mock = BZMock(obj._user)
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/web/version': HTTPError(None, None, None, None, None, ),
        })
        self.assertRaises(HTTPError, obj.prepare)

    def test_multiple_reporters_one_monitoring(self):
        obj1 = BlazeMeterUploader()
        obj1.engine = EngineEmul()
        BZMock(obj1._user)

        obj2 = BlazeMeterUploader()
        obj2.engine = EngineEmul()
        BZMock(obj2._user)

        obj1.prepare()
        obj2.prepare()

        for i in range(10):
            mon = [{"ts": i, "source": "local", "cpu": float(i) / 1000 * 100, "mem": 2, "bytes-recv": 100, "other": 0}]
            obj1.monitoring_data(mon)
            obj2.monitoring_data(mon)

    def test_public_report(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Test': {"result": []}
        })

        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests': {'result': {'id': 'unittest1'}},
            'https://a.blazemeter.com/api/v4/tests/unittest1/start-external': {"result": {
                'session': {'id': 'sess1', 'userId': 1, 'testId': 1},
                'master': {'id': 'master1', 'userId': 1},
                'signature': ''
            }},
            'https://a.blazemeter.com/api/v4/masters/master1/public-token': {'result': {'publicToken': 'publicToken'}},
            'https://data.blazemeter.com/submit.php?session_id=sess1&signature=&test_id=1&user_id=1&pq=0&target=labels_bulk&update=1': {
                "result": {'session': {}}},
            'https://data.blazemeter.com/api/v4/image/sess1/files?signature=': {'result': True},
        })

        obj = BlazeMeterUploader()
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.settings['public-report'] = True
        obj.settings['send-monitoring'] = False
        obj.engine = EngineEmul()
        mock.apply(obj._user)
        self.sniff_log(obj.log)
        obj.prepare()
        obj.startup()
        obj.aggregated_second(random_datapoint(10))
        obj.check()
        obj.shutdown()
        obj.post_process()

        log_buff = self.log_recorder.info_buff.getvalue()
        log_line = "Public report link: https://a.blazemeter.com/app/?public-token=publicToken#/masters/master1/summary"
        self.assertIn(log_line, log_buff)
        ROOT_LOGGER.warning("\n".join([x['url'] for x in mock.requests]))
        self.assertEqual(14, len(mock.requests))

    def test_new_project_existing_test(self):
        obj = BlazeMeterUploader()
        mock = BZMock(obj._user)
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Test': {'result': [
                {'id': 1, 'name': 'Taurus Test', 'configuration': {"type": 'external'}}
            ]},
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Test': {'result': []}
        })

        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {"id": 1}},
        })

        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/projects', mock.requests[4]['url'])
        self.assertEquals('POST', mock.requests[4]['method'])
        self.assertEquals('https://a.blazemeter.com/api/v4/tests', mock.requests[6]['url'])
        self.assertEquals('POST', mock.requests[6]['method'])

    def test_new_project_new_test(self):
        obj = BlazeMeterUploader()
        mock = BZMock(obj._user)
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': {'result': []}
        })

        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {"id": 1}},
        })

        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/projects', mock.requests[6]['url'])
        self.assertEquals('POST', mock.requests[6]['method'])
        self.assertEquals('https://a.blazemeter.com/api/v4/tests', mock.requests[7]['url'])
        self.assertEquals('POST', mock.requests[7]['method'])

    def test_existing_project_new_test(self):
        obj = BlazeMeterUploader()
        mock = BZMock(obj._user)
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Test': {'result': []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1': {'result': [
                {'id': 1, 'name': 'Proj name'}
            ]}
        })

        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {"id": 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {"id": 1}},
        })

        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.prepare()
        self.assertEquals('https://a.blazemeter.com/api/v4/tests', mock.requests[6]['url'])
        self.assertEquals('POST', mock.requests[6]['method'])

    def test_excluded_cumulative(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/tests?projectId=1&name=Taurus+Test': {"result": []},
        })
        mock.mock_post.update({
            'https://a.blazemeter.com/api/v4/projects': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests': {"result": {'id': 1}},
            'https://a.blazemeter.com/api/v4/tests/1/start-external': {"result": {
                "session": {'id': 1, "testId": 1, "userId": 1},
                "master": {'id': 1},
                "signature": "sign"
            }},
            'https://data.blazemeter.com/api/v4/image/1/files?signature=sign': {"result": True},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1' +
            '&pq=0&target=labels_bulk&update=1': {},
            'https://data.blazemeter.com/submit.php?session_id=1&signature=sign&test_id=1&user_id=1&pq=0&target=engine_health&update=1': {
                'result': {'session': {}}}
        })

        obj = BlazeMeterUploader()
        mock.apply(obj._user)
        obj.parameters['project'] = 'Proj name'
        obj.settings['token'] = '123'
        obj.settings['browser-open'] = 'none'
        obj.engine = EngineEmul()
        obj.prepare()
        obj.startup()
        obj.aggregated_second(random_datapoint(10))
        obj.kpi_buffer[-1][DataPoint.CUMULATIVE] = {}  # remove cumulative when ramp-up data is excluded
        obj.post_process()  # no 'Cumulative KPISet is non-consistent' exception here


class TestBlazeMeterClientUnicode(BZTestCase):
    def test_unicode_request(self):
        """
        test UnicodeDecodeError in BlazeMeterClient._request()
        """
        session = Session(data={'id': 1})
        mock = BZMock(session)
        mock.mock_post['https://data.blazemeter.com/api/v4/image/1/files?signature=None'] = {"result": 1}
        session.upload_file(RESOURCES_DIR + "jmeter/unicode_file")

    def test_binary_unicode_error(self):
        fd, fname = mkstemp()
        os.close(fd)
        file_handler = logging.FileHandler(fname, encoding="utf-8")
        file_handler.setLevel(logging.DEBUG)
        ROOT_LOGGER.addHandler(file_handler)

        try:
            session = Session(data={'id': 1})
            mock = BZMock(session)
            mock.mock_post['https://data.blazemeter.com/api/v4/image/1/files?signature=None'] = {"result": 1}
            with open(RESOURCES_DIR + "jmeter/jmeter-dist-2.13.zip", 'rb') as fds:
                zip_content = fds.read()
            session.upload_file("jtls_and_more.zip", zip_content)
        finally:
            ROOT_LOGGER.removeHandler(file_handler)
            file_handler.close()
            os.remove(fname)


class DummyHttpResponse(object):
    def __init__(self):
        self.fake_socket = BytesIO()
        self.fake_socket.write(open(RESOURCES_DIR + "unicode_file", 'rb').read())

    def read(self):
        self.fake_socket.seek(0)
        return self.fake_socket.read(1024)


def dummy_urlopen(*args, **kwargs):
    del args, kwargs
    return DummyHttpResponse()


class TestMonitoringBuffer(BZTestCase):
    def to_rad(self, deg):
        return deg * math.pi / 180

    def test_harmonic(self):
        iterations = 50
        size_limit = 10
        mon_buffer = MonitoringBuffer(size_limit, ROOT_LOGGER)
        for i in range(iterations):
            cpu = math.sin(self.to_rad(float(i) / iterations * 180))
            mon = [{"ts": i, "source": "local", "cpu": cpu}]
            mon_buffer.record_data(mon)
            self.assertLessEqual(len(mon_buffer.data['local']), size_limit)

    def test_downsample_theorem(self):
        # Theorem: average interval size in monitoring buffer will always
        # be less or equal than ITERATIONS / BUFFER_LIMIT
        mon_buffer = MonitoringBuffer(100, ROOT_LOGGER)
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
        mon_buffer = MonitoringBuffer(10, ROOT_LOGGER)
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
        mon_buffer = MonitoringBuffer(SIZE_LIMIT, ROOT_LOGGER)
        for i in range(ITERATIONS):
            mon = [{"ts": i, "source": "local", "cpu": 1}]
            mon_buffer.record_data(mon)
        unpacked = sum(item['interval'] for item in viewvalues(mon_buffer.data['local']))
        self.assertEqual(unpacked, ITERATIONS)
