import json
import logging
import math
import os
import shutil
import time
from io import BytesIO

from bzt import TaurusException
from bzt.bza import Master, Session
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.blazemeter import BlazeMeterUploader, ResultsFromBZA
from bzt.modules.blazemeter import MonitoringBuffer
from bzt.six import HTTPError
from bzt.six import iteritems, viewvalues
from tests import BZTestCase, random_datapoint, RESOURCES_DIR
from tests.mocks import EngineEmul, BZMock


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
            {'msg': 'Forbidden', 'cnt': 10, 'type': KPISet.ERRTYPE_ASSERT, 'urls': [], KPISet.RESP_CODES: '111'},
            {'msg': 'Allowed', 'cnt': 20, 'type': KPISet.ERRTYPE_ERROR, 'urls': [], KPISet.RESP_CODES: '222'}]
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
            {'msg': 'Forbidden', 'cnt': 10, 'type': KPISet.ERRTYPE_ASSERT, 'urls': [], KPISet.RESP_CODES: '111'},
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
        logging.warning("\n".join([x['url'] for x in mock.requests]))
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
        session = Session(data={'id': 1})
        mock = BZMock(session)
        mock.mock_post['https://data.blazemeter.com/api/v4/image/1/files?signature=None'] = {"result": 1}
        with open(RESOURCES_DIR + "jmeter/jmeter-dist-2.13.zip", 'rb') as fds:
            zip_content = fds.read()
        session.upload_file("jtls_and_more.zip", zip_content)


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


class TestResultsFromBZA(BZTestCase):
    @staticmethod
    def convert_kpi_errors(errors):
        result = {}
        for error in errors:
            result[error['msg']] = {'count': error['cnt'], 'rc': error['rc']}
        return result

    @staticmethod
    def get_errors_mock(errors, assertions=None):
        # return mock of server response for errors specified in internal format (see __get_errors_from_BZA())
        result = []
        if not assertions:
            assertions = {}
        for _id in list(set(list(errors.keys()) + list(assertions.keys()))):  # unique keys from both dictionaries

            errors_list = []
            if errors.get(_id):
                for msg in errors[_id]:
                    errors_list.append({
                        "m": msg,
                        "count": errors[_id][msg]["count"],
                        "rc": errors[_id][msg]["rc"]})

            assertions_list = []
            if assertions.get(_id):
                for msg in assertions[_id]:
                    assertions_list.append({
                        "failureMessage": msg,
                        "failures": assertions[_id][msg]["count"],
                        "name": "All Assertions"})

            result.append({
                "_id": _id,
                "name": _id,
                "assertions": assertions_list,
                "samplesNotCounted": 0,
                "assertionsNotCounted": 0,
                "otherErrorsCount": 0,
                "errors": errors_list})
        return {
            "https://a.blazemeter.com/api/v4/masters/1/reports/errorsreport/data?noDataError=false": {
                "api_version": 4,
                "error": None,
                "result": result}}

    def test_get_errors(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/data/labels?master_id=1': {
                "api_version": 4,
                "error": None,
                "result": [{
                    "sessions": ["r-t-5746a8e38569a"],
                    "id": "ALL",
                    "name": "ALL"
                }, {
                    "sessions": ["r-t-5746a8e38569a"],
                    "id": "e843ff89a5737891a10251cbb0db08e5",
                    "name": "http://blazedemo.com/"}]},
            'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=0&master_ids%5B%5D=1&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
                "api_version": 4,
                "error": None,
                "result": [{
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "label": "ALL",
                    "kpis": [{
                        "n": 1, "na": 1, "ec": 0, "p90": 0, "t_avg": 817, "lt_avg": 82,
                        "by_avg": 0, "n_avg": 1, "ec_avg": 0, "ts": 1464248743
                    }, {"n": 1, "na": 1, "ec": 0, "p90": 0, "t_avg": 817, "lt_avg": 82,
                        "by_avg": 0, "n_avg": 1, "ec_avg": 0, "ts": 1464248744}]}]},
            'https://a.blazemeter.com/api/v4/masters/1/reports/aggregatereport/data': {
                "api_version": 4,
                "error": None,
                "result": [{
                    "labelName": "ALL", "99line": 1050, "90line": 836, "95line": 912}, {
                    "labelName": "http://blazedemo.com", "99line": 1050, "90line": 836, "95line": 912}]},
            'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=1464248744&master_ids%5B%5D=1&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
                "api_version": 4,
                "error": None,
                "result": [{
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "label": "ALL",
                    "kpis": [{
                        "n": 1, "na": 1, "ec": 0, "p90": 0, "t_avg": 817, "lt_avg": 82,
                        "by_avg": 0, "n_avg": 1, "ec_avg": 0, "ts": 1464248744
                    }, {"n": 1, "na": 1, "ec": 0, "p90": 0, "t_avg": 817, "lt_avg": 82,
                        "by_avg": 0, "n_avg": 1, "ec_avg": 0, "ts": 1464248745}]}]},
            'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=1464248745&master_ids%5B%5D=1&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
                "api_version": 4,
                "error": None,
                "result": [{
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "label": "ALL",
                    "kpis": [{
                        "n": 1, "na": 1, "ec": 0, "p90": 0, "t_avg": 817, "lt_avg": 82,
                        "by_avg": 0, "n_avg": 1, "ec_avg": 0, "ts": 1464248745}]}]}})

        obj = ResultsFromBZA()
        obj.master = Master(data={"id": 1})
        mock.apply(obj.master)

        # set cumulative errors from BM
        mock.mock_get.update(self.get_errors_mock({'ALL': {"Not found": {"count": 10, "rc": "404"}}}))

        # frame [0, 1464248744)
        res1 = list(obj.datapoints(False))
        self.assertEqual(1, len(res1))
        cumul = res1[0][DataPoint.CUMULATIVE]
        cur = res1[0][DataPoint.CURRENT]
        self.assertEqual(1, len(cumul.keys()))
        self.assertEqual(1, len(cur.keys()))
        errors_1 = {'Not found': {'count': 10, 'rc': u'404'}}
        self.assertEqual(self.convert_kpi_errors(cumul[""]["errors"]), errors_1)  # all error data is written
        self.assertEqual(self.convert_kpi_errors(cur[""]["errors"]), errors_1)  # to 'current' and 'cumulative'

        # frame [1464248744, 1464248745)
        res2 = list(obj.datapoints(False))
        self.assertEqual(1, len(res2))
        cumul = res2[0][DataPoint.CUMULATIVE]
        cur = res2[0][DataPoint.CURRENT]
        self.assertEqual(1, len(cumul.keys()))
        self.assertEqual(1, len(cur.keys()))
        self.assertEqual(self.convert_kpi_errors(cumul[""]["errors"]), errors_1)  # the same errors,
        self.assertEqual(cur[""]["errors"], [])  # new errors not found

        mock.mock_get.update(self.get_errors_mock({
            "ALL": {
                "Not found": {
                    "count": 11, "rc": "404"},  # one more error
                "Found": {
                    "count": 2, "rc": "200"}},  # new error message (error ID)
            "label1": {
                "Strange behaviour": {
                    "count": 666, "rc": "666"}}}, {  # new error label
            "ALL": {"assertion_example": {"count": 33}}}))

        res3 = list(obj.datapoints(True))  # let's add the last timestamp [1464248745]
        self.assertEqual(1, len(res3))
        cumul = res3[0][DataPoint.CUMULATIVE]
        cur = res3[0][DataPoint.CURRENT]
        errors_all_full = {
            'Not found': {'count': 11, 'rc': '404'},
            'Found': {'count': 2, 'rc': '200'},
            'assertion_example': {'count': 33, 'rc': 'All Assertions'}}
        errors_all_update = {
            'Not found': {'count': 1, 'rc': '404'},
            'Found': {'count': 2, 'rc': '200'},
            'assertion_example': {'count': 33, 'rc': 'All Assertions'}}

        errors_label1 = {'Strange behaviour': {'count': 666, 'rc': '666'}}
        self.assertEqual(errors_label1, self.convert_kpi_errors(cumul["label1"]["errors"]))
        self.assertEqual(errors_all_full, self.convert_kpi_errors(cumul[""]["errors"]))
        self.assertEqual(errors_label1, self.convert_kpi_errors(cur["label1"]["errors"]))
        self.assertEqual(errors_all_update, self.convert_kpi_errors(cur[""]["errors"]))

    def test_datapoint(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/data/labels?master_id=1': {
                "api_version": 2,
                "error": None,
                "result": [{
                    "sessions": ["r-t-5746a8e38569a"],
                    "id": "ALL",
                    "name": "ALL"
                }, {
                    "sessions": ["r-t-5746a8e38569a"],
                    "id": "e843ff89a5737891a10251cbb0db08e5",
                    "name": "http://blazedemo.com/"}]},
            'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=0&master_ids%5B%5D=1&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
                "api_version": 2,
                "error": None,
                "result": [{
                    "labelId": "ALL",
                    "labelName": "ALL",
                    "label": "ALL",
                    "kpis": [{
                        "n": 1,
                        "na": 1,
                        "ec": 0,
                        "p90": 0,
                        "t_avg": 817,
                        "lt_avg": 82,
                        "by_avg": 0,
                        "n_avg": 1,
                        "ec_avg": 0,
                        "ts": 1464248743}]}]},
            'https://a.blazemeter.com/api/v4/masters/1/reports/aggregatereport/data': {
                "api_version": 2,
                "error": None,
                "result": [{
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
                }, {
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
                    "hasLabelPassedThresholds": None}]}})

        mock.mock_get.update(self.get_errors_mock({"ALL": {}}))

        obj = ResultsFromBZA()
        obj.master = Master(data={"id": 1})
        mock.apply(obj.master)
        res = list(obj.datapoints(True))
        cumulative_ = res[0][DataPoint.CUMULATIVE]
        total = cumulative_['']
        percentiles_ = total[KPISet.PERCENTILES]
        self.assertEquals(1.05, percentiles_['99.0'])

    def test_no_kpis_on_cloud_crash(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/data/labels?master_id=0': {
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
            },
            'https://a.blazemeter.com/api/v4/data/kpis?interval=1&from=0&master_ids%5B%5D=0&kpis%5B%5D=t&kpis%5B%5D=lt&kpis%5B%5D=by&kpis%5B%5D=n&kpis%5B%5D=ec&kpis%5B%5D=ts&kpis%5B%5D=na&labels%5B%5D=ALL&labels%5B%5D=e843ff89a5737891a10251cbb0db08e5': {
                "api_version": 2,
                "error": None,
                "result": [
                    {
                        "labelId": "ALL",
                        "labelName": "ALL",
                    }
                ]
            },
            'https://a.blazemeter.com/api/v4/masters/0/reports/aggregatereport/data': {
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
            }
        })

        obj = ResultsFromBZA(Master(data={'id': 0}))
        mock.apply(obj.master)

        res = list(obj.datapoints(True))
        self.assertEqual(res, [])


class TestMonitoringBuffer(BZTestCase):
    def to_rad(self, deg):
        return deg * math.pi / 180

    def test_harmonic(self):
        iterations = 50
        size_limit = 10
        mon_buffer = MonitoringBuffer(size_limit, logging.getLogger(''))
        for i in range(iterations):
            cpu = math.sin(self.to_rad(float(i) / iterations * 180))
            mon = [{"ts": i, "source": "local", "cpu": cpu}]
            mon_buffer.record_data(mon)
            self.assertLessEqual(len(mon_buffer.data['local']), size_limit)

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
