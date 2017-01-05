import cookielib
import json
import logging

import requests

from bzt import TaurusNetworkError, ManualShutdown
from bzt.six import text_type
from bzt.six import urlencode
from bzt.utils import to_json, MultiPartForm

logging.getLogger("requests").setLevel(logging.WARNING)


class BZAObject(dict):
    def __init__(self, proto=None, data=None):
        """
        :type proto: BZAObject
        :type data: dict
        """
        super(BZAObject, self).__init__()
        self.update(data if data is not None else {})

        self.address = "https://a.blazemeter.com"
        self.data_address = "https://data.blazemeter.com"
        self.timeout = 10
        self.logger_limit = 256
        self.token = None
        self.log = logging.getLogger(self.__class__.__name__)
        self._cookies = cookielib.CookieJar()

        if isinstance(proto, BZAObject):  # TODO: do we have anything more graceful to handle this?
            self.address = proto.address
            self.data_address = proto.data_address
            self.timeout = proto.timeout
            self.logger_limit = proto.logger_limit
            self.token = proto.token
            self._request = proto._request  # for unit tests override
            self._cookies = proto._cookies

    def _request(self, url, data=None, headers=None, method=None):
        if not headers:
            headers = {}

        if self.token:
            headers["X-Api-Key"] = self.token

        if method:
            log_method = method
        else:
            log_method = 'GET' if data is None else 'POST'

        url = str(url)
        self.log.debug("Request: %s %s %s", log_method, url, data[:self.logger_limit] if data else None)
        data = data.encode("utf8") if isinstance(data, text_type) else data
        response = requests.request(method=log_method, url=url, data=data, headers=headers, cookies=self._cookies,
                                    timeout=self.timeout)

        resp = response.content
        if not isinstance(resp, str):
            resp = resp.decode()

        self.log.debug("Response: %s", resp[:self.logger_limit] if resp else None)

        try:
            return json.loads(resp) if len(resp) else {}
        except ValueError as exc:
            self.log.debug('Response: %s', resp)
            raise TaurusNetworkError("Non-JSON response from API: %s" % exc)

    def ping(self):
        """ Quick check if we can access the service """
        self._request(self.address + '/api/v4/web/version')


class BZAObjectsList(list):
    def __getattr__(self, name):
        def call_list_items(**kwargs):
            res = BZAObjectsList()
            for item in self:
                method = getattr(item, name)
                chunk = method(**kwargs)  # TODO: make check for args count to improve error message
                if not isinstance(chunk, BZAObjectsList):
                    msg = "%s.%s() must return BZAObjectsList, but returned %s"
                    raise TypeError(msg % (type(item).__name__, name, type(chunk).__name__))
                res += chunk

            # logging.debug("%s[%s]: %s", name, len(res), json.dumps(res, indent=True))
            return res

        return call_list_items


# ================================= Entities =================================

class User(BZAObject):
    def accounts(self):
        """
        :rtype: BZAObjectsList[Account]
        """
        res = self._request(self.address + '/api/v4/accounts')
        return BZAObjectsList([Account(self, x) for x in res['result']])

    def get_user(self):  # TODO: move it to parent class?
        res = self._request(self.address + '/api/v4/user')
        return User(self, res)


class Account(BZAObject):
    def workspaces(self):
        """
        :rtype: BZAObjectsList[Workspace]
        """
        params = {"accountId": self['id']}
        res = self._request(self.address + '/api/v4/workspaces?' + urlencode(params))
        return BZAObjectsList([Workspace(self, x) for x in res['result']])


class Workspace(BZAObject):
    def projects(self, name=None, proj_id=None):
        """
        :rtype: BZAObjectsList[Project]
        """
        params = {"workspaceId": self['id']}
        res = self._request(self.address + '/api/v4/projects?' + urlencode(params))

        projects = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            if proj_id is not None and item['id'] != proj_id:
                continue

            projects.append(Project(self, item))
        return projects

    def private_locations(self):
        """
        :rtype: BZAObjectsList[BZAObject]
        """
        params = {"workspaceId": self['id']}
        res = self._request(self.address + '/api/v4/private-locations?' + urlencode(params))
        return BZAObjectsList([BZAObject(self, x) for x in res['result']])

    def tests(self, name=None, test_type=None):
        """
        :rtype: BZAObjectsList[Test]
        """
        params = {"workspaceId": self['id']}
        if name is not None:
            params["name"] = name

        res = self._request(self.address + '/api/v4/tests?' + urlencode(params))
        tests = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            if test_type is not None and item['configuration']['type'] != test_type:
                continue

            tests.append(Test(self, item))
        return tests

    def multi_tests(self, name=None):
        """
        :rtype: BZAObjectsList[MultiTest]
        """
        params = {"workspaceId": self['id']}
        if name is not None:
            params["name"] = name

        res = self._request(self.address + '/api/v4/multi-tests?' + urlencode(params))
        tests = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            tests.append(MultiTest(self, item))
        return tests

    def create_project(self, proj_name):
        hdr = {"Content-Type": "application/json"}
        data = self._request(self.address + '/api/v4/projects', to_json({"name": str(proj_name)}), headers=hdr)
        return data['result']['id']


class Project(BZAObject):
    def tests(self, name=None, test_type=None):
        """
        :rtype: BZAObjectsList[Test]
        """
        params = {"projectId": self['id']}
        if name is not None:
            params["name"] = name

        res = self._request(self.address + '/api/v4/tests?' + urlencode(params))
        tests = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            if test_type is not None and item['configuration']['type'] != test_type:
                continue

            tests.append(Test(self, item))
        return tests

    def multi_tests(self, name=None):
        """
        :rtype: BZAObjectsList[MultiTest]
        """
        params = {"projectId": self['id']}
        if name is not None:
            params["name"] = name

        res = self._request(self.address + '/api/v4/multi-tests?' + urlencode(params))
        tests = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            tests.append(MultiTest(self, item))
        return tests

    def create_test(self, name, configuration):
        self.log.debug("Creating new test")
        url = self.address + '/api/v4/tests'
        data = {"name": name, "projectId": self['id'], "configuration": configuration}
        hdr = {"Content-Type": "application/json"}
        resp = self._request(url, to_json(data), headers=hdr)
        return Test(self, resp['result'])


class Test(BZAObject):
    def start_external(self):
        url = self.address + "/api/v4/tests/%s/start-external" % self['id']
        res = self._request(url, method='POST')
        result = res['result']
        session = Session(self, result['session'])
        session.data_signature = result['signature']
        return session, Master(self, result['master'])

    def start_anonymous_external_test(self):
        url = self.address + "/api/v4/sessions"
        res = self._request(url, method='POST')
        result = res['result']
        session = Session(self, result['session'])
        session.data_signature = result['signature']
        return session, Master(self, result['master']), result['publicTokenUrl']

    def get_test_files(self, test_id):
        path = self.address + "/api/v4/web/elfinder/%s" % test_id
        query = urlencode({'cmd': 'open', 'target': 's1_Lw'})
        url = path + '?' + query
        response = self._request(url)
        return response["files"]

    def delete_test_files(self, test_id):
        files = self.get_test_files(test_id)
        self.log.debug("Test files: %s", [filedict['name'] for filedict in files])
        if not files:
            return
        path = "/api/v4/web/elfinder/%s" % test_id
        query = "cmd=rm&" + "&".join("targets[]=%s" % fname['hash'] for fname in files)
        url = self.address + path + '?' + query
        response = self._request(url)
        if len(response['removed']) == len(files):
            self.log.debug("Successfully deleted %d test files", len(response['removed']))


class MultiTest(BZAObject):
    pass


class Master(BZAObject):
    def make_report_public(self):
        url = self.address + "/api/v4/masters/%s/publicToken" % self['id']
        res = self._request(url, to_json({"publicToken": None}),
                            headers={"Content-Type": "application/json"}, method="POST")
        public_token = res['result']['publicToken']
        report_link = self.address + "/app/?public-token=%s#/masters/%s/summary" % (public_token, self['id'])
        return report_link

    def send_custom_metrics(self, data):
        url = self.address + "/api/v4/data/masters/%s/custom-metrics" % self['id']
        res = self._request(url, to_json(data), headers={"Content-Type": "application/json"}, method="POST")
        return res

    def send_custom_tables(self, data):
        url = self.address + "/api/v4/data/masters/%s/custom-table" % self['id']
        res = self._request(url, to_json(data), headers={"Content-Type": "application/json"}, method="POST")
        return res

    def fetch(self):
        url = self.address + "/api/v4/masters/%s" % self['id']
        res = self._request(url)
        self.update(res['result'])

    def set(self, data):
        url = self.address + "/api/v4/masters/%s" % self['id']
        res = self._request(url, to_json(data), headers={"Content-Type": "application/json"}, method='PATCH')
        self.update(res['result'])

    def get_master_status(self):
        sess = self._request(self.address + '/api/v4/masters/%s/status' % self['id'])
        return sess['result']

    def get_master_sessions(self):
        sess = self._request(self.address + '/api/v4/masters/%s/sessions' % self['id'])
        if 'sessions' in sess['result']:
            return sess['result']['sessions']
        else:
            return sess['result']

    def get_kpis(self, master_id, min_ts):
        params = [
            ("interval", 1),
            ("from", min_ts),
            ("master_ids[]", master_id),
        ]
        for item in ('t', 'lt', 'by', 'n', 'ec', 'ts', 'na'):
            params.append(("kpis[]", item))

        labels = self.get_labels(master_id)
        for label in labels:
            params.append(("labels[]", label['id']))

        url = self.address + "/api/v4/data/kpis?" + urlencode(params)
        res = self._request(url)
        return res['result']

    def get_labels(self, master_id):
        url = self.address + "/api/v4/data/labels?" + urlencode({'master_id': master_id})
        res = self._request(url)
        return res['result']

    def get_aggregate_report(self, master_id):
        url = self.address + "/api/v4/masters/%s/reports/aggregatereport/data" % master_id
        res = self._request(url)
        return res['result']


class Session(BZAObject):
    def __init__(self, proto=None, data=None):
        super(Session, self).__init__(proto, data)
        self.data_signature = None
        self.kpi_target = 'labels_bulk'

    def fetch(self):
        url = self.address + "/api/v4/sessions/%s" % self['id']
        res = self._request(url)
        self.update(res['result'])

    def set(self, data):
        url = self.address + "/api/v4/sessions/%s" % self['id']
        res = self._request(url, to_json(data), headers={"Content-Type": "application/json"}, method='PATCH')
        self.update(res['result'])

    def stop(self):
        url = self.address + "/api/v4/sessions/%s/stop" % self['id']
        self._request(url, method='POST')

    def stop_anonymous(self):
        url = self.address + "/api/v4/sessions/%s/terminate-external" % self['id']  # FIXME: V4 API has issue with it
        data = {"signature": self.data_signature, "testId": self['testId'], "sessionId": self['id']}
        self._request(url, to_json(data))

    def send_kpi_data(self, data, is_check_response=True):
        """
        Sends online data

        :param is_check_response:
        :type data: str
        """
        url = self.data_address + "/submit.php?session_id=%s&signature=%s&test_id=%s&user_id=%s"
        url %= self['id'], self.data_signature, self['testId'], self['userId']
        url += "&pq=0&target=%s&update=1" % self.kpi_target
        hdr = {"Content-Type": "application/json"}
        response = self._request(url, data, headers=hdr)

        if response and 'response_code' in response and response['response_code'] != 200:
            raise TaurusNetworkError("Failed to feed data, response code %s" % response['response_code'])

        if response and 'result' in response and is_check_response:
            result = response['result']['session']
            self.log.debug("Result: %s", result)
            if 'statusCode' in result and result['statusCode'] > 100:
                self.log.info("Test was stopped through Web UI: %s", result['status'])
                raise ManualShutdown("The test was interrupted through Web UI")

    def send_monitoring_data(self, src_name, data):
        self.upload_file('%s.monitoring.json' % src_name, to_json(data))

    def upload_file(self, filename, contents=None):
        """
        Upload single artifact

        :type filename: str
        :type contents: str
        :raise TaurusNetworkError:
        """
        body = MultiPartForm()

        if contents is None:
            body.add_file('file', filename)
        else:
            body.add_file_as_string('file', filename, contents)

        url = self.address + "/api/v4/image/%s/files?signature=%s"
        url %= self['id'], self.data_signature
        hdr = {"Content-Type": str(body.get_content_type())}
        response = self._request(url, body.form_as_bytes(), headers=hdr)
        if not response['result']:
            raise TaurusNetworkError("Upload failed: %s" % response)
