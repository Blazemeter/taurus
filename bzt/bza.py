"""
The idea for this module is to keep it separate from bzt codebase as much as possible,
it may become separate library in the future. Things like imports and logging should be minimal.
"""
import json
import logging
from collections import OrderedDict

import requests

from bzt import TaurusNetworkError, ManualShutdown, VERSION
from bzt.six import cookielib
from bzt.six import text_type
from bzt.six import urlencode
from bzt.utils import to_json, MultiPartForm


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
        self.http_request = requests.request

        # copy infrastructure from prototype
        if isinstance(proto, BZAObject):
            attrs_own = set(dir(BZAObject()))
            attrs_parent = set(dir(BZAObject.__bases__[0]()))
            attrs_diff = attrs_own - attrs_parent  # get only BZAObject attrs
            for attr in attrs_diff:
                if attr.startswith('__') or attr in (self._request.__name__,):
                    continue
                self.__setattr__(attr, proto.__getattribute__(attr))

    def _request(self, url, data=None, headers=None, method=None):
        """
        :param url: str
        :type data: Union[dict,str]
        :param headers: dict
        :param method: str
        :return: dict
        """
        if not headers:
            headers = {}

        headers["X-Client-Id"] = "Taurus"
        headers["X-Client-Version"] = VERSION

        if self.token:
            headers["X-Api-Key"] = self.token

        if method:
            log_method = method
        else:
            log_method = 'GET' if data is None else 'POST'

        url = str(url)

        if isinstance(data, text_type):
            data = data.encode("utf8")

        if isinstance(data, dict):
            data = to_json(data)
            headers["Content-Type"] = "application/json"

        self.log.debug("Request: %s %s %s", log_method, url, data[:self.logger_limit] if data else None)

        response = self.http_request(method=log_method, url=url, data=data, headers=headers, cookies=self._cookies,
                                     timeout=self.timeout)

        resp = response.content
        if not isinstance(resp, str):
            resp = resp.decode()

        self.log.debug("Response: %s", resp[:self.logger_limit] if resp else None)
        if response.status_code >= 400:
            raise TaurusNetworkError("API call error %s: %s %s" % (url, response.status_code, response.reason))

        try:
            result = json.loads(resp) if len(resp) else {}
        except ValueError as exc:
            self.log.debug('Response: %s', resp)
            raise TaurusNetworkError("Non-JSON response from API: %s" % exc)

        if 'error' in result and result['error']:
            raise TaurusNetworkError("API call error %s: %s" % (url, result['error']))

        return result


class BZAObjectsList(list):
    def first(self):
        """ Returns first item of non-empty list or None """
        if len(self):
            return self[0]

        return None

    def __getattr__(self, name):
        def call_list_items(*args, **kwargs):
            res = BZAObjectsList()
            for item in self:
                method = getattr(item, name)
                chunk = method(*args, **kwargs)
                if not isinstance(chunk, BZAObjectsList):
                    msg = "%s.%s() must return BZAObjectsList, but returned %s"
                    raise TypeError(msg % (type(item).__name__, name, type(chunk).__name__))
                res += chunk

            # logging.debug("%s[%s]: %s", name, len(res), json.dumps(res, indent=True))
            return res

        return call_list_items


# ================================= Entities =================================

class User(BZAObject):
    def ping(self):
        """ Quick check if we can access the service """
        self._request(self.address + '/api/v4/web/version')

    def accounts(self):
        """
        :rtype: BZAObjectsList[Account]
        """
        res = self._request(self.address + '/api/v4/accounts')
        return BZAObjectsList([Account(self, x) for x in res['result']])

    def fetch(self):
        res = self._request(self.address + '/api/v4/user')
        if 'result' in res:
            self.update(res['result'])
        else:
            self.update(res)

        return self

    def available_locations(self, include_harbors=False):
        if 'locations' not in self:
            self.fetch()

        locations = {}
        for loc in self['locations']:
            loc_id = str(loc['id'])
            if not loc_id.startswith('harbor-') or include_harbors:
                locations[loc_id] = loc
        return locations

    def collection_draft(self, name, taurus_config, resource_files):
        if resource_files:
            draft_id = "taurus_%s" % id(self.token)
            self._upload_collection_resources(resource_files, draft_id)
            taurus_config.merge({"dataFiles": {"draftId": draft_id}})

        collection_draft = self._import_config(taurus_config)
        collection_draft['name'] = name
        return collection_draft

    def _import_config(self, config):
        url = self.address + "/api/v4/multi-tests/taurus-import"
        resp = self._request(url, data=config, method="POST")
        return resp['result']

    def _upload_collection_resources(self, resource_files, draft_id):
        self.log.debug('Uploading resource files: %s', resource_files)
        url = self.address + "/api/v4/web/elfinder/%s" % draft_id
        body = MultiPartForm()
        body.add_field("cmd", "upload")
        body.add_field("target", "s1_Lw")
        body.add_field('folder', 'drafts')

        for rfile in resource_files:
            body.add_file('upload[]', rfile)

        hdr = {"Content-Type": str(body.get_content_type())}
        self._request(url, body.form_as_bytes(), headers=hdr)


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
        params = OrderedDict()
        params.update({"workspaceId": self['id']})
        params.update({"limit": 99999})
        res = self._request(self.address + '/api/v4/projects?' + urlencode(params))

        projects = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            if proj_id is not None and item['id'] != proj_id:
                continue

            projects.append(Project(self, item))
        return projects

    def locations(self, include_private=False):
        if 'locations' not in self:
            self.fetch()
        res = []
        for loc in self['locations']:
            if not loc['id'].startswith('harbor-') or include_private:
                res.append(Location(self, loc))

        return BZAObjectsList(res)

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
        params = OrderedDict({"workspaceId": self['id']})
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
        params = OrderedDict({"workspaceId": self['id']})
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
        params = {"name": str(proj_name), "workspaceId": self['id']}
        data = self._request(self.address + '/api/v4/projects', params)
        return Project(self, data['result'])

    def fetch(self):
        res = self._request(self.address + '/api/v4/workspaces/%s' % self['id'])
        self.update(res['result'])
        return self


class Location(BZAObject):
    pass


class Project(BZAObject):
    def tests(self, name=None, test_type=None):
        """
        :rtype: BZAObjectsList[Test]
        """
        params = OrderedDict({"projectId": self['id']})
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
        params = OrderedDict({"projectId": self['id']})
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
        resp = self._request(url, data)
        return Test(self, resp['result'])

    def create_multi_test(self, collection_draft):
        collection_draft['projectId'] = self['id']
        url = self.address + "/api/v4/multi-tests"
        resp = self._request(url, data=collection_draft, method="POST")
        return MultiTest(self, resp['result'])


class Test(BZAObject):
    def start_external(self):
        url = self.address + "/api/v4/tests/%s/start-external" % self['id']
        res = self._request(url, method='POST')
        result = res['result']
        session = Session(self, result['session'])
        session.data_signature = result['signature']
        return session, Master(self, result['master'])

    def start_anonymous_external_test(self):
        """
        :rtype: (Session,Master,str)
        """
        url = self.address + "/api/v4/sessions"
        res = self._request(url, method='POST')
        result = res['result']
        session = Session(self, result['session'])
        session.data_signature = result['signature']
        return session, Master(self, result['master']), result['publicTokenUrl']

    def get_files(self):
        path = self.address + "/api/v4/web/elfinder/%s" % self['id']
        query = urlencode(OrderedDict({'cmd': 'open', 'target': 's1_Lw'}))
        url = path + '?' + query
        response = self._request(url)
        return response["files"]

    def delete_files(self):
        files = self.get_files()
        self.log.debug("Test files: %s", [filedict['name'] for filedict in files])
        if not files:
            return
        path = "/api/v4/web/elfinder/%s" % self['id']
        query = "cmd=rm&" + "&".join("targets[]=%s" % fname['hash'] for fname in files)
        url = self.address + path + '?' + query
        response = self._request(url)
        if len(response['removed']) == len(files):
            self.log.debug("Successfully deleted %d test files", len(response['removed']))
        return response['removed']

    def start(self):
        url = self.address + "/api/v4/tests/%s/start" % self['id']
        resp = self._request(url, method='POST')
        master = Master(self, resp['result'])
        return master

    def upload_files(self, taurus_config, resource_files):
        self.log.debug("Uploading files into the test: %s", resource_files)
        url = '%s/api/v4/tests/%s/files' % (self.address, self['id'])

        body = MultiPartForm()
        body.add_file_as_string('script', 'taurus.yml', taurus_config)

        for rfile in resource_files:
            body.add_file('files[]', rfile)

        hdr = {"Content-Type": str(body.get_content_type())}
        self._request(url, body.form_as_bytes(), headers=hdr)


class MultiTest(BZAObject):
    def start(self):
        # NOTE: delayedStart=true means that BM will not start test until all instances are ready
        # if omitted - instances will start once ready (not simultaneously),
        # which may cause inconsistent data in aggregate report.
        url = self.address + "/api/v4/multi-tests/%s/start?delayedStart=true" % self['id']
        resp = self._request(url, method="POST")
        return Master(self, resp['result'])

    def stop(self):
        url = self.address + "/api/v4/multi-tests/%s/stop" % self['id']
        self._request(url, method='POST')

    def update_collection(self, coll):
        url = self.address + "/api/v4/multi-tests/%s" % self['id']
        self._request(url, data=coll, method="PATCH")


class Master(BZAObject):
    def make_report_public(self):
        url = self.address + "/api/v4/masters/%s/public-token" % self['id']
        res = self._request(url, {"publicToken": None}, method="POST")
        public_token = res['result']['publicToken']
        report_link = self.address + "/app/?public-token=%s#/masters/%s/summary" % (public_token, self['id'])
        return report_link

    def send_custom_metrics(self, data):
        url = self.address + "/api/v4/data/masters/%s/custom-metrics" % self['id']
        res = self._request(url, data, method="POST")
        return res

    def send_custom_tables(self, data):
        url = self.address + "/api/v4/data/masters/%s/custom-table" % self['id']
        res = self._request(url, data, method="POST")
        return res

    def fetch(self):
        url = self.address + "/api/v4/masters/%s" % self['id']
        res = self._request(url)
        self.update(res['result'])
        return self

    def set(self, data):
        url = self.address + "/api/v4/masters/%s" % self['id']
        res = self._request(url, data, method='PATCH')
        self.update(res['result'])

    def get_status(self):
        sess = self._request(self.address + '/api/v4/masters/%s/status' % self['id'])
        return sess['result']

    def sessions(self):
        sess = self._request(self.address + '/api/v4/masters/%s/sessions' % self['id'])
        if 'sessions' in sess['result']:
            arr = sess['result']['sessions']
        else:
            arr = sess['result']

        return BZAObjectsList([Session(self, x) for x in arr])

    def get_kpis(self, min_ts):
        params = [
            ("interval", 1),
            ("from", min_ts),
            ("master_ids[]", self['id']),
        ]
        for item in ('t', 'lt', 'by', 'n', 'ec', 'ts', 'na'):
            params.append(("kpis[]", item))

        labels = self.get_labels()
        for label in labels:
            params.append(("labels[]", label['id']))

        url = self.address + "/api/v4/data/kpis?" + urlencode(params)
        res = self._request(url)
        return res['result']

    def get_labels(self, ):
        url = self.address + "/api/v4/data/labels?" + urlencode({'master_id': self['id']})
        res = self._request(url)
        return res['result']

    def get_aggregate_report(self):
        url = self.address + "/api/v4/masters/%s/reports/aggregatereport/data" % self['id']
        res = self._request(url)
        return res['result']

    def force_start(self):
        url = self.address + "/api/v4/masters/%s/force-start" % self['id']
        self._request(url, method="POST")

    def stop(self):
        url = self.address + "/api/v4/masters/%s/stop"
        self._request(url % self['id'], method='POST')

    def get_full(self):
        url = self.address + "/api/v4/masters/%s/full" % self['id']
        return self._request(url)['result']


class Session(BZAObject):
    def __init__(self, proto=None, data=None):
        super(Session, self).__init__(proto, data)
        self.data_signature = None
        self.kpi_target = 'labels_bulk'

    def fetch(self):
        url = self.address + "/api/v4/sessions/%s" % self['id']
        res = self._request(url)
        self.update(res['result'])
        return self

    def set(self, data):
        url = self.address + "/api/v4/sessions/%s" % self['id']
        res = self._request(url, data, method='PATCH')
        self.update(res['result'])

    def stop(self):
        url = self.address + "/api/v4/sessions/%s/stop" % self['id']
        self._request(url, method='POST')

    def stop_anonymous(self):
        url = self.address + "/api/v4/sessions/%s/terminate-external" % self['id']  # FIXME: V4 API has issue with it
        data = {"signature": self.data_signature, "testId": self['testId'], "sessionId": self['id']}
        self._request(url, data)

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

    def send_monitoring_data(self, engine_id, data):
        self.upload_file('%s-%s-c.monitoring.json' % (self['id'], engine_id), to_json(data))

    def upload_file(self, filename, contents=None):
        """
        Upload single artifact

        :type filename: str
        :type contents: str
        :raise TaurusNetworkError:
        """
        body = MultiPartForm()  # TODO: can we migrate off it, and use something native to requests lib?
        # maybe http://stackoverflow.com/questions/12385179/how-to-send-a-multipart-form-data-with-requests-in-python

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

    def get_logs(self):
        url = self.address + "/api/v4/sessions/%s/reports/logs" % self['id']
        return self._request(url)['result']['data']
