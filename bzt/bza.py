"""
The idea for this module is to keep it separate from bzt codebase as much as possible,
it may become separate library in the future. Things like imports and logging should be minimal.
"""
import base64
import json
import logging
import time
import traceback
from collections import OrderedDict
from functools import wraps
from urllib.parse import urlencode

import requests

from bzt import ManualShutdown, TaurusException, TaurusNetworkError
from bzt.resources.version import VERSION
from bzt.utils import to_json, MultiPartForm, NETWORK_PROBLEMS

BZA_TEST_DATA_RECEIVED = 100
ENDED = 140
PERMANENT_ERROR_CODES = [500]


def send_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        try:
            method(self, *args, **kwargs)
        except (IOError, TaurusNetworkError):
            error_msg = f"Error sending data: {traceback.format_exc()}"

            if any(f"API call error {code}" in error_msg for code in PERMANENT_ERROR_CODES):
                self.log.error(error_msg)
                return

            self.log.debug(error_msg)
            self.log.warning("Failed to send data, will retry in %s sec...", self.timeout)
            try:
                time.sleep(self.timeout)
                method(self, *args, **kwargs)
                self.log.info("Succeeded with retry")
            except NETWORK_PROBLEMS:
                self.log.error("Fatal error sending data: %s", traceback.format_exc())
                self.log.warning("Will skip failed data and continue running")

    return _impl


def get_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        while True:
            try:
                return method(self, *args, **kwargs)
            except NETWORK_PROBLEMS:
                self.log.debug("Error making request: %s", traceback.format_exc())
                self.log.warning("Failed to make request, will retry in %s sec...", self.timeout)
                time.sleep(self.timeout)

    return _impl


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
        self.timeout = 30
        self.logger_limit = 256
        self.token = None
        self.log = logging.getLogger(self.__class__.__name__)
        self.http_session = requests.Session()
        self.http_request = self.http_session.request
        self._retry_limit = 5

        # copy infrastructure from prototype
        if isinstance(proto, BZAObject):
            attrs_own = set(dir(BZAObject()))
            attrs_parent = set(dir(BZAObject.__bases__[0]()))
            attrs_diff = attrs_own - attrs_parent  # get only BZAObject attrs
            for attr in attrs_diff:
                if attr.startswith('__') or attr in (self._request.__name__,):
                    continue
                self.__setattr__(attr, proto.__getattribute__(attr))

    def _request(self, url, data=None, headers=None, method=None, raw_result=False, retry=True):
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

        has_auth = headers and "X-Api-Key" in headers
        if has_auth:
            pass  # all is good, we have auth provided
        elif isinstance(self.token, str) and ':' in self.token:
            token = self.token
            if isinstance(token, str):
                token = token.encode('ascii')
            token = base64.b64encode(token).decode('ascii')
            headers['Authorization'] = 'Basic ' + token
        elif self.token:
            headers["X-Api-Key"] = self.token

        if method:
            log_method = method
        else:
            log_method = 'GET' if data is None else 'POST'

        url = str(url)

        if isinstance(data, str):
            data = data.encode("utf-8")

        if isinstance(data, (dict, list)):
            data = to_json(data)
            headers["Content-Type"] = "application/json"

        self.log.debug("Request: %s %s %s", log_method, url, data[:self.logger_limit] if data else None)

        retry_limit = self._retry_limit

        while True:
            try:
                response = self.http_request(
                    method=log_method, url=url, data=data, headers=headers, timeout=self.timeout)
            except requests.ReadTimeout:
                if retry and retry_limit:
                    retry_limit -= 1
                    self.log.warning("ReadTimeout: %s. Retry..." % url)
                    continue
                raise
            break

        resp = response.content
        if not isinstance(resp, str):
            resp = resp.decode()

        self.log.debug("Response [%s]: %s", response.status_code, resp[:self.logger_limit] if resp else None)
        if response.status_code >= 400:
            try:
                result = json.loads(resp) if len(resp) else {}
                if 'error' in result and result['error']:
                    raise TaurusNetworkError("API call error %s: %s" % (url, result['error']))
                else:
                    raise TaurusNetworkError("API call error %s on %s: %s" % (response.status_code, url, result))
            except ValueError:
                raise TaurusNetworkError("API call error %s: %s %s" % (url, response.status_code, response.reason))

        if raw_result:
            return resp

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

    def accounts(self, ident=None, name=None):
        """
        :rtype: BZAObjectsList[Account]
        """
        res = self._request(self.address + '/api/v4/accounts')
        accounts = []
        for acc in res['result']:
            if ident is not None and acc['id'] != ident:
                continue
            if name is not None and acc['name'] != name:
                continue
            accounts.append(Account(self, acc))
        return BZAObjectsList(accounts)

    def fetch(self):
        res = self._request(self.address + '/api/v4/user')
        if 'result' in res:
            self.update(res['result'])
        else:
            self.update(res)

        return self

    def available_locations(self, include_harbors=False):
        self.log.warning("Deprecated method used: available_locations")
        if 'locations' not in self:
            self.fetch()

        locations = {}
        for loc in self['locations']:
            loc_id = str(loc['id'])
            if not loc_id.startswith('harbor-') or include_harbors:
                locations[loc_id] = loc
        return locations

    def test_by_ids(self, account_id=None, workspace_id=None, project_id=None, test_id=None, test_type=None):
        account = self.accounts(ident=account_id).first()
        if not account:
            raise ValueError("Account not found: %s" % account_id)
        workspace = account.workspaces(ident=workspace_id).first()
        if workspace is None:
            raise ValueError("Workspace not found: %s" % workspace_id)
        project = workspace.projects(ident=project_id).first()
        if project:
            target = project
        else:
            target = workspace

        test = target.tests(ident=test_id, test_type=test_type).first()
        if not test:
            raise ValueError("Test wasn't found")

        return account, workspace, project, test


class Account(BZAObject):
    def workspaces(self, ident=None, name=None):
        """
        :rtype: BZAObjectsList[Workspace]
        """
        params = {"accountId": self['id'], 'enabled': 'true', 'limit': 100}
        params = OrderedDict(sorted(params.items(), key=lambda t: t[0]))
        res = self._request(self.address + '/api/v4/workspaces?' + urlencode(params))
        workspaces = []
        for wksp in res['result']:
            if not wksp['enabled']:
                continue

            if name is not None and wksp['name'] != name:
                continue

            if ident is not None and wksp['id'] != ident:
                continue

            workspaces.append(Workspace(self, wksp))

        return BZAObjectsList(workspaces)


class Workspace(BZAObject):
    def projects(self, name=None, ident=None):
        """
        :rtype: BZAObjectsList[Project]
        """
        params = OrderedDict()
        params.update({"workspaceId": self['id']})

        if name:
            params.update({"name": name})

        if ident:
            params.update({"limit": 1000})

        res = self._request(self.address + '/api/v4/projects?' + urlencode(params))

        projects = BZAObjectsList()
        for item in res['result']:
            if name is not None and item['name'] != name:
                continue

            if ident is not None and item['id'] != ident:
                continue

            projects.append(Project(self, item))
        return BZAObjectsList(projects)

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

    def tests(self, name=None, ident=None, test_type=None):
        """
        :rtype: BZAObjectsList[Test]
        """
        params = OrderedDict({"workspaceId": self['id']})
        if name is not None:
            params["name"] = name
        if ident is not None:
            params["id"] = ident

        res = self._request(self.address + '/api/v4/tests?' + urlencode(params))
        tests = BZAObjectsList()
        for item in res['result']:
            if ident is not None and item['id'] != ident:
                continue

            if name and item['name'] != name:
                continue

            if test_type and item['configuration']['type'] != test_type:
                continue

            tests.append(Test(self, item))
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
    def tests(self, name=None, ident=None, test_type=None):
        """
        :rtype: BZAObjectsList[Test]
        """
        params = OrderedDict({"projectId": self['id']})
        if name is not None:
            params["name"] = name
        if ident is not None:
            params["id"] = ident

        res = self._request(self.address + '/api/v4/tests?' + urlencode(params))
        tests = BZAObjectsList()
        for item in res['result']:
            if ident is not None and item['id'] != ident:
                continue

            if name is not None and item['name'] != name:
                continue

            if test_type is not None and item['configuration']['type'] != test_type:
                continue

            tests.append(Test(self, item))
        return tests

    def create_test(self, name, configuration):
        """
        :param name:
        :param configuration:
        :rtype: Test
        """
        self.log.debug("Creating new test")
        url = self.address + '/api/v4/tests'
        data = {"name": name, "projectId": self['id'], "configuration": configuration}
        resp = self._request(url, data)
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
        path = self.address + f"/api/v4/tests/{self['id']}/files"
        response = self._request(path, method="GET")
        return response["result"]

    def delete_files(self):
        files = self.get_files()
        self.log.debug("Test files: %s", [filedict['name'] for filedict in files])
        if not files:
            return
        path = f"/api/v4/tests/{self['id']}/delete-file"
        query = "&".join(f"fileName={fname['name']}" for fname in files)
        url = self.address + path + "?" + query
        response = self._request(url, method="POST")
        removed = query.count('fileName')
        if response['result']:
            self.log.debug(f"Successfully deleted {removed} test files.")
        else:
            self.log.debug("Error while test files deletion.")
        return removed

    def start(self, as_functional=False):
        url = self.address + "/api/v4/tests/%s/start" % self['id']
        if as_functional:
            url += "?functionalExecution=true"
        resp = self._request(url, method='POST')
        master = Master(self, resp['result'])
        return master

    def upload_files(self, taurus_config, resource_files):
        self.log.debug("Uploading files into the test: %s", resource_files)
        url = '%s/api/v4/tests/%s/files' % (self.address, self['id'])

        body = MultiPartForm()
        body.add_file_as_string('script', 'taurus.yml', taurus_config)

        for idx, rfile in enumerate(resource_files):
            body.add_file('files[{}]'.format(idx), rfile)

        hdr = {"Content-Type": str(body.get_content_type())}
        self._request(url, body.form_as_bytes(), headers=hdr)

    def update_props(self, coll):
        url = self.address + "/api/v4/tests/%s" % self['id']
        res = self._request(url, data=coll, method="PATCH")
        return res['result']

    def started_passfail_validation(self):
        # validate passfail configuration, get success if started
        url = f"{self.address}/api/v4/tests/{self['id']}/validate"
        resp = self._request(url, data={"files": [{"fileName": "taurus.yml"}], "performDataMerge": False})
        result = resp.get('result')
        if 'success' in result:
            return result['success']
        return False

    def get_passfail_validation(self):
        # get passfail validation status and results, log warnings if present
        url = f"{self.address}/api/v4/tests/{self['id']}/validations"
        resp = self._request(url, method='GET')
        validated = False
        for result in resp.get('result'):
            if result['status'] == 100:
                for warning_msg in result['warnings'] + result['fileWarnings']:
                    self.log.warning(f"Passfail Warning: {warning_msg}")
                validated = True
        if not validated:
            self.log.debug(f"Passfail error: Unable to validate by {url}.")
        return validated


class Master(BZAObject):
    def __init__(self, proto=None, data=None):
        super(Master, self).__init__(proto, data)
        self.warned_of_too_much_labels = False

    def make_report_public(self):
        url = self.address + "/api/v4/masters/%s/public-token" % self['id']
        res = self._request(url, data={"publicToken": None})
        public_token = res['result']['publicToken']
        report_link = self.address + "/app/?public-token=%s#/masters/%s/summary" % (public_token, self['id'])
        return report_link

    def fetch(self):
        url = self.address + "/api/v4/masters/%s" % self['id']
        res = self._request(url)
        self.update(res['result'])
        return self

    def set(self, data):
        url = self.address + "/api/v4/masters/%s" % self['id']
        res = self._request(url, data, method='PATCH')
        self.update(res['result'])

    @get_with_retry
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

        labels = self.get_labels()[:100]
        if len(labels) == 100 and not self.warned_of_too_much_labels:
            self.log.warn("Using only first 100 labels, while test has more labels")
            self.warned_of_too_much_labels = True

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

    def get_errors(self):
        url = self.address + "/api/v4/masters/%s/reports/errorsreport/data?noDataError=false" % self['id']
        res = self._request(url)
        return res['result']

    def force_start(self):
        url = self.address + "/api/v4/masters/%s/force-start" % self['id']
        self._request(url, method="POST")

    def stop(self):
        url = self.address + "/api/v4/masters/%s/stop"
        self._request(url % self['id'], method='POST')

    def terminate(self):
        url = self.address + "/api/v4/masters/%s/terminate"
        self._request(url % self['id'], method='POST')

    def get_full(self):
        url = self.address + "/api/v4/masters/%s/full" % self['id']
        return self._request(url)['result']

    def get_thresholds(self):
        url = self.address + "/api/v4/masters/%s/reports/thresholds?external=false&source=default" % self['id']
        return self._request(url)['result']

    def get_functional_report_groups(self):
        url = self.address + "/api/v4/masters/%s/reports/functional/groups" % self['id']
        return self._request(url)['result']

    def get_functional_report_group(self, group_id):
        url = self.address + "/api/v4/masters/%s/reports/functional/groups/%s" % (self['id'], group_id)
        return self._request(url)['result']


class Session(BZAObject):
    def __init__(self, proto=None, data=None):
        super(Session, self).__init__(proto, data)
        self.data_signature = None
        self.kpi_target = 'labels_bulk'
        self.monitoring_upload_notified = False

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

    def terminate(self):
        url = self.address + "/api/v4/sessions/%s/terminate" % self['id']
        self._request(url, method='POST')

    def stop_anonymous(self):
        url = self.address + "/api/v4/sessions/%s/terminate-external" % self['id']
        data = {"signature": self.data_signature, "testId": self['testId'], "sessionId": self['id']}
        self._request(url, data)

    @send_with_retry
    def send_kpi_data(self, data, is_check_response=True, submit_target=None):
        """
        Sends online data

        :type submit_target: str
        :param is_check_response:
        :type data: str
        """
        submit_target = self.kpi_target if submit_target is None else submit_target
        url = self.data_address + "/submit.php?session_id=%s&signature=%s&test_id=%s&user_id=%s"
        url %= self['id'], self.data_signature, self['testId'], self['userId']
        url += "&pq=0&target=%s&update=1" % submit_target
        hdr = {"Content-Type": "application/json"}
        response = self._request(url, data, headers=hdr)

        if response and 'response_code' in response and response['response_code'] != 200:
            raise TaurusNetworkError("Failed to feed data to %s, response code %s" %
                                     (submit_target, response['response_code']))

        if response and 'result' in response and is_check_response:
            result = response['result']['session']
            self.log.debug("Result: %s", result)
            if 'statusCode' in result and result['statusCode'] > 100:
                self.log.info("Test was stopped through Web UI: %s", result['status'])
                raise ManualShutdown("The test was interrupted through Web UI")

    def send_monitoring_data(self, engine_id, data):
        file_name = '%s-%s-c.monitoring.json' % (self['id'], engine_id)
        self.upload_file(file_name, to_json(data))
        if not self.monitoring_upload_notified:
            self.log.debug("Sending engine health notification")
            self.notify_monitoring_file(file_name)
            self.monitoring_upload_notified = True

    @send_with_retry
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

        url = self.data_address + "/api/v4/image/%s/files?signature=%s"
        url %= self['id'], self.data_signature
        master_id = self.get('masterId')
        if master_id:
            url += '&masterId={}'.format(master_id)
        hdr = {"Content-Type": str(body.get_content_type())}
        response = self._request(url, body.form_as_bytes(), headers=hdr)
        if not response['result']:
            raise TaurusNetworkError("Upload failed: %s" % response)

    def get_logs(self):
        url = self.address + "/api/v4/sessions/%s/reports/logs" % self['id']
        return self._request(url)['result']['data']

    def notify_monitoring_file(self, file_name):
        data = {
            'fileName': file_name,
        }
        data_str = json.dumps(data)
        self.send_kpi_data(data_str, submit_target='engine_health')


class BZAProxyException(TaurusException):
    pass


class BZAProxy(BZAObject):
    def __init__(self):
        super(BZAProxy, self).__init__()
        self.delay = 5

    def stop(self):
        self._request(self._get_url("/recording/stop"), method='POST')

    def start(self):
        self._request(self._get_url("/recording/start"), method='POST')

    def get_jmx(self, smart=False):
        path = '/download?format=jmx&smart=' + str(smart).lower()
        response_url = self._request(self._get_url(path)).get('result')
        response_content = self._request(response_url, raw_result=True, headers={"X-Api-Key": self.token})
        return response_content

    def _get_url(self, path=''):
        return self.address + "/api/latest/proxy" + path

    def get_addr(self):
        response = self._request(self._get_url())

        proxy_info = response['result']
        if proxy_info:
            self.log.info('Proxy already exists')
            if 'username' in proxy_info:
                msg = 'Proxy has auth, unable to use it'
                self.log.info(msg)
                raise BZAProxyException(msg)

            self.log.info('Using existing recording proxy...')
            if proxy_info['status'] == 'active':
                self.log.info('Proxy is active, stop it')
                self.stop()
        else:
            self.log.info('Creating new recording proxy...')
            response = self._request(self._get_url(), data={'auth': False, 'shipId': None})
            proxy_info = response['result']

        self.log.info('Clear proxy...')
        self._request(self._get_url("/recording/clear"), method='POST')

        return 'http://%s:%s' % (proxy_info['host'], proxy_info['port'])

    def get_json(self):
        response = self._request(self._get_url("/download?format=json"), raw_result=True)
        return response
