import json
import logging

from bzt import TaurusNetworkError
from bzt.six import Request
from bzt.six import text_type
from bzt.six import urlencode
from bzt.six import urlopen


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

        if isinstance(proto, BZAObject):  # TODO: do we have anything more graceful to handle this?
            self.address = proto.address
            self.data_address = proto.data_address
            self.timeout = proto.timeout
            self.logger_limit = proto.logger_limit
            self.token = proto.token
            self._request = proto._request  # for unit tests override

    def _request(self, url, data=None, headers=None, checker=None, method=None):
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
        req = Request(url, data, headers)
        if method:
            req.get_method = lambda: method

        response = urlopen(req, timeout=self.timeout)

        if checker:
            checker(response)

        resp = response.read()
        if not isinstance(resp, str):
            resp = resp.decode()

        self.log.debug("Response: %s", resp[:self.logger_limit] if resp else None)

        try:
            return json.loads(resp) if len(resp) else {}
        except ValueError as exc:
            self.log.debug('Response: %s', resp)
            raise TaurusNetworkError("Non-JSON response from API: %s" % exc)

    def ping(self):
        """
        Quick check if we can access the service
        """
        self._request(self.address + '/api/v4/web/version')


class BZAObjectsList(list):
    def __getattr__(self, name):
        def call_list_items(**kwargs):
            res = BZAObjectsList()
            for item in self:
                method = getattr(item, name)
                chunk = method(**kwargs)
                if not isinstance(chunk, BZAObjectsList):
                    msg = "%s.%s() must return BZAObjectsList, but returned %s"
                    raise TypeError(msg % (type(item).__name__, name, type(chunk).__name__))
                res += chunk

            logging.debug("%s[%s]: %s", name, len(res), json.dumps(res, indent=True))
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

    def get_user(self):
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

    def create_test(self, name, configuration):
        self.log.debug("Creating new test")
        url = self.address + '/api/latest/tests'
        data = {"name": name, "projectId": self['id'], "configuration": configuration}
        hdr = {"Content-Type": " application/json"}
        resp = self._request(url, json.dumps(data), headers=hdr)
        return Test(self, resp['result'])


class Test(BZAObject):
    pass


class Master(BZAObject):
    pass


class Session(BZAObject):
    pass
