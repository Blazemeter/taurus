import json
import logging
from StringIO import StringIO

from bzt.modules.loadosophia import Loadosophia, LoadosophiaClient
from tests import BZTestCase, random_datapoint
from tests.mocks import EngineEmul


class TestLoadosophia(BZTestCase):
    def test_check(self):
        client = LoadosophiaClientEmul(logging.getLogger(''))
        client.results.append(ResponseEmul(201, {'OnlineID': "1"}))
        client.results.append(ResponseEmul(202, {}))
        client.results.append(ResponseEmul(202, {}))
        client.results.append(ResponseEmul(202, {}))
        client.results.append(ResponseEmul(205, {}))

        obj = Loadosophia()
        obj.engine = EngineEmul()
        obj.client = client
        obj.prepare()
        obj.startup()
        obj.aggregated_second(random_datapoint(0))
        obj.aggregated_second(random_datapoint(1))
        obj.check()
        obj.aggregated_second(random_datapoint(10))
        obj.shutdown()
        obj.post_process()


class LoadosophiaClientEmul(LoadosophiaClient):
    def __init__(self, parent_logger):
        super(LoadosophiaClientEmul, self).__init__(parent_logger)
        self.results = []

    def _get_response(self, request):
        return self._request(request)

    def _get_request(self, url):
        return self._request(url)

    def _get_opener_response(self, data, url):
        return self._request(url, data)

    def _request(self, url, data=None):
        self.log.debug("Request %s: %s", url, data)
        res = self.results.pop(0)
        self.log.debug("Response: %s", res)
        return res


class ResponseEmul(object):
    def __init__(self, code, response):
        super(ResponseEmul, self).__init__()
        self.code = code
        self.response = StringIO(json.dumps(response))

    def getcode(self):
        return self.code

    def read(self):
        self.response.seek(0)
        return self.response.read()

    def __repr__(self):
        return "%s/%s" % (self.code, self.response.read())