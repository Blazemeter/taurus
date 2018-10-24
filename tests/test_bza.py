from unittest import skipUnless

from bzt import TaurusNetworkError
from bzt.bza import User, BZAObject
from bzt.six import PY3, text_type
from tests import BZTestCase
from tests.mocks import BZMock


class TestBZAClient(BZTestCase):
    @skipUnless(PY3, "Py3-only test")
    def test_bza_py3_unicode_token(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/web/version': {"result": {}},
        })

        user = User()
        mock.apply(user)
        user.token = text_type("something:something")
        user.ping()


class TestBZAObject(BZTestCase):
    def test_ping(self):
        obj = User()
        obj.ping()

    def test_request(self):
        obj = BZAObject()
        try:
            obj._request('https://a.blazemeter.com/api/v4/web/version', data={"test": 1})
            self.fail()
        except TaurusNetworkError:
            pass
