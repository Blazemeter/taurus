from unittest import skipUnless

from bzt.bza import User
from bzt.six import PY3, text_type
from tests import BZTestCase


class TestBZAClient(BZTestCase):
    @skipUnless(PY3, "Py3-only test")
    def test_bza_py3_unicode_token(self):
        user = User()
        user.token = text_type("something:something")
        user.ping()
