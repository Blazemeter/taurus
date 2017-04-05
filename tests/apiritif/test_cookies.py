from unittest import TestCase

from apiritif import http


class MyTest(TestCase):
    def test_cookies(self):
        response = http.get('http://example.com', cookies={"foo": "bar"})
        response.assert_ok()
