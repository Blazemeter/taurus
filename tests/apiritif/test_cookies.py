from apiritif import APITestCase


class MyTest(APITestCase):
    def test_cookies(self):
        response = self.request('http://example.com', cookies={"foo": "bar"})
        self.assertOk(response)
