from apiritif import APITestCase


class MyTest(APITestCase):
    def test_example(self):
        response = self.request('http://example.com')
        self.assertOk(response)
