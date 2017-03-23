from unittest import TestCase
import requests


class APITestCase(TestCase):
    """
    Base class for API test cases.

    Contains a bunch of utility functions (request, get, post, etc) and a few helpful assertions.
    """
    def setUp(self):
        self.request_log = []
        self.keep_alive = True
        self.session = None

    def tearDown(self):
        pass

    def request(self, url, method='GET'):
        if self.keep_alive and self.session is None:
            self.session = requests.Session()

        if self.keep_alive:
            response = self.session.request(method, url)
        else:
            response = requests.request(method, url)

        self.request_log.append({"url": url, "method": method, "response": response})
        return response

    def head(self, url):
        return self.request(url, method='HEAD')

    def get(self, url):
        return self.request(url, method='GET')

    def post(self, url):
        return self.request(url, method='POST')

    def put(self, url):
        return self.request(url, method='PUT')

    def patch(self, url):
        return self.request(url, method='PATCH')

    def delete(self, url):
        return self.request(url, method='DELETE')

    def assertOk(self, response, msg=None):
        self.assertTrue(response.ok, msg=msg)

    def assertFailed(self, response, msg=None):
        self.assertTrue(response.status_code >= 400, msg=msg)

    def assert200(self, response, msg=None):
        self.assertEqual(response.status_code, 200, msg=msg)

    def assertStatusCode(self, response, code, msg=None):
        self.assertEqual(response.status_code, code, msg=msg)
