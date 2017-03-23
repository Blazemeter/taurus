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

    def request(self, url, method='GET', timeout=None):
        if self.keep_alive and self.session is None:
            self.session = requests.Session()

        if self.keep_alive:
            response = self.session.request(method, url, timeout=timeout)
        else:
            response = requests.request(method, url, timeout=timeout)

        self.request_log.append({"url": url, "method": method, "timeout": timeout, "response": response})
        return response

    def head(self, url, **kwargs):
        return self.request(url, method='HEAD', **kwargs)

    def get(self, url, **kwargs):
        return self.request(url, method='GET', **kwargs)

    def post(self, url, **kwargs):
        return self.request(url, method='POST', **kwargs)

    def put(self, url, **kwargs):
        return self.request(url, method='PUT', **kwargs)

    def patch(self, url, **kwargs):
        return self.request(url, method='PATCH', **kwargs)

    def delete(self, url, **kwargs):
        return self.request(url, method='DELETE', **kwargs)

    def assertOk(self, response, msg=None):
        self.assertTrue(response.ok, msg=msg)

    def assertFailed(self, response, msg=None):
        self.assertTrue(response.status_code >= 400, msg=msg)

    def assert200(self, response, msg=None):
        self.assertEqual(response.status_code, 200, msg=msg)

    def assertStatusCode(self, response, code, msg=None):
        self.assertEqual(response.status_code, code, msg=msg)
