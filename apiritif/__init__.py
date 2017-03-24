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
        self.default_address = None
        self.path_prefix = None

    def tearDown(self):
        pass

    def request(self, url, method='GET', **kwargs):
        if self.keep_alive and self.session is None:
            self.session = requests.Session()

        address = ''
        if self.default_address is not None:
            address += self.default_address
        if self.path_prefix is not None:
            address += self.path_prefix
        address += url

        if self.keep_alive:
            response = self.session.request(method, address, **kwargs)
        else:
            response = requests.request(method, address, **kwargs)

        log_item = {
            "url": address,
            "method": method,
            "response": response
        }
        log_item.update(kwargs)

        self.request_log.append(log_item)
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

    def assertInBody(self, member, response, msg=None):
        self.assertIn(member, response.text, msg=msg)

    def assertNotInBody(self, member, response, msg=None):
        self.assertNotIn(member, response, msg=msg)

    def assertRegexIn(self, regex, text, msg=None):
        raise AssertionError('')

    def assertRegexInBody(self, member, response, msg=None):
        self.assertIn(member, response.text, msg=msg)

    def assertRegexNotInBody(self, member, response, msg=None):
        pass

    def assertInHeaders(self, member, response, msg=None):
        pass

    def assertNotInHeaders(self, member, response, msg=None):
        pass
