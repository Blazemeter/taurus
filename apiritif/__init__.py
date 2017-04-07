import inspect
import re
from collections import OrderedDict
from functools import wraps
from io import BytesIO

import jsonpath_rw
import requests
from lxml import etree


def headers_as_text(headers_dict):
    return "\n".join("%s: %s" % (key, value) for key, value in headers_dict.items())


def shorten(string, upto, end_with="..."):
    return string[:upto - len(end_with)] + end_with if len(string) > upto else string


def assert_regexp(regex, text, match=False, msg=None):
    if match:
        if re.match(regex, text) is None:
            msg = msg or "Regex %r didn't match expected value: %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)
    else:
        if not re.findall(regex, text):
            msg = msg or "Regex %r didn't find anything in text %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)


def assert_not_regexp(regex, text, match=False, msg=None):
    if match:
        if re.match(regex, text) is not None:
            msg = msg or "Regex %r unexpectedly matched expected value: %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)
    else:
        if re.findall(regex, text):
            msg = msg or "Regex %r unexpectedly found something in text %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)


class http(object):
    @staticmethod
    def target(*args, **kwargs):
        return HTTPTarget(*args, **kwargs)

    @staticmethod
    def request(method, address, session=None,
                params=None, headers=None, cookies=None, data=None, json=None, allow_redirects=False, timeout=30):
        """

        :param method: str
        :param address: str
        :return: response
        :rtype: HTTPResponse
        """
        if session is None:
            session = requests.Session()
        request = requests.Request(method, address,
                                   params=params, headers=headers, cookies=cookies, json=json, data=data)
        prepared = request.prepare()
        response = session.send(prepared, allow_redirects=allow_redirects, timeout=timeout)
        wrapped_response = HTTPResponse.from_py_response(response)
        recorder.record_http_request(method, address, prepared, wrapped_response, session)
        return wrapped_response

    @staticmethod
    def get(address, **kwargs):
        return http.request("GET", address, **kwargs)

    @staticmethod
    def post(address, **kwargs):
        return http.request("POST", address, **kwargs)

    @staticmethod
    def put(address, **kwargs):
        return http.request("PUT", address, **kwargs)

    @staticmethod
    def delete(address, **kwargs):
        return http.request("DELETE", address, **kwargs)

    @staticmethod
    def patch(address, **kwargs):
        return http.request("PATCH", address, **kwargs)

    @staticmethod
    def head(address, **kwargs):
        return http.request("HEAD", address, **kwargs)


class LogAssertion(object):
    def __init__(self, name, response):
        self.name = name
        self.response = response
        self.is_failed = False
        self.failure_message = ""

    def __repr__(self):
        tmpl = "Assertion(name=%r, response=%r, is_failed=%r, failure_message=%r)"
        return tmpl % (self.name, self.response, self.is_failed, self.failure_message)


class LogRequest(object):
    def __init__(self, method, address, request, response, session):
        self.method = method
        self.address = address
        self.request = request
        self.response = response
        self.session = session


class Record(object):
    def __init__(self):
        """
        :type requests: list[LogRequest]
        :type assertions: list[LogAssertion]
        """
        self.requests = []
        self.assertions = []

    def add_request(self, method, address, request, response, session):
        self.requests.append(LogRequest(method, address, request, response, session))

    def assertions_for_response(self, response):
        return [
            ass
            for ass in self.assertions
            if ass.response == response
        ]

    def add_assertion(self, assertion_name, target_response):
        self.assertions.append(LogAssertion(assertion_name, target_response))

    def __repr__(self):
        tmpl = "Record(requests=%r, assertions=%r)"
        return tmpl % (self.requests, self.assertions)


# TODO: thread-safe version?
class _Recorder(object):
    def __init__(self):
        self.log = OrderedDict()

    @staticmethod
    def _get_current_test_case_name():
        for entry in inspect.stack():
            _, _, _, func_name, _, _ = entry
            if func_name.startswith("test"):  # is this heuristic good enough?
                return func_name
        return None

    def record_http_request(self, method, address, request, response, session):
        test_case = self._get_current_test_case_name()
        if test_case not in self.log:
            self.log[test_case] = Record()
        self.log[test_case].add_request(method, address, request, response, session)

    def record_assertion(self, assertion_name, target_response):
        if not self.log:
            raise ValueError("Can't record assertion, no test cases were registered yet")
        last_record = self.log[next(reversed(self.log))]
        if not last_record:
            raise ValueError("Can't record assertion, no test cases were registered yet")
        last_record.add_assertion(assertion_name, target_response)

    def record_assertion_failure(self, failure_message):  # do we need to pass some kind of context or assertion id?
        if not self.log:
            raise ValueError("Can't record assertion, no requests were made yet")
        last_record = self.log[next(reversed(self.log))]
        if not last_record:
            raise ValueError("Can't record assertion, no test cases were registered yet")
        last_assertion = last_record.assertions[-1]
        last_assertion.is_failed = True
        last_assertion.failure_message = failure_message

    @staticmethod
    def assertion_decorator(assertion_method):
        @wraps(assertion_method)
        def _impl(self, *method_args, **method_kwargs):
            recorder.record_assertion(getattr(assertion_method, '__name__', 'assertion'), self)
            try:
                return assertion_method(self, *method_args, **method_kwargs)
            except BaseException as exc:
                recorder.record_assertion_failure(str(exc))
                raise
        return _impl


recorder = _Recorder()


class HTTPTarget(object):
    def __init__(
            self,
            address,
            base_path=None,
            use_cookies=True,
            default_headers=None,
            keep_alive=True,
            auto_assert_ok=True
    ):
        self.address = address
        # config flags
        self._base_path = base_path
        self._use_cookies = use_cookies
        self._keep_alive = keep_alive
        self._default_headers = default_headers
        self._auto_assert_ok = auto_assert_ok
        # internal vars
        self.__session = None

    def use_cookies(self, use=True):
        self._use_cookies = use
        return self

    def base_path(self, base_path):
        self._base_path = base_path
        return self

    def keep_alive(self, keep=True):
        self._keep_alive = keep
        return self

    def default_headers(self, headers):
        self._default_headers = headers  # NOTE: copy or even update?
        return self

    def auto_assert_ok(self, value=True):
        self._auto_assert_ok = value

    def _bake_address(self, path):
        addr = self.address
        if self._base_path is not None:
            addr += self._base_path
        addr += path
        return addr

    def request(self, method, path,
                params=None, headers=None, cookies=None, data=None, json=None, allow_redirects=False, timeout=30):
        """
        Prepares and sends an HTTP request. Returns the response.

        :param method: str
        :param path: str
        :return: response
        :rtype: HTTPResponse
        """
        if self._keep_alive and self.__session is None:
            self.__session = requests.Session()

        if self.__session is not None and not self._use_cookies:
            self.__session.cookies.clear()

        address = self._bake_address(path)
        response = http.request(method, address, session=self.__session,
                                params=params, headers=headers, cookies=cookies, data=data, json=json,
                                allow_redirects=allow_redirects, timeout=timeout)
        if self._auto_assert_ok:
            response.assert_ok()
        return response

    def get(self, path, **kwargs):
        # TODO: how to reuse requests.session? - pass it as additional parameter for http.request ?
        return self.request("GET", path, **kwargs)

    def post(self, path, **kwargs):
        return self.request("POST", path, **kwargs)

    def put(self, path, **kwargs):
        return self.request("PUT", path, **kwargs)

    def delete(self, path, **kwargs):
        return self.request("DELETE", path, **kwargs)

    def patch(self, path, **kwargs):
        return self.request("PATCH", path, **kwargs)

    def head(self, path, **kwargs):
        return self.request("HEAD", path, **kwargs)


# HTTP Response:
class HTTPResponse(object):
    # properties:
    # - url
    # - status code
    # - status message
    # - text
    # - content
    # - headers
    # - cookies
    # - request

    def __init__(self, py_response):
        """

        :param py_response: requests.Response
        :type py_response: requests.Response
        """
        self.py_response = py_response
        # TODO: unpack all py_response fields into local properties

    @classmethod
    def from_py_response(cls, py_response):
        "Construct HTTPResponse from requests.Response object"
        return cls(py_response)

    def __eq__(self, other):
        return self.py_response == other.py_response

    # TODO: text, content - @property?

    @recorder.assertion_decorator
    def assert_ok(self, msg=None):
        if self.py_response.status_code >= 400:
            msg = msg or "Request to %s didn't succeed" % self.py_response.url
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_failed(self, msg=None):
        if self.py_response.status_code < 400:
            msg = msg or "Request to %s didn't fail" % self.py_response.url
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_2xx(self, msg=None):
        if not 200 <= self.py_response.status_code < 300:
            msg = msg or "Response code isn't 2xx, it's %s" % self.py_response.status_code
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_3xx(self, msg=None):
        if not 300 <= self.py_response.status_code < 400:
            msg = msg or "Response code isn't 3xx, it's %s" % self.py_response.status_code
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_4xx(self, msg=None):
        if not 400 <= self.py_response.status_code < 500:
            msg = msg or "Response code isn't 4xx, it's %s" % self.py_response.status_code
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_5xx(self, msg=None):
        if not 500 <= self.py_response.status_code < 600:
            msg = msg or "Response code isn't 5xx, it's %s" % self.py_response.status_code
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_status_code(self, code, msg=None):
        actual = str(self.py_response.status_code)
        expected = str(code)
        if actual != expected:
            msg = msg or "Actual status code (%s) didn't match expected (%s)" % (actual, expected)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_not_status_code(self, code, msg=None):
        actual = str(self.py_response.status_code)
        expected = str(code)
        if actual == expected:
            msg = msg or "Actual status code (%s) unexpectedly matched" % actual
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_in_body(self, member, msg=None):
        if member not in self.py_response.text:
            msg = msg or "%r wasn't found in response body" % member
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_not_in_body(self, member, msg=None):
        if member in self.py_response.text:
            msg = msg or "%r was found in response body" % member
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_regex_in_body(self, regex, match=False, msg=None):
        assert_regexp(regex, self.py_response.text, match=match, msg=msg)

    @recorder.assertion_decorator
    def assert_regex_not_in_body(self, regex, match=False, msg=None):
        assert_not_regexp(regex, self.py_response.text, match=match, msg=msg)

    # TODO: assert_content_type?

    @recorder.assertion_decorator
    def assert_has_header(self, header, msg=None):
        if header not in self.py_response.headers:
            msg = msg or "Header %s wasn't found in response headers: %r" % (header, self.py_response.headers)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_header_value(self, header, value, msg=None):
        self.assert_has_header(header)
        actual = self.py_response.headers[header]
        if actual != value:
            msg = msg or "Actual header value (%r) isn't equal to expected (%r)" % (actual, value)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_in_headers(self, member, msg=None):
        headers_text = headers_as_text(self.py_response.headers)
        if member not in headers_text:
            msg = msg or "Header %s wasn't found in response headers text: %r" % (member, headers_text)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_not_in_headers(self, member, msg=None):
        if member in headers_as_text(self.py_response.headers):
            msg = msg or "Header %s was found in response headers text" % member
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_regex_in_headers(self, member, msg=None):
        assert_regexp(member, headers_as_text(self.py_response.headers), msg=msg)

    @recorder.assertion_decorator
    def assert_regex_not_in_headers(self, member, msg=None):
        assert_not_regexp(member, headers_as_text(self.py_response.headers), msg=msg)

    @recorder.assertion_decorator
    def assert_jsonpath(self, jsonpath_query, expected_value=None, msg=None):
        jsonpath_expr = jsonpath_rw.parse(jsonpath_query)
        body = self.py_response.json()
        matches = jsonpath_expr.find(body)
        if not matches:
            msg = msg or "JSONPath query %r didn't match response content: %s" % (jsonpath_query, body)
            raise AssertionError(msg)
        actual_value = matches[0].value
        if expected_value is not None and actual_value != expected_value:
            tmpl = "Actual value at JSONPath query (%r) isn't equal to expected (%r)"
            msg = msg or tmpl % (actual_value, expected_value)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_not_jsonpath(self, jsonpath_query, expected_value=None, msg=None):
        jsonpath_expr = jsonpath_rw.parse(jsonpath_query)
        body = self.py_response.json()
        matches = jsonpath_expr.find(body)
        if matches:
            msg = msg or "JSONPath query %r did match response content: %s" % (jsonpath_query, body)
            raise AssertionError(msg)
        actual_value = matches[0].value
        if expected_value is not None and actual_value == expected_value:
            tmpl = "Actual value at JSONPath query (%r) is equal to expected (%r)"
            msg = msg or tmpl % (actual_value, expected_value)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_xpath(self, xpath_query, parser_type='html', validate=False, msg=None):
        parser = etree.HTMLParser() if parser_type == 'html' else etree.XMLParser(dtd_validation=validate)
        tree = etree.parse(BytesIO(self.py_response.content), parser)
        matches = tree.xpath(xpath_query)
        if not matches:
            msg = msg or "XPath query %r didn't match response content: %s" % (xpath_query, self.py_response.text)
            raise AssertionError(msg)

    @recorder.assertion_decorator
    def assert_not_xpath(self, xpath_query, parser_type='html', validate=False, msg=None):
        parser = etree.HTMLParser() if parser_type == 'html' else etree.XMLParser(dtd_validation=validate)
        tree = etree.parse(BytesIO(self.py_response.content), parser)
        matches = tree.xpath(xpath_query)
        if matches:
            msg = msg or "XPath query %r did match response content: %s" % (xpath_query, self.py_response.text)
            raise AssertionError(msg)

    def extract_regex(self, regex, default=None):
        extracted_value = default
        for item in re.finditer(regex, self.py_response.text):
            extracted_value = item
            break
        return extracted_value

    def extract_jsonpath(self, jsonpath_query, default=None):
        jsonpath_expr = jsonpath_rw.parse(jsonpath_query)
        body = self.py_response.json()
        matches = jsonpath_expr.find(body)
        if not matches:
            return default
        return matches[0].value

    def extract_xpath(self, xpath_query, default=None, parser_type='html', validate=False):
        parser = etree.HTMLParser() if parser_type == 'html' else etree.XMLParser(dtd_validation=validate)
        tree = etree.parse(BytesIO(self.py_response.content), parser)
        matches = tree.xpath(xpath_query)
        if not matches:
            return default
        match = matches[0]
        return match.text