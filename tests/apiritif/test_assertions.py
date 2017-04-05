import unittest

from apiritif import http


class TestRequests(unittest.TestCase):
    def test_assert_regex(self):
        response = http.get('http://blazedemo.com/')
        response.assert_ok()
        response.assert_status_code(200)
        response.assert_regex_in_body('Welcome to the Simple Travel Agency!')

    def test_assert_xpath(self):
        response = http.get('http://blazedemo.com/')
        response.assert_ok()
        response.assert_xpath('//head/title', parser_type='html', validate=False)
        response.assert_not_xpath('//yo/my/man', parser_type='html', validate=False)

    def test_assert_jsonpath(self):
        response = http.get('https://jsonplaceholder.typicode.com/users')
        response.assert_ok()
        response.assert_jsonpath('$.[0].username', expected_value='Bret')
        response.assert_not_jsonpath("$.foo.bar")

    def test_assert_ok(self):
        response = http.get('http://blazedemo.com/')
        response.assert_ok()

    def test_assert_failed(self):
        response = http.get('http://blazedemo.com/not-found')
        response.assert_failed()

    def test_assert_2xx(self):
        response = http.get('http://blazedemo.com/')
        response.assert_2xx()

    def test_assert_3xx(self):
        response = http.get('https://httpbin.org/status/301', allow_redirects=False)
        response.assert_3xx()

    def test_assert_4xx(self):
        response = http.get('http://blazedemo.com/not-found')
        response.assert_4xx()

    def test_assert_5xx(self):
        response = http.get('https://httpbin.org/status/500')
        response.assert_5xx()

    def test_assert_status_code(self):
        response = http.get('http://blazedemo.com/')
        response.assert_status_code(200)

    def test_assert_not_status_code(self):
        response = http.get('http://blazedemo.com/not-found')
        response.assert_not_status_code(200)

    def test_assert_in_body(self):
        response = http.get('http://blazedemo.com/')
        response.assert_in_body("Welcome")

    def test_assert_not_in_body(self):
        response = http.get('http://blazedemo.com/')
        response.assert_not_in_body("Willcommen!")

    def test_assert_regex_in_body(self):
        response = http.get('http://blazedemo.com/')
        response.assert_regex_in_body("Welcome to the Simple .+ Agency")

    def test_assert_regex_not_in_body(self):
        response = http.get('http://blazedemo.com/not-found')
        response.assert_regex_not_in_body("Nope")

    def test_assert_has_header(self):
        response = http.get('http://blazedemo.com/')
        response.assert_has_header("Content-Type")

    def test_assert_header_value(self):
        response = http.get('http://blazedemo.com/not-found')
        response.assert_header_value("Content-Type", "text/html; charset=UTF-8")

    def test_assert_in_headers(self):
        response = http.get('http://blazedemo.com/')
        response.assert_in_headers("Content-Type: text/html")

    def test_assert_not_in_headers(self):
        response = http.get('http://blazedemo.com/')
        response.assert_not_in_headers("Content-Type: application/html")

    def test_assert_regex_in_headers(self):
        response = http.get('http://blazedemo.com/')
        response.assert_regex_in_headers(r"Content-Type: .+")

    def test_assert_regex_not_in_headers(self):
        response = http.get('http://blazedemo.com/')
        response.assert_regex_not_in_headers(r"Content-Type: application/.+")
