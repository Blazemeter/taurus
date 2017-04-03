import apiritif


class TestRequests(apiritif.APITestCase):
    def setUp(self):
        super(TestRequests, self).setUp()
        self.keep_alive = True

    def test_assert_regex(self):
        self.get('http://blazedemo.com/')
        self.assertOk()
        self.assertStatusCode(200)
        self.assertRegexInBody('Welcome to the Simple Travel Agency!')

    def test_assert_xpath(self):
        self.get('http://blazedemo.com/')
        self.assertOk()
        self.assertXPath('//head/title', parser_type='html', validate=False)
        self.assertNotXPath('//yo/my/man', parser_type='html', validate=False)

    def test_assert_jsonpath(self):
        self.get('https://jsonplaceholder.typicode.com/users',)
        self.assertOk()
        self.assertJSONPath('$.[0].username', expected_value='Bret')
        self.assertNotJSONPath("$.foo.bar")

    def test_assert_ok(self):
        self.get('http://blazedemo.com/')
        self.assertOk()

    def test_assert_failed(self):
        self.get('http://blazedemo.com/not-found')
        self.assertFailed()

    def test_assert_2xx(self):
        self.get('http://blazedemo.com/')
        self.assert2xx()

    def test_assert_3xx(self):
        self.get('https://httpbin.org/status/301', allow_redirects=False)
        self.assert3xx()

    def test_assert_4xx(self):
        self.get('http://blazedemo.com/not-found')
        self.assert4xx()

    def test_assert_5xx(self):
        self.get('https://httpbin.org/status/500')
        self.assert5xx()

    def test_assert_status_code(self):
        self.get('http://blazedemo.com/')
        self.assertStatusCode(200)

    def test_assert_not_status_code(self):
        self.get('http://blazedemo.com/not-found')
        self.assertNotStatusCode(200)

    def test_assert_in_body(self):
        self.get('http://blazedemo.com/')
        self.assertInBody("Welcome")

    def test_assert_not_in_body(self):
        self.get('http://blazedemo.com/')
        self.assertNotInBody("Willcommen!")

    def test_assert_regex_in_body(self):
        self.get('http://blazedemo.com/')
        self.assertRegexInBody("Welcome to the Simple .+ Agency")

    def test_assert_regex_not_in_body(self):
        self.get('http://blazedemo.com/not-found')
        self.assertRegexNotInBody("Nope")

    def test_assert_has_header(self):
        self.get('http://blazedemo.com/')
        self.assertHasHeader("Content-Type")

    def test_assert_header_value(self):
        self.get('http://blazedemo.com/not-found')
        self.assertHeaderValue("Content-Type", "text/html; charset=UTF-8")

    def test_assert_in_headers(self):
        self.get('http://blazedemo.com/')
        self.assertInHeaders("Content-Type: text/html")

    def test_assert_not_in_headers(self):
        self.get('http://blazedemo.com/')
        self.assertNotInHeaders("Content-Type: application/html")

    def test_assert_regex_in_headers(self):
        self.get('http://blazedemo.com/')
        self.assertRegexInHeaders(r"Content-Type: .+")

    def test_assert_regex_not_in_headers(self):
        self.get('http://blazedemo.com/')
        self.assertRegexNotInHeaders(r"Content-Type: application/.+")
