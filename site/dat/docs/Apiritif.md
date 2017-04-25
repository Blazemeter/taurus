# Apiritif

Apiritif is a number of utilities aimed to simplify the process of maintaining API tests.

Feature list:
- utilities for making HTTP requests
- assertions on them
- transactions

## Overview


## HTTP Requests

Apiritif allows to use simple `requests`-like API for making HTTP requests.

```python
from apiritif import http

response = http.get("http://example.com")
response.assert_ok()  # will raise AssertionError if request wasn't successful
```

`http` object provides the following methods:
```python
from apiritif import http

http.get("http://api.example.com/posts")
http.post("http://api.example.com/posts")
http.put("http://api.example.com/posts/1")
http.patch("http://api.example.com/posts/1")
http.delete("http://api.example.com/posts/1")
http.head("http://api.example.com/posts")
```

All methods (`get`, `post`, `put`, `patch`, `delete`, `head`) support the following arguments:
```python
def get(address,               # URL for the request
        params=None,           # URL params dict
        headers=None,          # HTTP headers
        cookies=None,          # request cookies
        data=None,             # raw request data
        json=None,             # attach JSON object as request body
        allow_redirects=True,  # automatically follow HTTP redirects
        timeout=30)            # request timeout, by default it's 30 seconds
```

## HTTP Targets

Target is an object that captures resource name of the URL (protocol, domain, port)
and allows to set some settings applied to all requests made for a target.


```python
from apiritif import http

qa_env = http.target("http://192.160.0.2")
qa_env.get("/api/v4/user")
qa_env.get("/api/v4/user")
```

Target constructor supports the following options:
```python
target = apiritif.http.target(
    address,               # target base address
    base_path=None,        # base path prepended to all paths (e.g. '/api/v2')
    use_cookies=True,      # use cookies
    additional_headers=None,  # additional headers for all requests
    keep_alive=True,       # reuse opened HTTP connection
    auto_assert_ok=True,   # automatically invoke 'assert_ok' after each request
)
```


## Assertions

Apiritif responses provide a lot of useful assertions that can be used on responses.

Here's the list of assertions that can be used:
```python
response = http.get("http://example.com/")

# assert that request succeeded (status code is 2xx or 3xx)
response.assert_ok()
# assert that request has failed
response.assert_failed()

# status code based assertions
response.assert_2xx()
response.assert_3xx()
response.assert_4xx()
response.assert_5xx()
response.assert_status_code(code)
response.assert_not_status_code(code)

# content-based assertions

# assert that response body contains a string
response.assert_in_body(member)

# assert that response body doesn't contain a string
response.assert_not_in_body(member)

# search (or match) response body with a regex
response.assert_regex_in_body(regex, match=False)
response.assert_regex_not_in_body(regex, match=False)

# assert that response has header
response.assert_has_header(header)

# assert that response has header with given value
response.assert_header_value(header, value)

# assert that response's headers contains a string
response.assert_in_headers(member)
response.assert_not_in_headers(member)

# search (or match) response body with a regex
response.assert_regex_in_headers(member)
response.assert_regex_not_in_headers(member)

# assert that response body matches JSONPath query
response.assert_jsonpath(jsonpath_query, expected_value=None)
response.assert_not_jsonpath(jsonpath_query)

# assert that response body matches XPath query
response.assert_xpath(xpath_query, parser_type='html', validate=False)
response.assert_not_xpath(xpath_query, parser_type='html', validate=False)
```

Note that assertions can be chained, so the following construction is entirely valid:
```python

response = http.get("http://example.com/")
response.assert_ok().assert_in_body("Example")
```

## Transactions

TODO: transactions

## Taurus Integration

TODO: describe that Taurus can extract Apiritif's action log and handle it.

## Logging

TODO: Describe that Apiritif creates 'apiritif' logger that can be used to
debug http requests and write test interactively.

TODO: describe how to silence Apiritif logging.


