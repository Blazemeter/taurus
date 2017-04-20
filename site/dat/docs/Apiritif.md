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

TODO: other methods and all parameters description.

## HTTP Targets

Target is an object that captures resource name of the URL (protocol, domain, port)
and allows to set some settings applied to all requests made for a target.


```python
from apiritif import http

qa_env = http.target("http://192.160.0.2")
qa_env.get("/api/v4/user")
qa_env.get("/api/v4/user")
```

TODO: options, auto_assert_ok

## Assertions

Apiritif responses provide a lot of useful assertions that can be used on responses.

TODO: assertion list

## Transactions

TODO: transactions

## Taurus Integration

TODO: describe that Taurus can extract Apiritif's action log and handle it.

## Logging

TODO: Describe that Apiritif creates 'apiritif' logger that can be used to
debug http requests and write test interactively.

TODO: describe how to silence Apiritif logging.


