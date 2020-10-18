# Apiritif

Apiritif is a number of utilities aimed to simplify the process of maintaining API tests. 
Apiritif tests fully based on python nose tests. This library can help you to develop and run your existing tests.
In order to create any valid tests for Apiritif you can read [nose test documentation](https://nose.readthedocs.io/en/latest/testing.html).

Here described some features of Apiritif which can help you to create tests more easily.  

## HTTP

#### HTTP Requests

Apiritif allows to use simple `requests`-like API for making HTTP requests.

```python
from apiritif import http

response = http.get("http://example.com")
response.assert_ok()  # will raise AssertionError if request wasn't successful
```

`http` object provides the following methods: `get`, `post`, `put`, `patch`, `delete`, `head`.

#### HTTP Targets

Target is an object that captures resource name of the URL (protocol, domain, port)
and allows to set some settings applied to all requests made for a target.

```python
from apiritif import http

qa_env = http.target("http://192.160.0.2")
qa_env.get("/api/v4/user")
qa_env.get("/api/v4/user")
```

#### Assertions

Apiritif responses provide a lot of useful assertions that can be used on responses.

Here's some of assertions that can be used:
```python
response = http.get("http://example.com/")

# assert that request succeeded (status code is 2xx or 3xx)
response.assert_ok()
# assert that request has failed
response.assert_failed()
```

If you want to see more info about requests, targets and assertions check [Apiritif documentation](https://github.com/Blazemeter/apiritif)


## Transactions

Apiritif allows to group multiple requests or actions into a transaction using a `transaction` context manager.
For example when we have test action like bellow we want to execute this test as one solid piece.

```python
def test_with_login():
    user_credentials = database.get_my_user();
    http.get("https://blazedemo.com/user/login?id="+user_credentials.id)
    http.get("https://blazedemo.com/user/id/personalPage")
    http.get("https://blazedemo.com/user/id/getPersonalData")
```

Here where we can use transaction in order to wrap login process in one block.

```python
def test_with_login():
    with apiritif.transaction('Login'):
        user_credentials = database.get_my_user();
        http.get("https://blazedemo.com/users/login?id="+user_credentials.id).assert_ok()
        http.get("https://blazedemo.com/users/id/personalPage").assert_ok()
        http.get("https://blazedemo.com/users/id/getPersonalData").assert_ok()
```

Transaction defines the name for the test. This will be displayed on the BlazeMeter report.

#### Smart transactions

`smart_transaction` is advanced option for test flow control (stop or continue after failed test method).
Let see another test method example:

```python
class Tests(TestCase):
    def test_available_pages():
        http.get("https://blazedemo.com/").assert_ok()
        http.get("https://blazedemo.com/users").assert_ok()
    
        http.get("https://blazedemo.com/users/search").assert_ok()
        http.get("https://blazedemo.com/users/count").assert_ok()
        http.get("https://blazedemo.com/users/login").assert_ok()
```
In this case we have multiple requests divided into blocks. I do not want to test pages under `users` space if it is not available. For this purpose we can use `smart_transaction`.

```python
class Tests(TestCase):
    def setUp(self):
        apiritif.put_into_thread_store(func_mode=True)
    
    def test_available_pages():
        with apiritif.smart_transaction('Availability check'):
            http.get("https://blazedemo.com/").assert_ok()
            http.get("https://blazedemo.com/users").assert_ok()
    
        with apiritif.smart_transaction('Test users pages'):
            http.get("https://blazedemo.com/users/search").assert_ok()
            http.get("https://blazedemo.com/users/count").assert_ok()
            http.get("https://blazedemo.com/users/login").assert_ok()
```
Now this two blocks are wrapped into `smart_transaction` which would help with error test flow handling and logging.
Also each transaction defines test name and will be shown will be shown on the BlazeMeter report.

About `apiritif.put_into_thread_store(func_mode=True)`, this is test execution mode for apiritif.
We can execute all of the transactions in test no matter what or stop after first failed transaction.
This flag tells to apiritif "Stop execution if some transaction failed". `False` says "Run till the end in any case".

## CSV Reader
In order to use data from csv file as test parameters Apiritif provides two different csv readers.
Simple `CSVReader` helps you to read data from file line by line and use this data wherever you need:

```python
data_reader = apiritif.CSVReader('---path to required file---')

class Tests(TestCase):
    def test_user_page():
        data_reader.read_vars()
        vars = data_reader.get_vars()
        http.get("https://blazedemo.com/users/" + vars.user_id).assert_ok()
```

In case of multithreading testing you may need to deviate data between threads and ysu uniq lines for each thread.
`CSVReaderPerThread` helps to solve this problem: 

```python
data_per_thread_reader = apiritif.CSVReaderPerThread('---path to required file---')

class Tests(TestCase):
    def setUp(self):
        data_per_thread_reader.read_vars()
        self.vars = data_per_thread_reader.get_vars()
    
    def test_user_page():
        http.get("https://blazedemo.com/users/" + self.vars.user_id).assert_ok()
```