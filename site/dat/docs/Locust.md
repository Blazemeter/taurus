# Locust.io Executor
[Locust.io](http://locust.io/) is python-based load generating tool where you have full freedom of programming test scenario in Python language. It uses resource-efficient coroutine approach.

Locust package is not installed automatically by Taurus, please install it manually: `pip install locustio`

Make note that for Locust `iterations` option means quantity of requests, not cycles of scenario (as the last can contains more than one request). Next load profile settings has no effect for this executor: `throughput` and `steps` 

Taurus appends `PYTHONPATH` with path to artifacts directory and current working directory. Make sure you have no module name clashes (for example, don't name your locustfile as `locust.py`).

Here's example config that uses existing locust file:

```yaml
execution:
- executor: locust
  concurrency: 10
  ramp-up: 1m
  iterations: 1000
  scenario: example

scenarios:
  example:
    default-address: http://blazedemo.com
    script: sample.py
```

Example locust file `sample.py`:
```python
from locust import HttpLocust, TaskSet, task

class WebsiteTasks(TaskSet):
    def on_start(self):
        self.client.post("/login", {
            "username": "test_user",
            "password": ""
        })

    @task
    def index(self):
        self.client.get("/")

    @task
    def about(self):
        self.client.get("/about/")

class WebsiteUser(HttpLocust):
    task_set = WebsiteTasks
    min_wait = 100
    max_wait = 1500
```

## Requests Scenario

LocustIO executor partially supports building scenario from requests. Supported features:

 - request methods GET/POST
 - headers and body for requests
 - set timeout/think-time on both scenario/request levels
 - assertions (for body and http-code)
 
```yaml
scenarios:
  request_example:
    timeout: 10  #  global scenario timeout for connecting, receiving results, 30 seconds by default
    think-time: 1s500ms  # global scenario delay between each request
    default-address: http://blazedemo.com  # specify a base address, so you can use short urls in requests
    keepalive: true  # flag to use HTTP keep-alive for connections, default is true
    requests:
    - url: /  
      method: post
      headers:
        var1: val1
      body: 'body content'
      assert:
      - contains:
        - blazemeter  # list of search patterns
        - Trusted
        subject: body # subject for check
        regexp: false  # treat string as regular expression, true by default
        not: false  # inverse assertion condition
```

Keep in mind: locust requires default url for its work (empty string is accepted). You have to set `host`
in python script or `default-address` in script for Taurus. If both are found value from Taurus script has priority.
 
