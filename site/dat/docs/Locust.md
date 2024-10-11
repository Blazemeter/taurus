# Locust.io Executor
[Locust.io](https://locust.io/) is a Python-based load generating tool where you have full freedom of programming test scenarios in the Python language. It uses a resource-efficient coroutine approach. The Locust package is not installed automatically by Taurus, please install it manually using `pip install locust`.

Note that for Locust, the `iterations` option means the quantity of requests, not the number of cycles of the scenario (as the last can contain more than one request). The following load profile settings have no effect for this executor: `throughput` and `steps` 

Tip: Define the `hold-for` value explicitly to allow long-running tests to terminate gracefully. 

Taurus appends `PYTHONPATH` with path to artifacts directory and current working directory. Make sure you have no module name clashes (for example, don't name your locustfile as `locust.py`).

Here's example config that uses existing locust file:

```yaml
execution:
- executor: locust
  concurrency: 10
  ramp-up: 1m
  hold-for: 2m
  iterations: 1000
  scenario: example

scenarios:
  example:
    default-address: http://blazedemo.com
    script: sample.py
```

Example locust file `sample.py`:
```python
from locust import HttpUser, TaskSet, task, between

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

class WebsiteUser(HttpUser):
    tasks = [WebsiteTasks]
    wait_time = between(0.100, 1.500)
```

Latest supported version is 2+.

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
      method: get
      headers:
        var1: val1
      body: 'body content'
      assert:
      - contains:
        - body  # list of search patterns
        - content
        subject: body # subject for check
        regexp: false  # treat string as regular expression, true by default
        not: false  # inverse assertion condition
```

Keep in mind that Locust requires default url for its work (empty string is accepted). You have to set `host`
in python script or `default-address` in script for Taurus. If both are found value from Taurus script has priority.
 
## Run Locust in Distributed Mode
Distributed mode for Locust is enabled with two option `master` and `workers` under execution settings:

```yaml
execution:
- executor: locust
  master: True
  workers: 10

scenarios:
  request_example:
...
```
Keep in mind that Taurus starts locust master node only. All other workers should be configured and started manually.  
