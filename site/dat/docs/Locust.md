# Locust.io Executor
[Locust.io](http://locust.io/) is python-based load generating tool where you have full freedom of programming test scenario in Python language. It uses resource-efficient coroutine approach.

Locust package is not installed automatically by Taurus, please install it manually: `pip install locustio`

Make note that for Locust `iterations` option means quantity of requests, not cycles of scenario (as the last can contains more than one request). Following load profile settings has no effect for this executor: `throughput` and `steps` 

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

For files generation we use syntax of Locust 1.0+. Latest supported version is 1.0.3.

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
Keep in mind: taurus starts locust master node only. All other workers should be configured and started manually.  
 
## Migration notes

Since you can read this migration notes, all latest dev builds and releases after 1.14.2 of taurus support 1.0+ 
locust version with new syntax. Here are some tips for updating to the latest locust version from version 0.13.*.

#### In .yaml files `slaves` field is renamed to `workers` according to locust

##### old version
```yaml
execution:
- executor: locust
  slaves: 10
...
```
##### new version
```yaml
execution:
- executor: locust
  workers: 10
...
```

#### Locust file migrations
- `Locusts` is renamed to `Users`
- Use property `tasks` instead of `task_set` 

##### old version
```python 
from locust import HttpLocust, TaskSet, ...

class WebsiteTasks(TaskSet):
...

class WebsiteUser(HttpLocust):
    task_set = WebsiteTasks
...
```

##### new version
```python 
from locust import HttpUser, TaskSet, ...

class WebsiteTasks(TaskSet):
...

class WebsiteUser(HttpUser):
    tasks = [WebsiteTasks]
...
```