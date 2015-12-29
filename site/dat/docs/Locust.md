# Locust.io Executor
[Locust.io](http://locust.io/) is python-based load generating tool where you have full freedom of programming test scenario in Python language. It uses resource-efficient coroutine approach.

Locust package is not installed automatically by Taurus, please install it manually: `pip install locustio`

Make note that not all load profile settings are supported by Locust module. Only `concurrency` and `ramp-up` will have effect. Also, you should set `iterations` option to limit execution time, otherwise locust will run until manually interrupted.

Make note that Taurus appends `PYTHONPATH` with path to artifacts directory and current working directory. Make sure you have no module name clashes (for example, don't name your locustfile as `locust.py`).

Here's example config that uses existing locust file:

```yaml
---
execution:
- executor: locust
  concurrency: 10
  ramp-up: 1m
  iterations: 1000
  scenario:
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