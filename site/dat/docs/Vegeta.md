# Vegeta Executor

`Vegeta` executor allows you to run [Vegeta](https://github.com/tsenart/vegeta)-based test suites.

In order to launch Vegeta executor, you can use yaml config like in the example below.

Example 1 - use existing Vegeta script:
```yaml
execution:
- executor: vegeta
  concurrency: 100  # number of desired requests rate
  hold-for: 60s  # execution duration
  scenario:
    script: vegeta.txt  # has to be a valid Vegeta script
```

Example 2 - describe the requests in a scenario:
```yaml
execution:
- executor: vegeta
  concurrency: 100
  hold-for: 60s
  scenario: vegeta-test

scenarios:
  vegeta-test:
    requests:
      - url: http://localhost:8000
        method: HEAD
        headers:
          X-Account-ID: 8675309
      - url: http://localhost:8000/abc
        method: POST
        headers:
          Confirmation-Token: 90215
          Authorization: Token DEADBEEF
        body:
          id: 123
          name: Some name
```
