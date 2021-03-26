# Vegeta Executor

_This is available only in [unstable snapshot](https://gettaurus.org/install/Installation/#Latest-Unstable-Snapshot)._

[Vegeta](https://github.com/tsenart/vegeta) is an HTTP load generator tool.
Written in Go programming language Vegeta consumes CPU and memory resources very efficiently.
This makes Vegeta a good candidate for performance testing organized in the cloud where cost effectiveness is especially important.

Another strong benefit of Vegeta is that it is safe from the "Coordinated omission" problem.

Vegeta does not have so called "virtual users", but it is very handy because it supports setting the desired number of requests per second (i.e. throughput).

In Taurus, `Vegeta` executor allows to run the load for given duration with specified request rate (throughput) using either existing Vegeta scripts or YAML representation of the HTTP requests.

In order to launch Vegeta executor, you can use yaml config like in the examples below.

Example 1 - use existing Vegeta script:
```yaml
execution:
- executor: vegeta
  throughput: 100  # number of desired requests rate
  hold-for: 60s    # execution duration
  scenario:
    script: vegeta.txt  # has to be a valid Vegeta script
```

Example 2 - describe the requests in a scenario:
```yaml
execution:
- executor: vegeta
  throughput: 100
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
          Authorization: Token ABCDEF
        body:
          id: 123
          name: Some name
```
