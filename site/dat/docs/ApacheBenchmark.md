# Apache Benchmark Executor

## About
[Apache Benchmark](https://httpd.apache.org/docs/2.4/programs/ab.html) is a tool for benchmarking your HTTP server.
Taurus supports the following features of Apache Benchmark:

 - `iterations`: number of requests to make
 - `concurrency`: number of multiple requests to make at a time (defaults to 1).
 - `hold-for`: run load testing until specified timeout
 - `headers`: headers to attach to HTTP request
 - `keepalive`: use HTTP KeepAlive feature

Keep in mind the following rules when using `executor: apachebench`:
 - You cannot specify more than one request in `requests` section.
 - Apache Benchmark supports only GET requests.
 - `timeout` option is not supported.
 - You should have Apache Benchmark installed.

## Scenario Samples

Simplest working example:
```yaml
---
execution:
- executor: apachebench
  scenario:
    requests:
    - http://blazedemo.com/
```

Example of `hold-for` usage:
```yaml
---
execution:
- executor: apachebench
  hold-for: 30s
  scenario:
    requests:
    - http://blazedemo.com/
```

Complex example:
```yaml
---
execution:
- executor: apachebench
  concurrency: 20
  iterations: 1000
  headers:
    - Content-Type: text/plain
  scenario:
    keepalive: false
    requests:
      - url: http://blazedemo.com/
        headers:
          - X-Answer: 42
```


## Module Settings

If you have Apache Benchmark in non-standard location, use `path` option to point Taurus to the `ab`:

```yaml
---
modules:
  apachebench:
    path: /home/john/build/apache2/bin/ab
```