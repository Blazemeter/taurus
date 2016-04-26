# Tsung Executor

## About

[Tsung](http://tsung.erlang-projects.org/) is an open-source multi-protocol distributed load testing tool.
The purpose of Tsung is to simulate users in order to test the scalability and performance of IP based client/server
applications. You can use it to do load and stress testing of your servers.

Taurus supports only Tsung version 1.5.1 and higher.

## Tsung Installation

To install Tsung, see official [Tsung Installation](http://tsung.erlang-projects.org/user_manual/installation.html) docs.

### Notes for Ubuntu 14.04

Ubuntu 14.04 repos provide 1.4.1 version of Tsung, which is not supported by Taurus, as it lacks some important features.
You can install more recent 1.5.1 version from [tsung-stable](https://launchpad.net/~tsung/+archive/ubuntu/stable) PPA.

More recent Ubuntu versions provide Tsung 1.5.1 by default, so `apt-get install tsung` is enough to get Tsung.

## Tsung load generation model

When given fixed `concurrency`, Tsung spawns N "users" each second. Every user is actually a lightweight Erlang process.
User executes all requests from given scenario and then stops.

## Scenario Samples

Example of using constant `concurrency` and `hold-for`:
```yaml
---
execution:
- executor: tsung
  concurrency: 100
  hold-for: 1m
  scenario:
    default-address: http://blazedemo.com
    requests:
      - /
      - /reserve.php
```

Example of using user's Tsung config:
```yaml
---
execution:
- executor: tsung
  scenario:
    script: tsung/http_simple.xml
```

If you specify both `scenario.script` and load profile (`concurrency` and `hold-for`) — Taurus will copy your Tsung
configuration and overwrite `<load>` section. The rest of your Tsung config will be preserved.

Example:
```yaml
---
execution:
- executor: tsung
  concurrency: 100
  hold-for: 5m
  scenario:
    script: tsung/http_simple.xml
```

Note that Tsung doesn't support `throughput` and `ramp-up` options.

Here's the example of various HTTP requests features that Taurus supports:

```yaml
---
execution:
- executor: tsung
  concurrency: 50
  hold-for: 5m
  scenario:
    default-address: http://blazedemo.com  # base address for HTTP requests

    think-time: 1s  # delay to make after executing this request, applies to all requests, default value is 0

    timeout: 3s  # TCP connection timeout, applies to all requests, defaults value is infinity
    max-retries: 1  # max TCP reconnect retries number, defaults to 1

    headers:  # global headers, applies to all requests
      User-Agent: taurus-tsung

    requests:
    - /  # shorthard form

    - url: /reserve.php  # full form with specified method
      method: GET

    - url: /
      think-time: 3s  # overrides scenario-level think-time option

    - url: /submit.php  # POST request with body
      method: POST
      body: 'request-body-string'

    - url: /submit.php  # PUT request with file body
      method: PUT
      body-file: path/to/file.bin
      
    - url: /  # request with additional headers
      headers:
        Authentication: Token 142857
        Referer: http://gettaurus.org/docs
```

## Module Settings

If you have installed Tsung in non-standard location (i.e. `tsung` is not in your `$PATH`), you can use `path` option
to point Taurus to the `tsung` executable.

```yaml
---
modules:
  tsung:
    path: /usr/local/bin/tsung
```
