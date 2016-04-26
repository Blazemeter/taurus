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

## Scenario Examples

Example of using constant `throughput` and `hold-for`:
```yaml
---
execution:
- executor: tsung
  throughput: 100
  hold-for: 1m
  scenario:
    requests:
    - http://blazedemo.com/
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
