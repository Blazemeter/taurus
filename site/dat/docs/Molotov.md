# Molotov Executor

[Molotov](https://molotov.readthedocs.io/en/stable/) is a load testing tool developed by Mozilla.
It's based on Python 3 and asynchronous I/O subsystem.

Important note: molotov requires python >= 3.5 and doesn't work on Windows systems at the moment.

Usage:
```yaml
execution:
- executor: molotov
  concurrency: 10  # number of Molotov workers
  iterations: 5  # iteration limit for the test
  ramp-up: 30s
  hold-for: 1m
  scenario:
    script: loadtest.py  # has to be valid Molotov's script
```

## Process number

If your tests requires a number of workers that is impossible to create from one process — you
can use additional `processes` execution option to specify the number of processes for Molotov.

```yaml
execution:
- executor: molotov
  concurrency: 100
  processes: 10
  hold-for: 5m
  scenario:
    script: loadtest.py
```

## Think-time

If you need a delay between each worker run, use `think-time` option in scenario level. Note that you would have to
move your script there as well.

```yaml
execution:
- executor: molotov
  concurrency: 100
  processes: 10
  hold-for: 5m
  scenario: simple

scenarios:
  simple:
    think-time: 5
    script: molotov_example.py
```

## Module Settings

If you have installed Molotov in a non-standard location, you can use `path` option to point Taurus to the `molotov` executor:

```yaml
modules:
  molotov:
    path: /home/john/venv/bin/molotov
```
