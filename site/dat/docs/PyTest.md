# Pytest Executor

`pytest` executor allows running [pytest](https://docs.pytest.org/en/latest/)-based test suites with Taurus.

Taurus will execute tests in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded. The default number of iterations is 1.

Usage:
```yaml
execution:
- executor: pytest
  iterations: 3  # perform 3 iterations over the entire test suite
  scenario:
    script: tests/
```

To run tests in parallel and distribute them across multiple CPUs, set `concurrency` either to positive integer or to 
'auto' value. Pass 'auto' to use as many processes as your computer has CPU cores.
Concurrency in the case with pytest executor is not the number of target concurrent virtual users, but
the number of parallel processes.

For now if `concurrency` is set in the test execution - all tests are grouped by module for test functions and by class 
for test methods. Groups are distributed to available workers as whole units.

Usage:
```yaml
execution:
- executor: pytest
  concurrency: 2  # all tests will be executed into 2 processes
  scenario:
    script: tests/
```


Possible `script` values for this executor are:
- single Python module (single .py file)
- Python package (folder with Python modules and packages)

## Additional Command-Line Options for Pytest

As `pytest` is regularly configured with command line options, Taurus allows you to pass additional
options to Pytest with `additional-args` scenario-level option.

Example:
```yaml
execution:
- executor: pytest
  scenario: pytest-run

scenarios:
  pytest-run:
    additional-args: --maxfail=1 --runslow
    script: tests/smoketests.py
```

## Configuration Options

The `interpreter` option allows providing custom interpreter for your tests. 
Also, you can specify PyTest version via `version` keyword:

```yaml
modules:
  pytest:
    interpreter: /usr/local/bin/python  # path to custom Python interpreter
    version: 6.2.1
    dry-install: false  # turn it on to avoid auto installation 
```
