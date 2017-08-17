# PyTest Executor

`pytest` executor allows to run [pytest](https://docs.pytest.org/en/latest/)-based test suites with Taurus.

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

Possible `script` values for this executor are:
- single Python module (single .py file)
- Python package (folder with Python modules and packages)

##  Configuration options

The `interpreter` option allows providing custom interpreter for your tests
(for example when you're running Taurus with Python 2 but want to run pytest with Python 3).

```yaml
modules:
  pytest:
    interpreter: /usr/local/bin/python  # path to custom Python interpreter
```
