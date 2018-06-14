# Robot Executor

`robot` executor allows to run [Robot Framework](http://robotframework.org/)-based test suites with Taurus.

Taurus will execute tests in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded. The default number of iterations is 1.

Usage example:
```yaml
execution:
- executor: robot
  scenario:
    script: tests/
```

Value of `script` is passed to Robot directly, so it can be either one Robot file or a directory with a
lot of them.

## Variables

You can pass additional variables to Robot by specifying `variables` dict inside test scenario.

```yaml
execution:
- executor: robot
  scenario:
    variables:
      BROWSER: Firefox
    script: tests/
```

In this case Taurus will create and intermediate YAML file and pass it to Robot as `variablefile`.

If you have your own Robot-compatible variables file (be it Python or YAML), you can specify it directly
as `variables` value:

```yaml
execution:
- executor: robot
  scenario:
    variables: vars.yaml
    script: tests/
```

## Tags

You can pass tags you want to include by specifying `tags` in the scenario:
```yaml
execution:
- executor: robot
  scenario:
    tags: create,database  # comma-separated list of tags
    variables:
      BROWSER: Firefox
    script: tests/
```

## Configuration Options

The `interpreter` option allows providing custom interpreter for your tests
(for example when you're running Taurus with Python 2 but want to run pytest with Python 3).

```yaml
modules:
  robot:
    interpreter: /usr/bin/python3
```

## Examples

You can find an example of complete Robot/Selenium-based test suite and a Taurus config to run it with
in [examples/selenium/robot](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/robot)
directory of Taurus's repo.

