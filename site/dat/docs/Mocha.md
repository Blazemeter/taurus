# Mocha Executor
Allows to run javascript tests based on Mocha.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Usage:
```yaml
execution:
- executor: mocha
  scenario:
    script: tests/  # folder with your tests or path to one test script
```

## Supported file types:

Test scenario may be presented as single javascript file or as a folder.

You can also find an example of complete Mocha-based test suite and Taurus config to run it with
in [examples/selenium/mocha](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/mocha)
folder of Taurus's repo.

