# WebdriverIO Executor

`wdio` executor allows you to run [WebdriverIO](http://webdriver.io/)-based test suites.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Usage:
```yaml
execution:
- executor: wdio
  iterations: 2
  scenario:
    script: wdio.conf.js  # wdio configuration script
```

You can find an example of complete WebdriverIO/Mocha based test suite and a Taurus config to run it with
in [examples/selenium/wdio](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/wdio)
folder of Taurus's repo.

## Settings

By default, Taurus will install wdio and its dependencies with npm into `~/.bzt/selenium-taurus/wdio/` directory.
You can customize it with `tools-dir` module setting.

```yaml
modules:
  wdio:
    tools-dir: my-dir
```