# Mocha Executor
Allows to run javascript tests based on Mocha.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Taurus currently uses Mocha v4 as default version.  
##Usage
```yaml
execution:
- executor: mocha
  scenario:
    script: tests/  # folder with your tests or path to one test script
```

Basic example of Mocha test with Selenium webdriver:
```javascript
const assert = require('assert'),
    webdriver = require('selenium-webdriver');
let driver;

describe('Blazedemo verification', function() {
    this.timeout(30000);

    before(async () => {
        driver = new webdriver.Builder().
            withCapabilities(webdriver.Capabilities.firefox()).
            build();
        await driver.get('http://blazedemo.com/');
    });

    after(function () {
        driver.quit();
    });
});

```
**Note**: When using Selenium Webdriver api, be aware than it uses async behavior with it's commands. For example, command `driver.get` will return a promise, which will be resolved only when webdriver fully loads page.  

## Supported file types:

Test scenario may be presented as single javascript file or as a folder.

You can also find an example of complete Mocha-based test suite and Taurus config to run it with
in [examples/selenium/mocha](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/mocha)
folder of Taurus's repo.

## Settings

By default, Taurus will install Mocha and its dependencies with npm into `~/.bzt/selenium-taurus/mocha/` directory.
You can customize it with `tools-dir` module setting.

```yaml
modules:
  mocha:
    tools-dir: my-dir
```