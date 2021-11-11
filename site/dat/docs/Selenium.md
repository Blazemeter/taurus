# Selenium Executor
Selenium is virtual executor provided you ability to run functional tests locally with Selenium WebDriver by choosing appropriate executor. Currently, supported executors are:
- [JUnit](JUnit.md) (Java)
- [TestNG](TestNG.md) (Java)
- [Apiritif](Apiritif.md) (Python)
- [PyTest](PyTest.md) (Python)
- [RSpec](RSpec.md) (Ruby)
- [Mocha](Mocha.md) (JavaScript)
- [NUnit](NUnit.md) (C#)
- [xUnit](XUnit.md) (C#)

Selenium Grid isn't supported for tests described with Taurus language, but if you have your own test suite that uses
Selenium Grid to manage browser instances - Taurus will run these tests just fine.

Selenium executor uses multiple test runners (JUnit, TestNG, Mocha, etc.), test type is detected automatically.
If automatic detection fails - you can use `runner` option described below.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

## Supported file types:

Test scenario may be presented not only as single file but as a folder (or, in case of Java-based tests, as a jar).

  - .java/single file
  - .java/folder (flat structure, no package)
  - .java/folders (package)
  - .jar/single file
  - .jar/folder
  - .py/single file
  - .py/folder
  - .rb/single file
  - .rb/folder
  - .js/single file
  - .js/folder
  - .dll/single file

## Specifying Test Runner Explicitly

By default, Taurus tries to automatically detect the language your tests are written in. If, for some reason,
the autodetection fails - you can specify test runner explicitly with `runner` execution-level option.

Supported values:
- `junit`: [JUnit](JUnit.md)-based Java tests
- `testng`: [TestNG](TestNG.md)-based Java tests
- `apiritif`: [Apiritif](Apiritif.md)-based Python tests
- `pytest`: [pytest](PyTest.md)-based Python tests
- `rspec`: [RSpec](RSpec.md)-based Ruby tests
- `mocha`: [Mocha](Mocha.md)-based JavaScript tests
- `nunit`: [NUnit](NUnit.md)-based C# tests
- `xunit`: [xUnit](XUnit.md)-based C# tests

Note that automatic detection can't differentiate between `apiritif` and `pytest`-based test suites,
so if you want to run `pytest` - you have to specify it as a `runner` explicitly. Same situation with `nunit` and `xunit`.

Usage:
```yaml
execution:
- executor: selenium
  runner: apiritif
  scenario:
    script: tests/
```

## Scenario Examples

JUnit-based test with single .java file:
```yaml
execution:
- executor: selenium
  scenario: simple

scenarios:
  simple:
    script: /home/user/selenium_tests.java
```

Running folder of test scripts with automatic runner detection:
```yaml
execution:
- executor: selenium
  scenario: simple
  
scenarios: 
  simple:
    script: /home/user/folder/
```

Extended scenario with runner options:
```yaml
execution:
- executor: selenium
  iterations: 5  # loop over test suite for 5 times
  runner: junit
  scenario: complex
  
scenarios:
  complex:
    script: /home/user/tests/my_test.java
    additional-classpath:  # optional, following libs will be added to java classpath
    - /home/user/lib_one.jar
    - /home/user/lib_two.jar    
    
modules:
  junit:
    jar-name: compiled_jar_from_provided_sources.jar
        
reporting:
- module: junit-xml
```

## Requests Scenario

Selenium executor supports building test script from the `requests` option of `scenario`. Look at [Apiritif executor manual page](Apiritif.md#Scenario) for more information. Note: it that case `test-mode` will be equal `selenium`

## Automatic Installation of Web Driver

By default, Taurus will download ChromeDriver and GeckoDriver and put them in PATH when running tests.

You can configure this behaviour with the following options:
```yaml
execution:
- executor: selenium
  iterations: 1
  scenario: simple
  
scenarios:
  simple:
    requests:
    - http://blazedemo.com/

modules:
  selenium:
    chromedriver:
      version: 2.30
      download-link: https://chromedriver.storage.googleapis.com/{version}/chromedriver_{arch}.zip
    geckodriver:
      version: 0.17.0
      download-link: https://github.com/mozilla/geckodriver/releases/download/v{version}/geckodriver-v{version}-{arch}.{ext}
```

_This is available only in [unstable snapshot](https://gettaurus.org/install/Installation/#Latest-Unstable-Snapshot)._

By default, Taurus will download the appropriate ChromeDriver and GeckoDriver using webdriver-manager and put them in PATH when running tests.

You can also use already downloaded drivers by the following options:
```yaml
execution:
- executor: selenium
  iterations: 1
  scenario: simple
  
scenarios:
  simple:
    requests:
    - http://blazedemo.com/

modules:
  selenium:
    chromedriver:
      path: /full/path/to/driver
    geckodriver:
      path: /full/path/to/driver
```

## Using Virtual Display on Linux

If you want to run headless tests on Linux using virtual framebuffer (Xvfb), you can tell Taurus to run virtual
display by using following config piece:

```yaml
services:
- module: virtual-display
  width: 1024
  height: 768
```
See more info [here](Services.md#Virtual-Display-Service).

## Browser Options

You can configure the following browser options:  
- `ignore-proxy`: boolean  
HTTP_PROXY and HTTPS_PROXY will be ignored from being picked up and used. Option is only available starting from Selenium version 4.  
- `arguments`: list  
Add [command-line arguments](https://peter.sh/experiments/chromium-command-line-switches/) to use when starting browser.  
- `experimental-options`: dict  
Add a dictionary of experimental options. Option is only available in Chrome.  
- `preferences`: dict  
Add a [dictionary of preferences](http://kb.mozillazine.org/index.php?title=Category:Preferences&until=Browser.urlbar.restrict.typed). Option is only available in Firefox.

```yaml
execution:
- executor: selenium
  scenario: simple
  capabilities:
    browserName: chrome
  
scenarios:
  simple:
    requests:
      - label: label_1
        actions:
          - go(http://blazedemo.com/)

modules:
  selenium:
    options:
      ignore-proxy: true
      arguments:  # for Chrome and Firefox, but each browser has its own set of available arguments
        - window-size=1920x1080  # to start with a specific window size in Chrome
        - --use-fake-device-for-media-stream  # to emulate video from camera in Chrome
        - --use-fake-ui-for-media-stream  # to emulate video from camera in Chrome
        - user-agent=<ser_agent_value>  # to change user agent in Chrome
      experimental-options:  # only for Chrome
        mobileEmulation:
          deviceName: "iPhone X"  # to Emulate iPhone X in Chrome
      preferences:  # only for Firefox
        general.useragent.override: <ser_agent_value>  # to change user agent in Firefox
```

## Appium

[Appium](http://appium.io) is a tool for testing native mobile applications.
Taurus supports only Python scripts for Appium in Selenium executor. Additionally, you can use Taurus services to run
[Appium server](Services.md#Appium-Loader) and [Android emulator](Services.md#Android-Emulator-Loader).
Here is a typical example of usage:

```yaml
execution:
- executor: selenium
  scenario: ap_scen

scenarios:
  ap_scen:
    script: test_appium_script.py

services:
- appium
- android-emulator

modules:
  android-emulator:
    avd: android10_arm128
```  

## Conversion of Tests into JMeter format

You can convert your Selenium tests into JMX files by using a Proxy2JMX Converter module, as described
[here](Proxy2JMX.md#Proxy2JMX-Converter).
