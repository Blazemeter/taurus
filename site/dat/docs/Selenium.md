# Selenium Executor
Selenium is virtual executor provided you ability to run functional tests locally with Selenium WebDriver by choosing appropriate executor. Currently supported executors are:
- [JUnit](JUnit.md) (Java)
- [TestNG](TestNG.md) (Java)
- [Apiritif](Apiritif.md) (Python)
- [PyTest](PyTest.md) (Python)
- [RSpec](RSpec.md) (Ruby)
- [Mocha](Mocha.md) (JavaScript)
- [NUnit](NUnit.md) (C#)

Selenium Grid isn't supported for tests described with Taurus language, but if you have your own test suite that uses
Selenium Grid to manage browser instances - Taurus will run these tests just fine.

Selenium executor uses multiple test runners (JUnit, TestNG, Mocha, etc), test type is detected automatically.
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
  - .dll/folder

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

Note that automatic detection can't differentiate between `apiritif` and `pytest`-based test suites,
so if you want to run `pytest` - you have to specify it as a `runner` explicitly.

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

Selenium executor supports building test script from the `requests` option of `scenario`. Look at [Apiritif executor manual page](Apiritif.md#Request-Scenario) for more information. Note: it that case `test-mode` will be equal `selenium`

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

## Using Virtual Display on Linux

If you want to run headless tests on Linux using virtual framebuffer (Xvfb), you can tell Taurus to run virtual
display by using following config piece:

```yaml
services:
- module: virtual-display
  width: 1024
  height: 768
```

Note: SeleniumExecutor uses shared virtual display for all executions.

Deprecation Note: using `virtual-display` in `selenium` module settings is deprecated and will be removed
in the future, you should use [Virtual Display Service](Services.md#Virtual-Display-Service).

## Appium

[Appium](http://appium.io) is a tool for testing naitive mobile applications.
Taurus supports only python scripts for appium in Selenium executor. Additionally, you can use taurus services to run
[Appium server](Services.md#Appium-Loader) and [Android emulator](Services.md#Android-Emulator-Loader).
There is typical example of usage:
 
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
