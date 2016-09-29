# Selenium Executor
Allows to run functional tests locally with Selenium WebDriver. Currently supported selenium test languages: Java/JUnit, Python UnitTest. Using Selenium Grid is not supported.

Selenium executor uses two types of test runners: JUnit and Nose, test type and runner type are detected automatically. Scenario may be presented not only as single file but as a folder.

Taurus can repeat Selenium script in a loop until desired number of `iterations` will complete or `hold-for` time will be exceeded.

## Supported file types:

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

## JUnit Runner

All `.java` files will be compiled and packed into jar file before running tests. All necessary tools will be downloaded and installed automatically into `~/.bzt/selenium-taurus`.

Configuration options:

```yaml
---
modules:
  selenium:
    selenium-tools:
      junit:
        path: ~/.bzt/selenium-taurus/tools/junit/junit.jar  # path to JUnit framework
        selenium-server: ~/.bzt/selenium-taurus/selenium-server.jar  # path to Selenium Standalone Server
        jar-name: compiled.jar,  # set name of jar file when compiling from java source files 
        working-dir: classes  # set name of runner working directory within artifacts dir
        compile-target-java: 1.7  # -source and -target option value for javac
```

## Python Unittest Runner

Python unittests are launched using NoseTest runner.

Configuration options:

```yaml
---
modules:
  selenium:
    selenium-tools:
      nose:
        working-dir: classes  # set name of runner working directory within artifacts dir
        interpreter: /home/user/interpreter/python  # path to custom interpreter.
```

## Scenario Samples
Minimal working scenario:

```yaml
---
execution:
- executor: selenium
  scenario: simple

scenarios:
  simple:
    script: /home/user/selenium_tests.java
```

or

```yaml
---
execution:
- executor: selenium
  scenario: simple
  
scenarios: 
  simple:
    script: /home/user/folder/
```

Extended scenario with runner options:

```yaml
---
execution:
- executor: selenium
  iterations: 5
  scenario: complex
  
scenarios:
  complex:
    script: /home/user/tests/my_test.java
    additional-classpath:  # optional, following libs will be added to java classpath
    - /home/user/lib_one.jar
    - /home/user/lib_two.jar    
    
modules:
  selenium:
    selenium-tools:
      junit:
        jar-name: compiled_jar_from_provided_sources.jar
        
reporting:
- module: junit-xml
```

## Ruby RSpec Runner

You can run your RSpec-based test suite with Taurus.

Minimal example:

```yaml
scenarios:
  rspec-suite:
    script: spec/  # folder with your specs you normally pass to RSpec

execution:
- executor: selenium
  scenario: rspec-suite
```

Just like JUnit-based and nosetests-based runners, RSpec runner supports `iterations` and `hold-for` options,
in case you want to loop your test execution.

The complete example of RSpec-based test suite and Taurus config can be found in
[examples/selenium/rspec-capybara](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/rspec-capybara)
folder of Taurus's repo.

## JavaScript Mocha Runner

Taurus supports running Mocha-based test suites.

Minimal example:
```yaml
scenarios:
  mocha-tests:
    script: test/  # folder with your tests or path to one test file

execution:
- executor: selenium
  scenario: mocha-tests
```

Looping test execution is possible with `iterations` and `hold-for` options. If none of these
options are specified — Mocha will do one iteration over a test suite. If both time limit
and iteration limit are specified - tests will keep executing until any of limits is reached.

```yaml
execution:
- executor: selenium
  iterations: 10  # perform 10 iterations over test suite
  hold-for: 5m  # limit test execution to 5 minutes
  scenario: mocha-tests
```

You can also find an example of complete Mocha-based test suite and Taurus config to run it with
in [examples/selenium/mocha](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/mocha)
folder of Taurus's repo.

## Requests Scenario
Selenium executor partially supports building scenario from requests.
Supported features:
  - select browser
  - set timeout/think-time on both scenario/request levels
  - assertions (only requested page source inspected)
  - request method GET (only)

Sample request scenario
```yaml
---
scenarios:
  request_example:
    browser: Firefox  # available browsers are: ["Firefox", "Chrome", "Ie", "Opera"]
    timeout: 10  #  global scenario timeout for connecting, receiving results, 30 seconds by default
    think-time: 1s500ms  # global scenario delay between each request
    default-address: http://demo.blazemeter.com  # specify a base address, so you can use short urls in requests
    requests:
    - url: /  # url to open, only get method is supported
      assert:
      - contains:
        - blazemeter  # list of search patterns
        - Trusted
        subject: body # only body subject supported
        regexp: false  # treat string as regular expression
        not: false  # inverse assertion condition
```

## Using Virtual Display on Linux

If you want to run headless tests on Linux using virtual framebuffer (Xvfb), you can tell Taurus to run virtual display by using following config piece:

```yaml
---
modules:
  selenium:
    virtual-display:
      width: 1024
      height: 768
```
Note: SeleniumExecutor uses shared virtual display for all executions. 

## Enforcing Test Language

By default Taurus tries to automatically detect the language tests are written in. If autodetection fails - you can enforce specific
language with `language` execution-level option.

Supported values:
- `python-nose` - nosetests-based Python tests
- `java-junit` - JUnit-based Java tests
- `ruby-rspec` - RSpec-based Ruby tests
- `js-mocha` - Mocha-based JavaScript tests

```yaml
---
execution:
- executor: selenium
  language: python-nose
  scenario:
    script: tests/
```

## Conversion of Tests into JMeter format
You can convert your Selenium tests as described [here](Proxy2JMX.md#Proxy2JMX-Converter).
