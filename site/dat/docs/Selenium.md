# Selenium Executor
Allows to run functional tests locally with Selenium WebDriver. Currently supported selenium test runners are:
- Java + JUnit
- Java + TestNG
- Python + UnitTest
- Ruby + RSpec
- JavaScript + Mocha

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

## Specifying Test Runner Explicitly

By default, Taurus tries to automatically detect the language your tests are written in. If, for some reason,
the autodetection fails - you can specify test runner explicitly with `runner` execution-level option.

Supported values:
- `nose` - nosetests-based Python tests
- `junit` - JUnit-based Java tests
- `testng` - TestNG-based Java tests
- `rspec` - RSpec-based Ruby tests
- `mocha` - Mocha-based JavaScript tests

Usage:
```yaml
execution:
- executor: selenium
  runner: nose
  scenario:
    script: tests/
```


## JUnit Runner

JUnit runner corresponds to 'junit' `runner` value.

All `.java` files will be compiled and packed into jar file before running tests. All necessary tools will be
downloaded and installed automatically into `~/.bzt/selenium-taurus`.

Configuration options:

```yaml
modules:
  selenium:
    selenium-tools:
      junit:
        path: ~/.bzt/selenium-taurus/tools/junit/junit.jar  # path to JUnit framework
        selenium-server: ~/.bzt/selenium-taurus/selenium-server.jar  # path to Selenium Standalone Server
        hamcrest-core: ~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar  # path to Hamcrest lib
        json-jar: ~/.bzt/selenium-taurus/tools/junit/json.jar  # path to JSON lib
        jar-name: compiled.jar,  # set name of jar file when compiling from java source files 
        working-dir: classes  # set name of runner working directory within artifacts dir
        compile-target-java: 1.7  # -source and -target option value for javac
```

When running tests, Taurus will automatically add `selenium-server`, `json-jar`, `hamcrest-core`, along with JUnit jar
to the classpath. If your test suite requires additional libraries - you can specify them as a list of jars with
`additional-classpath` option.

```yaml
scenarios:
  script-with-depepdencies:
    script: tests/FrontendTest.java
    additional-classpath:
    - deps/gson-1.0.1.jar
    - deps/common-utils-0.15.1.jar    
```

## TestNG Runner

TestNG runner corresponds to 'testng' `runner` value.

Just like with JUnit runner, all `.java` files will be compiled and packed into the jar before running the tests. 

TestNG configuration file (testng.xml) can be specified with `testng-xml` option. If the options isn't specified —
Taurus will attempt to find TestNG config automatically by looking for 'testng.xml' file in current directory and
in the script directory.
If no TestNG configuration is found — Taurus will launch all tests from the test suite.

Just like JUnit runner, TestNG runner supports the `additional-classpath` option.

Configuration options:

```yaml
modules:
  selenium:
    selenium-tools:
      testng:
        path: ~/.bzt/selenium-taurus/tools/testng/testng.jar  # TestNG jar
        selenium-server: ~/.bzt/selenium-taurus/selenium-server.jar  # path to Selenium Standalone Server
        hamcrest-core: ~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar  # path to Hamcrest lib
        json-jar: ~/.bzt/selenium-taurus/tools/junit/json.jar  # path to JSON lib
        jar-name: compiled.jar,  # set name of jar file when compiling from java source files 
        working-dir: classes  # set name of runner working directory within artifacts dir
        compile-target-java: 1.7  # -source and -target option value for javac
```

## Python Unittest Runner

Python unittests are launched using NoseTest runner. It's `runner` value is 'nose'.

It is valid to specify both single Python module (single .py file) and a Python package (folder with Python modules
and packages).

Configuration options:

```yaml
modules:
  selenium:
    selenium-tools:
      nose:
        working-dir: classes  # set name of runner working directory within artifacts dir
        interpreter: /home/user/interpreter/python  # path to custom interpreter.
```

## Ruby RSpec Runner

You can run your RSpec-based test suite with Taurus. `runner` value for RSpec-based tests is `rspec`.

Minimal example:

```yaml
scenarios:
  rspec-suite:
    script: spec/  # folder with your specs you normally pass to RSpec

execution:
- executor: selenium
  scenario: rspec-suite
```

Just like other runners, RSpec runner supports `iterations` and `hold-for` options, in case you want to loop your test
execution.

Also, you can specify the path to Ruby interpreter, if you don't have it in $PATH:
```yaml
modules:
  selenium:
    selenium-tools:
      rspec:
        interpreter: /home/user/ruby-2.4/bin/ruby
```

The complete example of RSpec-based test suite and Taurus config can be found in
[examples/selenium/rspec-capybara](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/rspec-capybara)
folder of Taurus's repo.

## JavaScript Mocha Runner

Taurus supports running Mocha-based test suites. Corresponding `runner` value is `mocha`.

Minimal example:
```yaml
scenarios:
  mocha-tests:
    script: test/  # folder with your tests or path to one test script

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

## Appium

## About
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

## Requests Scenario

Selenium executor supports building test script from the `requests` option of `scenario`. In that case Taurus will
generate a Python script that will be launched with `nose`.

Supported features:
  - select browser
  - set timeout/think-time on both scenario and request levels
  - assertions (only requested page source inspected)
  - request method GET (only)
  - send keys, wait for items, click on items selected by ID/Name/CSS/XPath
   
Action names are built as `<action>By<selector type>(<selector>)`. Sometimes actions can have value. Options are:
  - `waitByID`, `waitByName`, `waitByLinkText`, `waitByCSS` and `waitByXPath` - to wait until desired option becomes present on page.
  Timeout is taken from scenario-level `timeout` option. Optionally, you can specify parameter `visible` to wait
  for visibility rather than presence (like `waitByName(elmName): visible`)
  - `clickByID`, `clickByName`, `clickByLinkText`, `clickByCSS` and `clickByXPath` - no parameter, just click on object
  - `keysByID`, `keysByName`, `keysByLinkText`, `keysByCSS` and `keysByXPath` - to enter keys into input items, requires parameter.
  Like this: `keysByName(MyFormInputName): Value To Enter`

Sample request scenario:
```yaml
scenarios:
  request_example:
    browser: Firefox  # available browsers are: ["Firefox", "Chrome", "Ie", "Opera"]
    timeout: 10  #  global scenario timeout for connecting, receiving results, 30 seconds by default
    think-time: 1s500ms  # global scenario delay between each request
    default-address: http://demo.blazemeter.com  # specify a base address, so you can use short urls in requests
    requests:
    - url: /  # url to open, only get method is supported
      actions:  # holds list of actions to perform
      - waitByCSS(body)
      - clickByID(mySubmitButton)
      - keysByName(myInputName): keys_to_type
      - waitByID(myObjectToAppear): visible
      assert: # assert executed after actions
      - contains:
        - blazemeter  # list of search patterns
        - Trusted
        subject: body # only body subject supported
        regexp: false  # treat string as regular expression
        not: false  # inverse assertion condition
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
  selenium:
    selenium-tools:
      junit:
        jar-name: compiled_jar_from_provided_sources.jar
        
reporting:
- module: junit-xml
```


## Using Virtual Display on Linux

If you want to run headless tests on Linux using virtual framebuffer (Xvfb), you can tell Taurus to run virtual
display by using following config piece:

```yaml
modules:
  selenium:
    virtual-display:
      width: 1024
      height: 768
```
Note: SeleniumExecutor uses shared virtual display for all executions. 

## Conversion of Tests into JMeter format

You can convert your Selenium tests into JMX files by using a Proxy2JMX Converter module, as described
[here](Proxy2JMX.md#Proxy2JMX-Converter).
