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
  scenario:
    script: /home/user/selenium_tests.java
```

or

```yaml
---
execution:
- executor: selenium
  scenario:
    script: /home/user/folder/
```

Extended scenario with runner options:

```yaml
---
execution:
- executor: selenium
  iterations: 5
  scenario:
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
execution:
- executor: selenium
  hold-for: 5m
  scenario:
    browser: Firefox  # available browsers are: ["Firefox", "Chrome", "Ie", "Opera"]
    timeout: 10  #  global scenario timeout for connecting, receiving results, 30 seconds by default
    think-time: 1s500ms  # global scenario delay between each request
    requests:
    - url: http://demo.blazemeter.com/  # url to open, only get method is supported
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