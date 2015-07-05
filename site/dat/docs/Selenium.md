# Selenium Executor
Allows to run functional tests locally with Selenium WebDriver. Currently supported selenium test languages: Java/JUnit, Python UnitTest. Using Selenium Grid is not supported.

Selenium executor uses two types of test runners: JUnit and Nose, test type and runner type are detected automatically. Scenario may be presented not only as single file but as a folder.

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
        path: "~/.bzt/selenium-taurus/tools/junit/junit.jar"  # path to JUnit framework
        selenium-server: "~/.bzt/selenium-taurus/selenium-server.jar"  # path to Selenium Standalone Server
        "jar-name": "compiled.jar",  # set name of jar file when compiling from java source files 
        "working-dir": "classes"  # set name of runner working directory within artifacts dir     
```

## Python Nose Runner

Configuration options:

```yaml
---
modules:
  selenium:
    selenium-tools:
      nose:
        "working-dir": "classes"  # set name of runner working directory within artifacts dir
        "interpreter": "/home/user/interpreter/python"  # path to custom interpreter.
```

## Scenario samples
Minimal working scenario:

```yaml
---
execution:
  executor: "selenium"
  scenario:
    script: "/home/user/selenium_tests.java"
```

or

```yaml
---
execution:
  executor: "selenium"
  scenario:
    script: "/home/user/folder/"
```

Extended scenario with runner options:

```yaml
---
execution:
  executor: "selenium"
  scenario:
    script: "/home/user/tests/my_test.java"
    additional-classpath:  # optional, following libs will be added to java classpath
      - /home/user/lib_one.jar
      - /home/user/lib_two.jar
modules:
  selenium:
    selenium-tools:
      junit:
        "jar-name": "compiled_jar_from_provided_sources.jar"
reporting:
  - module: junit-xml
```
