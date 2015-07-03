

### Selenium Executor
Allows to run functional tests with Selenium WebDriver.
Currently supported test cases: Java/JUnit, Python/Unittest.
Grid is not supported.

Scenario may be presented not only as single file but as folder.
Selenium executor uses two types of test runners: JUnit and Nose, test type and runner type are detected automatically.

### Supported file types:

  - .java/single file
  - .java/folder (flat structure, no package)
  - .java/folders (package)
  - .jar/single file
  - .jar/folder
  - .py/single file
  - .py/folder

### Runner types:

  - JUnit
  
.java files will be compiled and packed into jar file before running tests.
All necessary tools will be downloaded and installed automatically in ~/selenium-taurus.


config options:

```yaml
---
modules:
  selenium:
    selenium-tools:
      junit:
        path: "~/selenium-taurus/tools/junit/junit.jar"  # path to JUnit framework
        selenium-server: "~/selenium-taurus/selenium-server.jar"  # path to Selenium Standalone Server
        "jar-name": "compiled.jar",  # set name of jar file when compiling from java source files 
        "working-dir": "classes"  # set name of runner working directory within artifacts dir     
```

  - Python Nose


config options:
```yaml
---
modules:
  selenium:
    selenium-tools:
      nose:
        "working-dir": "classes"  # set name of runner working directory within artifacts dir
        "interpreter": "/home/user/interpreter/python"  # path to custom interpreter.
```

### Scenario samples
Minimal working scenarios:

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
