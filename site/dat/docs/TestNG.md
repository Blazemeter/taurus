# TestNG Executor
Allows to run functional tests based on TestNG library.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Usage:
```yaml
execution:
- executor: testng  
  scenario:
    script: tests/  # folder with your tests or path to one test script
```

## Supported File Types

Test scenario may be presented not only as single file, folder or as a jar. Following variants are supported :

  - .java/single file
  - .java/folder (flat structure, no package)
  - .java/folders (package)
  - .jar/single file
  - .jar/folder

All `.java` files will be compiled and packed into jar file before running tests. All necessary tools will be
downloaded and installed automatically into `~/.bzt/selenium-taurus`.

TestNG configuration file (testng.xml) can be specified with `testng-xml` option on the execution level. If the options isn't specified — Taurus will attempt to find TestNG config automatically by looking for 'testng.xml' file in the script directory. You can turn off last detection with autodetect-xml option of testng:
```yaml
execution:
- executor: testng
  testng-xml: /path/to/your_testng.xml   # explicit path to test config
  scenario:
    script: some_file.java
modules:
  testng:
    autodetect-xml: False    # default value: True
```
If no TestNG configuration is found — Taurus will launch all tests from the test suite.

Just like JUnit runner, TestNG runner supports the `additional-classpath` option on the scenario and settings (i.e. modules.testng) levels.

## Configuration options:

```yaml
modules:
  testng:
    path: ~/.bzt/selenium-taurus/tools/testng/testng.jar  # TestNG jar
    selenium-server: ~/.bzt/selenium-taurus/selenium-server.jar  # path to Selenium Standalone Server
    hamcrest-core: ~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar  # path to Hamcrest lib
    json-jar: ~/.bzt/selenium-taurus/tools/junit/json.jar  # path to JSON lib
    jar-name: compiled.jar,  # set name of jar file when compiling from java source files 
    working-dir: classes  # set name of runner working directory within artifacts dir
    compile-target-java: 1.7  # -source and -target option value for javac
```
