# JUnit Executor
Allows to run functional tests based on JUnit library.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Usage:
```yaml
execution:
- executor: junit  
  scenario:
    script: tests/  # folder with your tests or path to one test script
```

## Supported file types:

Test scenario may be presented not only as single file, folder or as a jar. Following variants are supported :

  - .java/single file
  - .java/folder (flat structure, no package)
  - .java/folders (package)
  - .jar/single file
  - .jar/folder

All `.java` files will be compiled and packed into jar file before running tests. All necessary tools will be
downloaded and installed automatically into `~/.bzt/selenium-taurus`.

## Configuration options:

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

## Scenario Example

```yaml
execution:
- executor: junit
  iterations: 5  # loop over test suite for 5 times  
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
