# JUnit Executor
Allows to run functional tests based on JUnit library.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time will be exceeded.
Also following `(load settings)[ExecutionSettings.md#Load-Profile]` are available: `concurrency`, `ramp-up`, `steps`.

Usage:
```yaml
execution:
- executor: junit  
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

## Configuration Options

```yaml
modules:
  junit:
    path: ~/.bzt/selenium-taurus/tools/junit  # path to directory of JUnit framework
    selenium-server: ~/.bzt/selenium-taurus/selenium-server.jar  # path to Selenium Standalone Server
    hamcrest-core: ~/.bzt/selenium-taurus/tools/junit/hamcrest-core.jar  # path to Hamcrest lib
    json-jar: ~/.bzt/selenium-taurus/tools/junit/json.jar  # path to JSON lib
    jar-name: compiled.jar,  # set name of jar file when compiling from java source files 
    working-dir: classes  # set name of runner working directory within artifacts dir
    compile-target-java: 1.7  # -source and -target option value for javac
    junit-version: 5 # use JUnit5 (turned off by default)
    properties:  # Java system properties
      propname: propvalue
```

Note: only specific names/versions of JUnit framework jars is supported.
If you want to use your own bundle at first just run test without `path` and revise `~/.bzt/selenium-taurus` directory.
For other jar tools (hamcrest, selenium-sever, etc.) extended setup is possible. You can choose follow params of them:
```yaml
modules:
  junit:
    selenium-server:
      download-link: http://my.own.host.com/selenium-server.jar
      version: 1.2.3
      path: /some/place/on/disk/selenium-server.jar
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
    properties:  # Java system properties, will extend module properties
      propname: propvalue
```

## Running Subset of Tests

You can specify list of specific classes, or methods, to run from available tests. For that, use `run-items` option under scenario or execution. This is optional parameter. See [example](#scenario-example) below.

You can also specify `include-categories` and `exclude-categories` under scenario or execution, to utilize [corresponding feature](https://github.com/junit-team/junit4/wiki/categories) of JUnit. These are optional parameters.


## Scenario Example

```yaml
execution:
- executor: junit
  iterations: 5  # loop over test suite for 5 times
  concurrency: 20   # number of virtual users
  ramp-up: 1m       # time of load growing
  steps: 5          # number of steps of growing
  scenario: complex
  run-items:
  - package.Class2#testmethod2
  include-categories:
  - categories.FastTests
  - categories.SmokeTests
  exclude-categories:
  - categories.SlowTests
  properties:
    target_url: http://prod.abc.def/ghi  # will extend module and scenario properties    
  
scenarios:
  complex:
    script: /home/user/tests/my_test.java
    run-items:
    - package.Class1
    - package.Class2#test2
    include-categories:
    - categories.SlowTests
    exclude-categories:
    - categories.FastTests
    - categories.SmokeTests
    additional-classpath:  # optional, following libs will be added to java classpath
    - /home/user/lib_one.jar
    - /home/user/lib_two.jar
    properties:
      target_url: http://testing.abc.def/ghi
      filesize: 10M    
    
modules:
  junit:
    jar-name: compiled_jar_from_provided_sources.jar
    properties:
      filesize: 1M    
        
reporting:
- module: junit-xml
```
