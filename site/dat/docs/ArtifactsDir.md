# Artifacts Directory Information

When Taurus starts, a directory called 'Artifacts directory' is created, and it contains all the files 
that already exist and/or created, which are necessary for the program to run.

## Common files
These are the files that are common for each executor:
- **bzt.log** contains all Taurus logs
- **effective.json** and **effective.yml** contain all Taurus settings and parameters
- **merged.json** and **merged.yml** contain final config version, that Taurus will run
- **config\_name.yml** is an original config file
- **executor.err**, **executor.log**, **executor.out** are files with errors, logs and output of an executor
- **kpi.jtl** or **ExecutorTester.ldjson** are files, which contain data of all requests which were made

## Executor-specific files
The following files are executor-specific:
- **apiritif.X.csv** contains data of all requests which were made with [Apiritif](Apiritif.md)
- **simulation.log** contains data of all requests which were made with [Gatling](Gatling.md)
- **pbench-kpi.txt** and **pbench-additional.ldjson** contain data of all requests which were made with [PBench](PBench.md)
- **test\_requests.py** is a Python file, generated for [Apiritif](Apiritif.md)
- **webdriver.log** is a log of a [Selenium](Selenium.md) webdriver
- **generated\_locust.py** is a Python file, generated for [Locust](Locust.md)
- **error.jtl** is a [JMeter](JMeter.md) specific file with its error
- **requests.jmx** and **modified\_requests.jmx** are specific [JMeter](JMeter.md) files with user's config data
- various **.properties** files contain different Taurus, executor, or runner properties
- **log.html** and **output.xml** are specific [Robot Executor](Robot.md) files with its data
- **gatling-launcher.sh** contains launch data for [Gatling](Gatling.md)
- **TaurusSimulation\_X.scala** contains Scala code for launching [Gatling](Gatling.md)
- **pbench.conf**, **pbench.sched**, **pbench.src** and **pbench-request-response.txt** contain specific [PBench](PBench.md) data
- **tsung-config.xml** is a configuration for [Tsung](Tsung.md)
