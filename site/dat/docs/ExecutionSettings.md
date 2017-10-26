# Execution Settings

Execution objects represent actual underlying tool executions. You can launch unlimited number of JMeter's, Gatling Tool's, Grinder Tools, etc. Executions are configured under top-level config key `execution`. Specifying single execution config is equivalent to specifying array of executions with single element, for example:

```yaml
execution:
  scenario: scenario_name    
```

is equivalent for 

```yaml
execution:
- scenario: scenario_name
```

However, users are encouraged to use array notation always to leverage the arrays auto-join capability when combining multiple config files into one. See [config merge rules](CommandLine.md#configuration-files-processing) for more details on this.

There are load profile and scenario settings that are common for all execution types, and each executor type can also have its own settings.

## Executor Types

Taurus tool may use different underlying tools as executors for scenarios. Currently supported tools are:
 
  - [JMeter](JMeter.md), executor type `jmeter`
  - [Selenium](Selenium.md), executor type `selenium`
  - [Gatling](Gatling.md), executor type `gatling`
  - [Grinder](Grinder.md), executor type `grinder`
  - [Locust](Locust.md), executor type `locust`
  - [PBench](PBench.md), executor type `pbench`
  - [Siege](Siege.md), executor type `siege`
  - [ApacheBenchmark](ApacheBenchmark.md), executor type `ab`
  - [Tsung](Tsung.md), executor type `tsung`
  - [Molotov](Molotov.md), executor type `molotov`
  - [JUnit](JUnit.md), executor type `junit`
  - [TestNG](TestNG.md), executor type `testng`
  - [Nose](Nose.md), executor type `nose`
  - [PyTest](PyTest.md), executor type `pytest`
  - [RSpec](RSpec.md), executor type `rspec`
  - [Mocha](Mocha.md), executor type `mocha`
  - [NUnit](NUnit.md), executor type `nunit`
  - [WebdriverIO](WebdriverIO.md), executor type `wdio`
  - [Robot](Robot.md), executor type `robot`

Default executor is `jmeter` and can be changed under [general settings](ConfigSyntax.md#top-level-settings) section.
```yaml
settings:
  default-executor: jmeter
```

You may contribute your efforts in supporting requests-scenarios for your favorite tool by discussing this on [project forum](https://groups.google.com/forum/#!forum/codename-taurus).

## Load Profile

Execution has several options to set load profile settings. Support for options is specific to executor type. Available settings are:

 - `concurrency` - number of target concurrent virtual users
 - `ramp-up` - ramp-up time to reach target concurrency
 - `hold-for` - time to hold target concurrency
 - `iterations` - limit scenario iterations number
 - `throughput` - apply RPS shaper, limiting maximum RPS to throughput, requires `ramp-up` and/or `hold-for`
 - `steps` - allows users to apply stepping ramp-up for concurrency and rps, requires `ramp-up`
 - `scenario` - name of scenario that described in `scenarios` part (see below)

```yaml
execution: 
- concurrency: 10
  ramp-up: 15s
  hold-for: 2m
  iterations: 1000
  throughput: 20
  scenario: scenario_name
```

## Scenario

Scenario is a sequence of steps that is used to build script for underlying tool (e.g. generate JMX file for JMeter). It is described in special `scenarios` top-level config element. There are three examples of scenario syntax:

```yaml
scenarios:
  get-requests:                     # normal form: scenario is dictionary
    requests:
      - http://localhost/1
      - http://localhost/2  
  only-script: grinder_script.py    # short form: just script

execution:
- concurrency: 10
  hold-for: 1m
  scenario: get-requests  
- executor: gatling
  concurrency: 5
  iterations: 10
  scenario: only-script
- hold-for: 20s
  scenario: my_jmx_file.jmx         # shortest form: only script file name  
```

## Startup Delay

You can run different executions at different times with `delay` option:
```yaml
execution:
- concurrency: 10
  hold-for: 20s
  scenario: main
- concurrency: 20
  hold-for: 15s
  scenario: main
  delay: 10s

scenarios:
  main:
    requests:
    - http://blazedemo.com/
```
By this way, the first execution works 10 seconds, then two executions will work 10 seconds together, then the first will stop and the second will complete its work in 5 seconds.

Another way to schedule is usage of `start-at`:
```yaml
execution:
- concurrency: 10
  hold-for: 20s
  start-at: '2020-03-25 23:15'  # must be string
  scenario: sample
  
scenarios:
  sample:
    requests:
    - http://blazedemo.com/
```
Supported time formats are:
- YYYY-MM-DD HH:MM:SS
- YYYY-MM-DD HH:MM
- HH:MM:SS
- HH:MM

## Additional Files

When your execution requires additional files (e.g. JARs, certificates etc.) and you plan to send tests to the `[Сloud](Cloud.md#Cloud-Provisioning)`, you may use `files` option of execution and list paths for files there. 

## Sequential Execution

By default, Taurus runs items under `execution` in parallel. To switch it into sequential mode, run it with `-sequential` command-line option. This is an alias for this setting:

```yaml
modules:
  local:
    sequential: true
```
Keep in mind: sequential execution doesn't work in the `[Сloud](Cloud.md#Cloud-Provisioning)` mode. And as modules start sequentially, `[Startup Delay](#Startup-Delay)` doesn't matter in this case.
