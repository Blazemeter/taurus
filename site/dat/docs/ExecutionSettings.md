# Execution Settings

Execution objects represent actual underlying tool executions. You can launch unlimited number of JMeter's, Gatling Tool's, Grinder Tools, etc. Executions are configured under top-level config key `execution`. Specifying single execution config is equivalent to specifying array of executions with single element, for example:

```yaml
---
execution:
  scenario:
    script: my-existing.jmx
```

is equivalent for 

```yaml
---
execution:
- scenario:
    script: my-existing.jmx
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

Default executor is `jmeter` and can be changed under [general settings](ConfigSyntax.md#top-level-settings) section.
```yaml
---
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
 - `throughput` - apply RPS shaper, limiting maximum RPS to throughput
 - `steps` - allows users to apply stepping ramp-up for concurrency and rps. Requires JMeter plugins to be installed!

```yaml
---
execution: 
- concurrency: 10
  ramp-up: 15s
  hold-for: 2m
  iterations: 1000
  throughput: 20
```

## Scenario

Scenario is a sequence of steps that is used to build script for underlying tool (e.g. generate JMX file for JMeter). Scenarios can be specified directly inside execution section. However, it is recommended to use special `scenarios` top-level config element to declare
scenarios and access them through aliases. Some examples:

```yaml
---
# embedded scenario
execution:
- scenario:
    requests:
      - http://localhost/1
      - http://localhost/2
```

```yaml
---
# referenced scenario
scenarios:
  get-requests:
    requests:
      - http://localhost/1
      - http://localhost/2

execution:
- scenario: get-requests
```

## Startup delay

You can run different executions at different times with `delay` option:
```yaml
---
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
---
execution:
- concurrency: 10
  hold-for: 20s
  start_at: '2020-03-25 23:15'  # must be string
  scenario: 
    requests:
    - http://blazedemo.com/
```
Supported time formats are:
- YYYY-MM-DD HH:MM:SS
- YYYY-MM-DD HH:MM
- HH:MM:SS
- HH:MM

You can describe your own format with `time-format` option (use directives of python method datetime.strptime())

## Additional Files

When your execution requires additional files (e.g. JARs, certificates etc). you may use `files` option of execution and list paths for files there.
