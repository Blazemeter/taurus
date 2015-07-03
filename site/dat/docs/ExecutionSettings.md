# Execution Settings

Execution objects represent actual underlying tool executions. You can launch unlimited number of JMeter's, Gatling Tool's, Grinder Tools, etc. Executions are configured under top-level config key "execution". Specifying single execution config is equivalent to specifying array of executions with single element, for example:

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

## Scenario

Scenario is a sequence of steps that is used to build script for underlying tool (e.g. generage JMX file for JMeter). Scenarios can be specified directly inside execution section. However, it is recommended to use special `scenarios` top-level config element to declare
scenarios and access them through aliases. Read more on building scenarios [here](ScenarioBuilding.md). Some examples:

```yaml
---
# embedded scenario
execution:
  scenario:
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
  scenario: get-requests
```

## Load Profile

Execution has several options to set load profile settings. Available settings are:

 - `concurrency` - number of target concurrent virtual users
 - `ramp-up` - ramp-up time to reach target concurrency
 - `hold-for` - time to hold target concurrency
 - `iterations` - limit scenario iterations number
 - `throughput` - apply RPS shaper, limiting maximum RPS to throughput
 - `steps` - allows users to apply stepping ramp-up for concurrency and rps. Requires JMeter plugins to be installed!

```yaml
---
execution: 
  concurrency: 10
  ramp-up: 15s
  hold-for: 2m
  iterations: 1000
  throughput: 20
```

## Executor Types

Taurus tool may use different underlying tools as executors for scenarios. Currently supported tools are: 
  - JMeter, executor type `jmeter`
  - Grinder, executor type `grinder`
  - Gatling, executor type `gatling`
  - Selenium, executor type `selenium`

```yaml
---
settings:
  default-executor: jmeter
```

