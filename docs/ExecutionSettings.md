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

```yaml
---
settings:
  default-executor: jmeter
```

### JMeter Executor

This executor type is used by default, it uses [Apache JMeter](http://jmeter.apache.org/) as underlying tool.

#### JMeter Location & Auto-Installation

If there is no JMeter installed at the configured `path`, Taurus will attempt to install latest JMeter and Plugins into
this location, by default `~/jmeter-taurus/bin/jmeter`. You can change this setting to your preferred JMeter location (consider putting it into `~/.bzt-rc` file). All module settings that relates to JMeter path and auto-installing are listed below:
 
```yaml
---
modules:
  jmeter:
    path: ~/jmeter-taurus/bin/jmeter
    download-link: http://apache.claz.org/jmeter/binaries/apache-jmeter-{version}.zip
    version: 2.13
    plugins-download-link: http://jmeter-plugins.org/files/JMeterPlugins-{plugin}-1.2.1.zip
```

#### Run Existing JMX File
```yaml
---
execution:
  scenario:
    script: tests/jmx/dummy.jmx
```

or simply `bzt tests/jmx/dummy.jmx`

TODO: explain how multi-thread group will accept concurrency with maintained proportion

#### JMeter Properties
There are two places to specify JMeter properties: global at module-level and local at scenario-level. Scenario properties are merged into global properties and resulting set comes as input for JMeter, see corresponding `.properties` file in artifacts.

Global properties are set like this:
```yaml
---
modules:
  jmeter:
    properties:
      my-hostname: www.pre-test.com
      log_level.jmeter: WARN
      log_level.jmeter.threads: DEBUG
```

Scenario-level properties are set like this:
```yaml
---
execution:
  scenario: 
    properties:
        my-hostname: www.prod.com
        log_level.jmeter: DEBUG
```

#### Open JMeter GUI
When you want to verify or debug the JMX file that were generated from your requests scenario, you don't need to search for the file on disk, just enable GUI mode for JMeter module:
```yaml
---
modules:
  jmeter:
    gui: false  # set it to true to open JMeter GUI instead of running non-GUI test
```

For the command-line, use alias `-gui` or option `-o modules.jmeter.gui=true`, without the need to edit configuration file.

#### Run JMeter in Distributed Mode
Distributed mode for JMeter is enabled with simple option `distributed` under execution settings, listing JMeter servers under it:

```yaml
---
execution:
  distributed: 
    - host1.mynet.com
    - host2.mynet.com
    - host3.mynet.com
  scenario:
    script: my-test.jmx
```


#### Modifications for Existing Scripts

JMeter executor allows you to apply some modifications to the JMX file before running JMeter (this affects both existing JMXes and generated from requests):

```yaml
---
execution:
  scenario:
    script: tests/jmx/dummy.jmx
    variables: # add User Defined Variables component to test plan, overriding other global variables
      user_def_var: http://demo.blazemeter.com/api/user
      user_def_var2: user_def_val_2
    modifications:
        disable:  # Names of the tree elements to disable
            - Thread Group 1
        enable:  # Names of the tree elements to ensable
            - Thread Group 2
        set-prop:  # Set element properties, selected as [Element Name]>[property name]
          "HTTP Sampler>HTTPSampler.connect_timeout": "0"
          "HTTP Sampler>HTTPSampler.protocol": "https"
```


### Gatling Executor

configuration options:

 - `path`: "/somepath/folder/bin/gatling_executable"
    Path to Gatling executable.
    If no Gatling executable found, it will be automatically downloaded and installed in "path".
    By default "~/gatling-taurus/bin/gatling.sh".
    
 - `download-link`:"http://somehost/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    Link to download Gatling.
    By default: "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    
 -  `version`: "2.1.4"
    Gatling version, by default "2.1.4"

#### Run Gatling Tool

```javascript
{
  "execution": {
    "executor": "gatling",
    "scenario": {
      "script": "tests/gatling/BasicSimulation.scala",
      "simulation": "mytest.BasicSimulation"
    }
  }
}
```

### Grinder Executor

configuration options:

 - "path": "/somepath/folder/"
    Path to Grinder.
    If no grinder.jar found in folder/lib/, Grinder tool will be automatically downloaded and installed in "path".
    By default "~/grinder-taurus/".
    
 - "download-link":"http://somehost/grinder-{version}-binary.zip"
    Link to download Grinder.
    By default "http://switch.dl.sourceforge.net/project/grinder/The%20Grinder%203/{version}/grinder-{version}-binary.zip"
    
 -  "version": "3.11"
    Grinder version, by default "3.11"

#### Run Grinder Tool
```javascript
{
  "execution": [
    {
      "executor": "grinder",
      "concurrency": 3,
      "ramp-up": 10,
      "iterations": 20,
      "scenario": {
        "script": "tests/grinder/helloworld.py",
        "properties_file": "tests/grinder/grinder.properties",
        "properties": {
          "grinder.useConsole": false
        }
      }
    },
    {
      "executor": "grinder",
      "concurrency": 2,
      "ramp-up": 5,
      "iterations": 10,
      "scenario": {
        "requests": [
          "http://demo.blazemeter.com/",
          "http://demo.blazemeter.com/api"
        ]
      }
    }
  ]
}
```
