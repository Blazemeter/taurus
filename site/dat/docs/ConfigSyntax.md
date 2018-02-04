# Taurus Configuration Syntax

Configuration dictionary has several top-level keys:

 - `[execution](ExecutionSettings.md)` - main section, declares tools to be executed during run, scenarios to use, etc
 - `[reporting](Reporting.md)` - analysis and reporting settings, list of reporting modules to process results 
 - `[services](Services.md)` - service modules configuration 
 - `scenarios` - dictionary of scenario specifications with aliases, to be referred from executions
 - `[modules](#Modules-Settings)` - list of classes to load and their respective settings
 - `[settings](#Top-Level-Settings)` - some top-level settings for the tool
 - `provisioning` - advanced option, allows using resources other than local to have distributed high-load test, available if you have corresponding provider installed and configured
 - `[included-configs](#Included-Configs)` - this section allows to merge additional config files
 
Example for config that touches all sections:

```yaml
execution:
- concurrency: 10
  hold-for: 5m
  ramp-up: 2m
  scenario: sample
  
scenarios:
  sample:
    headers:
        Connection: close
    requests:
    - http://localhost/

reporting:
- module: final-stats
- module: console

modules:
  jmeter:
    path: ./local/jmeter
    properties:
        log_level: DEBUG
  console:
    disable: false
    
settings:
  check-interval: 5s
  default-executor: jmeter

provisioning: local
```

## Multiple Files Merging Rules

The rules for merging multiple configuration files into single are following:

 1. Process starts with empty configuration structure `{}`
 2. Files are loaded one by one, every file must contain dictionary, either in YAML or in JSON format (+INI for command-line overrides)
 3. Loaded dictionary is merged recursively into configuration, dictionaries are merged and lists are joined.
 4. If dictionary key has `~` prefix, it will overwrite the value instead of merging
 5. If dictionary key has `^` prefix, it will delete the corresponding key/value pair
 6. If dictionary key has `$` prefix and the value under that key is a list, it will perform element-wise merging of corresponding list items.
 7. If dictionary values has different type, eg. string value vs array, the value will be overwritten and the warning message will be issued

Most of configuration elements have their default values and also have some shorthand forms to specify trivial cases. We encourage users to learn these rules to speed up configuration files creation and make them short.

Important to know that merging happens _before_ any default value or shorthand rules are processed, so you should use the same approach to express settings in all files. The most common case is `execution` branch, that can be either dictionary (key/value pairs) or array of dictionaries. Attempting to merge file containing array of executions with single-execution will lead to overwriting instead of appending array. For that case, have a practice to always use either array or dictionary in files that are being merged.


## Modules Settings

The Module Settings section is a dictionary, having module aliases as keys and setting dictionaries as values. Module settings are specific to every module, the only common option is `class`, containing full python class name as value. Module aliases are used to refer from reporting settings, tool executors and many other places. In fact, everything in Taurus is implemented through modules and engine has no dependencies to specific module implementations, everything is config-driven.

The shorthand is to specify module class string instead of settings dictionary, Taurus will expand it into dict automatically. For example,

```yaml
modules:
  final-stats: bzt.modules.reporting.FinalStatus
```

is equivalent to

```yaml
modules:
  final-stats:
    class: bzt.modules.reporting.FinalStatus
```
 
## Top-Level Settings

Available settings are:

 - `artifacts-dir` - path template where to save artifact files, uses [strftime template syntax](http://strftime.org/)
 - `check-interval` - polling interval that used by engine after startup and until shutdown to determine if test is need to be stopped 
 - `aggregator` - module alias for top-level [results aggregator](Reporting.md#results-reading-and-aggregating-facility) to be used for collecting results and passing it to reporters
 - `default-executor` - module alias for executor that will be used by default for [executions](ExecutionSettings)
 - `proxy` - proxy settings for BZA feeding, Taurus will use proxy settings from OS environment by default.
 
See default settings below:

```yaml
settings:
  artifacts-dir: /tmp/%H%M%S # path where to save artifacts, default is %Y-%m-%d_%H-%M-%S.%f
  aggregator: consolidator
  default-executor: jmeter
  check-interval: 1
  proxy:  # custom proxy settings
    address: http://127.0.0.1:8080  # proxy server address
    username: user  # username and password used if authentication is configured on proxy server
    password: 12345
  check-updates: true  # check for newer version of Taurus on startup
  verbose: false  # whenever you run bzt with -v option, it sets debug=true, 
                  # some modules might use it for debug features,
                  # setting this through config also switches CLI verbosity
```

## Human-Readable Time Specifications
All time specifications in Taurus configs, including timeouts and durations, are _always_ expressed in unit of _seconds_.
Use special strings convention to make it human-readable. Examples:

  - `1s200ms` = 1 second 200 milliseconds
  - `1d 2h 3m 4s` = 93784 seconds
 
`d` - days, `h` - hours, `m` - minutes, `s` - seconds, `ms` - milliseconds, optional space characters allowed

If you have found config instruction that does not follow this rule, report immediately, this is most likely a bug.

## YAML/JSON Format for Config Files

As you know, JSON is a subset of YAML. But in BZT config files there is no
usage for YAML-JSON incompatibilities, so you can use either JSON or YAML for
your configs. Also you can have some of configs in JSON and some in YAML, the
engine will perfectly deal with it. For example, following JSON file:
 
```json
{
  "execution": [
    {
      "executor": "jmeter",
      "scenario": "sample"
    }
  ],
  "scenarios": {
     "sample": {
       "script": "tests/jmx/dummy.jmx"
     }
  },
  "provisioning": "local",
  "aggregator": "aggregator",
  "reporting": [
    {
      "module": "final-stats"
    }
  ],
  "modules": {
    "jmeter": {
      "path": "build/jmeter/bin/jmeter",
      "class": "bzt.modules.jmeter.JMeterExecutor",
      "properties": {
        "jmeter.save.saveservice.autoflush": "true"
      }
    }
  }
}
```
 
is equivalent to YAML:

```yaml
aggregator: aggregator

execution:
- executor: jmeter
  scenario: jmx_sample
  
scenarios:
  jmx_sample:
    script: tests/jmx/dummy.jmx
      
modules:
  jmeter:
    class: bzt.modules.jmeter.JMeterExecutor
    path: build/jmeter/bin/jmeter
    properties:
      jmeter.save.saveservice.autoflush: 'true'
      
provisioning: local

reporting:
- module: status
```

Look for `merged.yml/json` and `effective.yml/json` file pairs in artifacts to see matching examples. 

If you're not familiar with YAML, you can check out our [YAML Tutorial](YAMLTutorial.md).

Hint: YAML config files on Linux/MacOS allows a trick of self-executing config. To have it, add [shebang line](https://en.wikipedia.org/wiki/Shebang_(Unix\)) as first line of your file, like this:

```yaml
#! /usr/local/bin/bzt
execution:
- hold-for: 1m
  scenario: simple
  
scenarios:
  simple:
    requests:
     - http://blazedemo.com/
```

Then add execution flag to the file:

```bash
chmod +x myscript.yml
```

Now you are able to start this file on its own:
```bash
./myscript.yml -o execution.hold-for=5m
```

## Included Configs

After all config files loaded, Taurus will also recursively merge into resulting configuration any `included-configs` from the list. Example syntax piece:

```yaml
included-configs:  # it must be a list of string values
- additional-local-file.yml  # to add local file just set its path
- http://central.host/mystorage/remote.yml  # you can also download config from http/https location
```
