# Services Subsystem

When you need to perform some actions before test starts, after test starts, or in parallel with running test, you should use services subsystem in Taurus. To configure services, use `services` top-level section of config with list of service module configs:

```yaml
---
services:
- shellexec:
    prepare: ...
```

## Pass/Fail Criteria

Every execution has pass/fail status and there is a way to set this status based on runtime criteria. Special `passfail` service offers this functionality. Another useful feature of pass/fail criteria is _auto-stop_ functionality, allowing to interrupt failed tests automatically, sparing the time and resources.

Pass/fail criteria are specified as array of `criteria`, set through `services` item in config:

```yaml
---
services:
- module: passfail
  criterias:
  - avg-rt of Sample Label>150ms for 10s, stop as failed
  - fail of Sample Label>50% for 10s, stop as failed
```

The above example use short form for criteria, its general format is `subject of label{condition}threshold {logic} timeframe, action as status`, where:

  - `subject` is the KPI that will be compared, listed below
  - `label` is sample label, empty for overall
  - `{condition}` is the comparison operator, one of `>`, `<`, `>=`, `<=`, `=`, `==` (same as `=`)
  - `threshold` is the value to compare with, some KPIs allows percentage thresholds
  - `{logic}` is the way value aggregated withing timeframe, `for` means taking latest value, `within` means aggregating as average or sum (depends on criteria nature)
  - `timeframe` is number of seconds the comparison must be valid, if `timeframe` is omitted, then the cumulative value for whole test will be used for comparison.
  - `action` is one of `stop` or `continue`, default is `stop`, if you have chosen to continue, the fail status will be applied at the end of the test execution
  - `status` is one of `failed` (default) or `non-failed`

Any non-required parameters might be omitted, the minimal form is `subject{condition}threshold`. 

Possible subjects are:

 - `avg-rt` - average response time, e.g. `avg-rt>2s500ms`
 - `avg-lt`- average latency, e.g. `avg-lt for mylabel>2`
 - `avg-ct` - average connect time, e.g. `avg-ct>100ms`
 - `stdev-rt` - standard deviation for full response time, e.g. `stdev-rt>0.5`
 - `p...` - percentile timing, e.g. `p90>1s for 10s`, `p99.9>10s, stop as failed`
 - `hits` - number of responses, e.g. `hits for my-label>100 for 5s, stop as non-failed`
 - `succ` or `success` - successful responses, supports percentage threshold, e.g. `succ\<100%` 
 - `fail` or `failures` - failed responses, supports percentage threshold, e.g. `failures>50% for 5s, stop as failed`
 - `rc...` - response codes criteria, supports percentage threshold, response code may be specified using wildcards `?` and `\*`, e.g. `rc500>20 for 5s, stop as failed`, `rc4??>20%`, `rc\*>=10 for 1m`, `rcException>99% for 1m, continue as failed`, 


The full form of the criteria is conducted by Taurus automatically from short form. You can also specify it as this:

```yaml
---
services:
- module: passfail
  criterias:
  - subject: avg-rt  # required
    label: 'Sample Label'  # optional, default is ''
    condition: '>'  # required
    threshold: 150ms  # required
    timeframe: 10s  # optional, default is none
    logic: for  # optional, logic to aggregate values within timeframe. 
                # Default 'for' means take latest, 
                # 'within' means take sum/avg of all values within interval
    fail: true  # optional, default is true
    stop: true  # optional, default is true
```

### Custom Messages for Criteria

By default, Taurus uses criteria string to present it in messages. If you want to change the message, you can do one of:
 - set `message` field for full form of criteria
 - set message by prepending it to criteria string, like this: `My message: avg-rt>10s`
 - use dictionary instead of array to specify message and criteria, like this:
 
```yaml
---
services:
- module: passfail
  criterias:
    My Message: avg-rt of Sample Label>150ms for 10s, stop as failed
    Sample Label fails too much: fail of Sample Label>50% for 10s, stop as failed
```


## Shell Executor Service Module

Sample configuration:
```yaml
---
services:
- module: shellexec
  prepare:  
  - mkdir /tmp/test
  startup:
  - echo 1 > /tmp/test
  - echo 2 > /tmp/test
  shutdown:
  - cat /tmp/test2 
  post-process:
  - rm /tmp/test1
  - rm /tmp/test2
execution:
- scenario: tg1
  hold-for: 10s
scenarios:
  tg1:
    requests:
    - label: HTTP Request
      method: GET
      url: http://127.0.0.1/
```

Extended task configuration sample:
```yaml
---
services:
- module: shellexec
  prepare: # stage names: [prepare, startup, check]
  - command: echo 1 > /tmp/1.txt && sleep 1 && dmesg | grep pci  # task command
    background: true  # wait for task completion or send it to background, false by default. 
    ignore-failure: true  # false by default, otherwise will shutdown tests if command return code != 0, 
    out: taskout.txt  # set file name for task stdout, null to print to stdout
    err: taskerr.txt  # set file name for task stderr, null to print to stdout
    run-at: local  # provisioning level to limit command usage, null to run always
    cwd: artifacts-dir  # option to change working dir for command, null to not change it
                        # special value 'artifacts-dir' will change to taurus artifacts dir
    env:  # environment variables to set for command
      VAR1: val1
      VAR2: val2
```

Minimum task configuration sample:
```yaml
---
services:
- module: shellexec
  prepare: ls -la
  startup:
  - pwd
  - echo something
```
Notes:
 - Non-background tasks are not allowed on startup stage.
 - Background tasks will be shut down forcefully on mirror stages (see [Lifecycle](Lifecycle.md)) if they were not finished yet.
 - Background tasks on Check stage will not start until same previous task completed.
 - Special environment variable `TAURUS\_ARTIFACTS\_DIR` is set for every command, containing path to current artifacts directory
 - there is module setting `default-cwd` for `shellexec` module that allows to change `cwd` default value for all tasks
 - there is module setting `env` which contains dictionary for additional environment variables for commands

```yaml
---
modules:
  shellexec:
    default-cwd: /tmp
    env:
      VARNAME: value
      VARNAME2: value2
```
 
## Resource Monitoring Service

A frequest task for tests is to monitor target server's health. Monitoring service is built to collect data from those remote servers. At this time followind sources are supported:
 - local health stats, enabled by default
 - [ServerAgent](http://jmeter-plugins.org/wiki/PerfMonAgent/) - technology that is used by JMeter users for long time and
 - [Graphite](https://graphite.readthedocs.org/en/latest/).
Also you can use `local` monitoring (enabled by default) for check tester system state.

### Local Resource Stats

Following metrics are collected locally: 
- `cpu` - total CPU usage %
- `mem` - total RAM usage %
- `bytes-sent`/`bytes-recv` - network transfer rate 
- `disk-read`/`disk-write` - disk I/O rate
- `disk-space` - % disk space used for artifacts storage
- `engine-loop` - Taurus "check loop" utilization, values higher than 1.0 means you should increase `settings.check-interval`

```yaml
---
services:
- module: monitoring
  local:
  - metrics:
    - cpu
    - disk-space
    - engine-loop
```
  
### ServerAgent
 
Shortly, you need to unzip and launch small Java server on each of your target servers and then specify [metrics](http://jmeter-plugins.org/wiki/PerfMonMetrics/) to collect under `services` item. For example: 
```yaml
---
services:
- module: monitoring
  server-agent:
  - address: 127.0.0.1:4444
    metrics:
    - cpu
    - disks
    - memory

```
### Graphite 

Graphite data source uses graphite The Render URL API to receive metrics.
In this example you can see usage optional server `label`, `timeout` for graphite answers, `interval` between requests and interesting graphite data range definition with parameters `from`/`until`.
```yaml
---
services:
- module: monitoring
  graphite:
  - address: 192.168.0.38
    interval: 5s
    from: 100s
    until: 1s
    timeout: 2s
    metrics:
    - store.memUsage
    - test.param1
  - address: local_serv:2222
    label: test_serv
    metrics:
    - production.hardware.cpuUsage
    - groupByNode(myserv_comp_org.cpu.?.cpu.*.value, 4, 'avg')
``` 

### Sidebar Widget

Once you have resource monitoring enabled, you'll be presented with small sidebar widget that informs you on latest data from monitoring agents:

[](monitoring-widget.png)

The widget will possibly not display all the metrics for the long list, that's the limitation of screen height :)
 
### Monitoring-Based Failure Criteria 

Once you have working resource collecting process, you can use special failure criteria based on data from target servers. Most of parameters for criteria are same like in other [fail criterias](#Pass-Fail-Criteria). You'll have to use full format for metric specification because of the need to specify metric class `bzt.modules.monitoring.MonitoringCriteria`. For example, to stop test once local CPU is exhausted, use:

```yaml
---
services:
- module: passfail
  criterias:
  - class: bzt.modules.monitoring.MonitoringCriteria
    subject: local/cpu
    condition: '>'
    threshold: 90
    timeframe: 5s
```

### Unpacker

You can ask to unzip some of your files into artifacts directory before test start (only zip format are supported). It's easy with `unpacker` service:
   
```yaml
---
services:
- module: unpacker
  files:
  - c:\tmp.zip
  - /home/user/temp.zip
```  
 