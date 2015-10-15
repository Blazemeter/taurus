# Services Subsystem

When you need to perform some actions before test starts, after test starts, or in parallel with running test, you should use services subsystem in Taurus. To configure services, use `services` top-level section of config with list of service module configs:

```yaml
---
services:
  - shellexec:
      prepare: ...
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
 
 
## Resource Monitoring Service

A frequest task for tests is to monitor target server's health. Monitoring service is built to collect data from those remote servers. It relies on [ServerAgent](http://jmeter-plugins.org/wiki/PerfMonAgent/) technology that is used by JMeter users for long time. 
 
Shortly, you need to unzip and launch small Java server on each of your target servers and then specify [metrics](http://jmeter-plugins.org/wiki/PerfMonMetrics/) to collect under `services` item. For example: 

```yaml
---
services:
  - module: monitoring
    server-agents:
      127.0.0.1:4444:
        metrics:
          - cpu
          - disks
          - memory
      application server:
        address: myserv.target.com 
        metrics:
          - cpu
          - disks
          - memory
``` 

### Sidebar Widget

Once you have resource monitoring enabled, you'll be presented with small sidebar widget that informs you on latest data from monitoring agents:

[](monitoring-widget.png)

The widget will possibly not display all the metrics for the long list, that's the limitation of screen height :)
 
### Monitoring-Based Failure Criteria 

Once you have working resource collecting process, you can use special failure criteria based on data from target servers. Most of parameters for criteria are same like in other [fail criterias](Reporting.md/#Pass-Fail-Criteria). You'll have to use full format for metric specification because of the need to specify metric class `bzt.modules.monitoring.MonitoringCriteria`. For example, to stop test once local CPU is exhausted, use:

```yaml
reporting:
  - module: fail-criteria
    criterias:
      - class: bzt.modules.monitoring.MonitoringCriteria
        subject: 127.0.0.1/cpu:idle
        condition: '<'
        threshold: 5
        timeframe: 5s
```
