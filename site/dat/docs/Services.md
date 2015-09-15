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
      out: taskout.txt  # set file name for task stdout
      err: taskerr.txt  # set file name for task stderr
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
 
 - service
 
### Sidebar Widget
 - widget
 
### Monitoring-Based Failure Criteria 
 - fail criteria