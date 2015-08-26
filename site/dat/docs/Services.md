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
    prepare:  # stage names: [prepare, startup, check]
    - command: /folder/file | grep anything  # task definition
      ignore-failure: False
    - sleep 1  # second task definition (shorthand)
    startup:
     - command: echo startup stage
       background: True
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
    prepare:
    - command: echo 1 > /tmp/1.txt && sleep 1 && dmesg | grep pci  # task command
      background: true  # wait for task completion or continue, false by default. 
      ignore-failure: true  # true by default, otherwise will shutdown tests if command return code != 0, 
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
 - Background tasks will be shut down forcefully on opposite stages (see [Lifecycle](Lifecycle.md)) if they were not finished yet.
 - Background tasks on Check stage will not start until same previous task completed.