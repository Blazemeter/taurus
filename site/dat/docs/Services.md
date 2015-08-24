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
  shellexec:
    prepare:  # stage names: [prepare, startup, check, shutdown, post-process]
    - command: 'pwd'  # task definition
      label: sample task
    - command: 'pwd'  # second task definition
      label: sample task label
    shutdown:
    - label: sleep  # task definition
      command: sleep 1
    - command: pwd > /tmp/pwd.txt
```

Full task configuration sample:
```yaml
---
- command: echo 1 > /tmp/1.txt && sleep 1 && echo "done" > /tmp/done.txt && dmesg | grep pci  # task command
  label: sample task  # task label, if not set, command will be used as label
  block: true  # wait for task completion or continue, default false. Blocking tasks are not allowed on startup stage.
  stop-stage: prepare  # stage name to shutdown task, post-process by default
  stop-on-fail: false  # shutdown tests if command return code != 0
  out: /tmp/taskout.txt  # set file name for task stdout
  err: /tmp/taskerr.txt  # set file name for task stderr
```

Minimum task configuration sample:
```yaml
---
- command: 'ls -la'
```
Nonblocking tasks will be shut down forcefully on "stop-stage" if they were not finished yet.