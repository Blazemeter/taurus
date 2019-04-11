# ShellExec Service

This service module is used to call arbitrary commands during test execution stages.

Sample configuration:
```yaml
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
 - Special environment variables `TAURUS\_EXIT\_CODE` and `TAURUS\_STOPPING\_REASON` is set for every command after Taurus begins shutdown, containing exit code and possible failure explanation
 - There is module setting `default-cwd` for `shellexec` module that allows to change `cwd` default value for all tasks
 - There is module setting `env` which contains dictionary for additional environment variables for commands

```yaml
modules:
  shellexec:
    default-cwd: /tmp
    env:
      VARNAME: value
      VARNAME2: value2
```
