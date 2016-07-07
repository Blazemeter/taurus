# Services Subsystem

When you need to perform some actions before test starts, after test starts, or in parallel with
running test, you should use services subsystem in Taurus. To configure services, use `services`
top-level section of config with list of service module configs:

Services are configured with `services` toplevel section. `services` section contains a list of
services to run:
```yaml
---
services:
- module: shellexec:
  post-process: ...
- module: monitoring
  server-agent:
  - address: 127.0.0.1:4444
    metrics:
    - cpu
    - disks
    - memory
```

Taurus provides the following services:
- `passfail` allows you to set test status based on runtime criteria
- `shellexec` used to execute additional shell commands when test is executed
- `monitoring` allows including monitoring data in test reports

## Pass/Fail Service

Pass/Fail Service is used to dynamically update test status based on some runtime criteria. For
example, you can use it to automatically fail the test when response time exceeds some limit.
Here's a sample:

```yaml
---
services:
- module: passfail
  criteria:
  - avg-rt of IndexPage>150ms for 10s, stop as failed
  - fail of CheckoutPage>50% for 10s, stop as failed
```

You can learn more about Pass/Fail Service at its [page](PassFail.md)

## Shell Executor Service Module

Shell executor is used to perform additional shell commands at various test execution phases.
Taurus provides the following hooks to bind your shell commands to:
- `prepare`, when test is configured and all test data is prepared
- `startup`, when load test begins
- `check`, every `check-interval` seconds after test is started and before it's finished
- `shutdown`, when load test ends
- `post-process`, when all tests ended and reports are generated

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
 - There is module setting `default-cwd` for `shellexec` module that allows to change `cwd` default value for all tasks
 - There is module setting `env` which contains dictionary for additional environment variables for commands

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

It may be useful to attach monitoring data from both servers and load generators in the test
report for further analysis. You can achieve that with `monitoring` service.
Here's a quick example.

```yaml
services:
- module: monitoring
  server-agent:  # collect data from remote server which has ServerAgent running
  - address: 192.168.1.3:4444
    metrics:
    - cpu
    - disks
    - memory
```

You can learn more about Monitoring Service at its [page](PassFail.md)

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
 
