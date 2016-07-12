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
Taurus provides hooks to all Taurus [text execution phases](Lifecycle.md).

Sample configuration:
```yaml
---
services:
- module: shellexec
  prepare:  
  - mkdir /tmp/test
  startup:
  - echo 'started' >> /tmp/test/log
  shutdown:
  - echo 'shutdown' >> /tmp/test/log 
  post-process:
  - rm /tmp/test/log
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
 
Learn more about `shellexec` service [here](ShellExec.md).
 
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

You can learn more about Monitoring Service at its [page](Monitoring.md)

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
 
