# Services Subsystem

When you need to perform some actions before test starts, after test starts, or in parallel with
running test, you should use services subsystem in Taurus. To configure services, use `services`
top-level section of config with list of service module configs:

Services are configured with `services` toplevel section. `services` section contains a list of
services to run:
```yaml
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
- `shellexec` used to execute additional shell commands when test is executed
- `monitoring` allows including monitoring data in test reports
- `chrome-profiler` allows to extract performance metrics from Chrome running Selenium tests

## Shell Executor Service Module

Shell executor is used to perform additional shell commands at various test execution phases.
Taurus provides hooks to all Taurus [test execution phases](Lifecycle.md).

Sample configuration:
```yaml
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

## Virtual Display Service

If your tests open windows (e.g. Selenium tests) and you want to run them in a headless
environment, you can tell Taurus to run virtual display by using `virtual-display` service.

`virtual-display` is build on top of [Xvfb](https://www.x.org/archive/X11R7.6/doc/man/man1/Xvfb.1.xhtml),
so it's only available on Linux.

```yaml
services:
- module: virtual-display
  width: 1024
  height: 768
```

## Chrome Profiler Service

This service allows you to extract frontend performance stats from Chrome.

It calculates the following metrics:
- page load metrics (time to 'load' event, time to full page load, etc)
- network metrics (page footprint, time to first byte, number of HTTP requests, etc)
- memory metrics (how much memory was consumed by browser, tab, JavaScript engine)
- JavaScript CPU utilization
- all HTTP requests that were made by browser tab
- all AJAX requests
- JavaScript functions that browser spent most of the time executing

Service also comes with a reporter (named `chrome-metric-reporter`) that will print
performance metrics extracted from Chrome to terminal at the end of the test.

You can learn more about `chrome-profiler` service at its [own page](ChromeProfiler.md).

## Unpacker

You can ask Taurus to unzip some of your files into artifacts directory before test starts (only zip format is supported). It's easy with `unpacker` service:
   
```yaml
services:
- module: unpacker
  files:
  - c:\tmp.zip
  - /home/user/temp.zip
```  

## Alternate Provisionings and Services
If you use alternate provisionings, like [BlazeMeter Cloud](Cloud.md), you might want to specify where to run the service module - at target machine, or on your local machine. For that, use `run-at` option of service modules. The service will be effective only when its `run-at` value match to `provisioning` value. On the cloud images, `provisioning` always has value of `local`. Default value for `run-at` of services is also `local`.


## Required Tools Installer

There is small service which helps to check for all possible tools presence on the machine. It is called `install-checker` service. This service, once present in config, will check for all possible tools to be installed on the computer. After that, it will shutdown Taurus without running anything else. 

To invoke this service, just run Taurus like this `bzt -install-tools`. 

## Appium Loader

Useful for start/stop appium server automatically. This service doesn't provide installation ability so use [official documentation](http://appium.io) to get appium. You can specify path to appium through the appropriate setting:

```yaml
services:
- appium
modules:
  appium:
    path: 'path/to/appium/executable'
    timeout: 20     # timeout for appium startup
```

## Android Emulator Loader

It used to start/stop android emulator. For that purpose you have to get Android SDK by yourself and tell Taurus where it placed with path to emulator (usualy it can be found in <sdk_directory>/tools) in config or environment variable ANDROID_HOME, which contains SDK location. Moreover, you should choose one of your android emulators with `avd` option. 

```yaml
services:
- android-emulator
modules:
  android-emulator:
    path: /home/user/Android/sdk/tools/emulator
    avd: android10_arm128
    timeout: 20     # timeout for android emulator startup, adb should be available through the PATH for startup detection 
```    
