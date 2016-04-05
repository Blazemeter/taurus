# Reporting & Results Processing

Reporting functionality in Taurus is delegated to special modules category. There is special [internal facility](#results-reading-and-aggregating-facility) that reads results from [executors](ExecutionSettings.md), aggregates them and feeds to configured reporters. Reporters are specified as list under top-level config key `reporting`, by default it is configured with two reporters:

```yaml
---
reporting:
- final_stats
- console
```

The example above uses a shorthand form for specifying reporters. Full form is using dictionaries and allows specifying some additional settings for reporters:

```yaml
---
reporting:
- module: final_stats
- module: console
```

Possible reporting modules are listed below.

## Post-Test Summary

This is the simplest reporter that just prints few basic KPIs in the console log after test execution, for example:

```
18:04:24 INFO: Samples count: 367, 8.17% failures
18:04:24 INFO: Average times: total 0.385, latency 0.115, connect 0.000
18:04:24 INFO: Percentile   0.0%: 0.125
18:04:24 INFO: Percentile  50.0%: 0.130
18:04:24 INFO: Percentile  90.0%: 1.168
18:04:24 INFO: Percentile  95.0%: 1.946
18:04:24 INFO: Percentile  99.0%: 2.131
18:04:24 INFO: Percentile  99.9%: 3.641
18:04:24 INFO: Percentile 100.0%: 3.641
```

This reporter is enabled by default. To enable it manually, use following config, some additional options are available:

```yaml
---
reporting:
- module: final_stats
  summary: true  # overall samples count and percent of failures
  percentiles: true  # display average times and percentiles
  failed-labels: false  # provides list of sample labels with failures
  test-duration: true  # provides test duration
  dump-xml: filename to export data in XML format
  dump-csv: filename to export data in CSV format
```

### Dump Summary for Jenkins Plot Plugin

Two options `dump-csv` and `dump-xml` allows to export final cumulative stats into files that can be used by [Jenkins Plot Plugin](https://wiki.jenkins-ci.org/display/JENKINS/Plot+Plugin) to plot historical data inside Jenkins. Prefer CSV as it is much easier to use with Plot Plugin. XML format also can be used with other tools to automate results processing.

Field names with explanations:

 -   `label` - is the sample group for which this CSV line presents the stats. Empty label means total of all labels
 -   `concurrency` - average number of Virtual Users
 -   `throughput` - total count of all samples
 -   `succ` - total count of not-failed samples   
 -   `fail`  - total count of saved samples
 -   `avg_rt` - average response time  
 -   `stdev_rt` - standard deviation of response time  
 -   `avg_ct` - average connect time if present
 -   `avg_lt`  - average latency if present 
 -   `rc_200` - counts for specific response codes
 -   `perc_0.0` .. `perc_100.0` - percentile levels for response time, 0 is also minimum response time, 100 is maximum

## Console Screen

This reporter shows fullscreen dashboard with some KPIs and even ASCII-art graphs like this:

![Console Screen](console-rsz.png)

This reporter is enabled by default. To enable it manually, use following config:

```yaml
---
reporting:
- console
```

There is module settings for Console Screen, containing option `disable`. It allows easy disabling fullscreen display by using [command-line](CommandLine.md) switch `-o`:

```bash
bzt config.yml -o modules.console.disable=true
```

On Windows, Console Screen is shown in separate window and users may change font size by holding Ctrl key and using mouse wheel.
Two additional options are `dummy-cols` and `dummy-rows`, they affect the size of _dummy_ screen that is used for non-tty output.


## BlazeMeter.com Reporting Service

Like it always happens with tools that focused on _executing_ tests, they are unable to provide sufficient reporting functionality. As professional user, you need some centralized storage to be able to access test results in convenient and interactive way, compare different executions, see trends over time and collaborate with your colleagues. [BlazeMeter.com](http://blazemeter.com) offers such service, it has both commercial and free of charge versions. 

![BlazeMeter Report](blazemeter-rsz.png)

### Anonymous Usage

The simplest way to get a taste of BlazeMeter reporting is to use `-report` command-line switch. This will enable result feeding to service without any other settings required. You will receive the link for your report in the console text, and the link will be automatically opened in your default browser, see `browser-open` option for more tuning.

The official policy for BlazeMeter reports uploaded from Taurus, is that anonymous reports are kept for 7 days and if you're using your own account, then reports are kept according to the retention policy of your account. For details see BlazeMeter service [website](https://blazemeter.com/). 

### Personalized Usage

If you want the results to be stored in your existing BlazeMeter account, you'll need to specify the reporting settings in your configuration file. Get the API token from BlazeMeter.com (find it under your [Settings => API Key](https://a.blazemeter.com/app/#settings/api-key)) and put it into `token` option:

```yaml
---
modules:
  blazemeter:
    token: TDknBxu0hmVnJ7NqtG2F
```

It is highly recommended to place the token setting in your personal [per-user config](CommandLine.md#configuration-files-processing) `~/.bzt-rc` to prevent it from being logged and collected in artifacts.

Now you can use `-report` command-line switch, or you can set BlazeMeter reporting as part of your config, the `test` option specifies test name to use, `project` names group of tests:

```yaml
---
reporting:
- module: blazemeter
  report-name: Jenkins Build 1
  test: Taurus Demo
  project: Taurus Tests Group
```

Advanced settings:

```yaml
---
modules:
  blazemeter:
    address: https://a.blazemeter.com  # reporting service address
    data-address: https://data.blazemeter.com  # data service address
    browser-open: start  # auto-open the report in browser, 
                         # can be "start", "end", "both", "none"
    send-interval: 30s   # send data each n-th second
    timeout: 5s  # connect and request timeout for BlazeMeter API
    artifact-upload-size-limit: 5  # limit max size of file (in megabytes) 
                                   # that goes into zip for artifact upload, 10 by default
    
    # following instructions will have effect when no per-reporter settings
    report-name: My Next Test  # if you will use value 'ask', it will ask it from command line
    test: Taurus Test
    project: My Local Tests
```

Note how easy is to set report settings from command line, i.e. from inside Jenkins build step:
```bash
bzt mytest.yml -o modules.blazemeter.report-name="Jenkins Build ${BUILD_NUMBER}"
```

## BlazeMeter Sense Reporting
It is possible to integrate Taurus with [BlazeMeter Sense](https://sense.blazemeter.com/). BlazeMeter Sense is the
service for storing and analysing performance test results. It provides highly detailed and interactive graphs and
reports.

Example of usage:
```yaml
execution:
- concurrency: 10
  scenario:
    requests:
      - http://blazedemo.com/

reporting:
- sense

modules:
  sense:
    token: <Sense upload token>
```

Full Sense configuration:
```yaml
modules:
  sense:
    token: <Sense upload token>
    address: https://sense.blazemeter.com/
    project: Taurus  # name of Sense project
    test-title: Sense Test  # name of Sense test
    test-color: blue  # test color label in Sense UI
    online-enabled: true  # send live reports to Sense as test is executing
    browser-open: true  # open browser with live test data
```



## JUnit XML Reporter

This reporter provides test results in JUnit xml format parsable by Jenkins [JUnit Plugin](https://wiki.jenkins-ci.org/display/JENKINS/JUnit+Plugin).
Reporter has two options:
- `filename` (full path to report file, optional. By default `xunit.xml` in artifacts dir)
- `data-source` (which data source to use: `sample-labels` or `pass-fail`)

If `sample-labels` used as source data, report will contain urls with test errors.
If `pass-fail` used as source data, report will contain Pass/Fail criterias information.

Sample configuration:

```yaml
---
reporting:
- module: junit-xml
  filename: /path_to_file/file.xml
  data-source: pass-fail
```


## Results Reading and Aggregating Facility

Aggregating facility module is set through general settings, by default it is: 

```yaml
---
settings:
  aggregator: consolidator
```

The `consolidator` has several settings:

```yaml
---
modules:
  consolidator:
    generalize-labels: false  # replace digits and UUID sequences 
                              # with N and U to decrease label count
    ignore-labels: # sample labels from this list 
      - ignore     # will be ignored by results reader
      
    buffer-multiplier: 0.5  # choose middle value from following percentiles list (95.0)
    buffer-scale-choice: 2  # make buffer two times bigger than need to receive 95% samples      
    min-buffer-len: 2s      # minimal length of buffer (default: 2s)
    max-buffer-len: 2h      # maximal length of buffer (default: infinity)
        
    percentiles:  # percentile levels to track, 
                  # 0 also means min, 100 also means max 
    - 0.0
    - 50.0
    - 90.0
    - 95.0
    - 99.0
    - 99.9
    - 100.0
```


