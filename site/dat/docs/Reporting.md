# Generating Test Reports

Reporting functionality in Taurus is delegated to special modules category. There is special
[internal facility](#results-reading-and-aggregating-facility) that reads results from
[executors](ExecutionSettings.md), aggregates them and feeds to configured reporters.
Reporters are specified as list under top-level config key `reporting`, by default it is
configured with two reporters:

```yaml
reporting:
- final-stats
- console
```

The example above uses a shorthand form for specifying reporters. Full form is using dictionaries
and allows specifying some additional settings for reporters:

```yaml
reporting:
- module: final-stats
- module: console
```

Taurus provides the following reporter modules:
- `console`, that shows live test stats in your terminal
- `blazemeter`, that provides interactive online test reports
- `final\_stats`, that provides post-test summary stats
- `junit-xml`, that generates test stats in JUnit-compatible format
- `influxdb-reporter`, that generates test stats to an influxdb datasource

## Console Reporter

The `console` reporter provides a nice in-terminal dashboard with live test stats and is enabled by default.
You can read more about it on its own [page](ConsoleReporter.md).

## BlazeMeter Reporter

The `blazemeter` reporter uploads all tests stats into [BlazeMeter.com](https://blazemeter.com) service,
which provides a nice UI to store and analyze test results. You can enable it with `-report` command
line option or by adding `blazemeter` item to `reporting` section of your config.

You can learn more about BlazeMeter reporter on its own [page](BlazemeterReporter.md).

## Final Stats Reporter

This simple reporter just prints a few basic KPIs in the console log after test execution,
for example:
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

This reporter is enabled by default. You can tweak its behaviour with the following options:

```yaml
reporting:
- module: final-stats
  summary: true  # overall samples count and percent of failures
  percentiles: true  # display average times and percentiles
  summary-labels: false # provides list of sample labels, status, percentage of completed, avg time and errors
  failed-labels: false  # provides list of sample labels with failures
  test-duration: true  # provides test duration
  dump-xml: filename to export data in XML format
  dump-csv: filename to export data in CSV format
```

### Dump Summary for Jenkins Plugins

Two options `dump-xml` and `dump-csv` allows exporting final cumulative stats into files that can be used
by  [Jenkins Performance Plugin](https://plugins.jenkins.io/performance/) and [Jenkins Plot Plugin](https://plugins.jenkins.io/plot/) to plot historical data
inside Jenkins. CSV to use with Plot Plugin (you can read good [article](https://www.blazemeter.com/blog/how-run-taurus-jenkins-performance-plugin) about it). XML format is for Performance Plugin.

Field names with explanations:
 -   `label` - is the sample group for which this CSV line presents the stats. Empty label means total of all labels
 -   `concurrency` - average number of Virtual Users
 -   `throughput` - total count of all samples
 -   `succ` - total count of not-failed samples
 -   `fail`  - total count of saved samples
 -   `avg\_rt` - average response time
 -   `stdev\_rt` - standard deviation of response time
 -   `avg\_ct` - average connect time if present
 -   `avg\_lt`  - average latency if present 
 -   `rc\_200` - counts for specific response codes
 -   `perc\_0.0` .. `perc\_100.0` - percentile levels for response time, 0 is also minimum response time, 100 is maximum
 -   `bytes` - total download size

## JUnit XML Reporter

This reporter provides test results in JUnit XML format can be parsed by Jenkins [JUnit Plugin](https://plugins.jenkins.io/junit/).
Reporter has two options:
- `filename` (full path to report file, optional. By default `xunit.xml` in artifacts dir)
- `data-source` (which data source to use: `sample-labels` or `pass-fail`)

If `sample-labels` used as source data, report will contain urls with test errors.
If `pass-fail` used as source data, report will contain [Pass/Fail](PassFail.md) criteria information. Please note that you have to place pass-fail module in reporters list, before junit-xml module.

Sample configuration:

```yaml
reporting:
- module: junit-xml
  filename: /path_to_file/file.xml
  data-source: pass-fail
```

## Influxdb Reporter

Influxdb Reporter is a Taurus extension which send stats to an influxdb (1.X) datasource.
The plugin has several settings:

```yaml
    modules:
      influxdb-reporter:
        host: localhost # influxdb address (default: localhost)
        port: 8086 # port (default: 8086)
        database: jmeter # the datasource name
        measurement: taurus # the measurement name
        username: ... # username if influxdb authentication enabled
        password: ...  # password if influxdb authentication enabled
        summary: true  # overall samples count and percent of failures
        percentiles: true  # send average times and percentiles
        send-data: true # enable/disable the sending
        send-interval: 30s   # send data each n-th second
        applications: myapp  # Optional - will attach tag 'application'
```

Influxdb reporting capabilities need to be explicitly enabled in the reporting section of the configuration, for example like this:

```yaml
    reporting:
    - final-stats
    - console
    - influxdb-reporter
```

### Tags and measurement

Influxdb is a time series database based on the following key concept:
* measurement : key represents the series used to store data. By default, _measurement_ is set to _jmeter_. You can override it.
* tags : provides a dimensional projection to measurement.

Here is an example of datapoint

```json
    {
     "time": 1666575154,
     "measurement": "jmeter",
     "fields": {
         "countError": 21
     },
     "tags": {
         "application": "myapp",
         "transaction": "all",
         "status": "all"
     }
}
```

> As you can see the time attribute is a timestamp. To perform write, the reporter use 'seconds' as epoch

Tags allow you to define criteria to filter datapoint. The plugin used 3 common tags:
* application: the application name. the name of System Under Test specified in _application_ property. (Optional)
* transaction: the sample (e.g the request url or label ). For aggregated metrics _status_ is set to _all_
* status: all, ok, ko. For aggregated metrics _status_ is set to _all_

## Results Reading and Aggregating Facility

Aggregating facility module is set through general settings, by default
it is: 

```yaml
settings:
  aggregator: consolidator
```

The `consolidator` has several settings:

```yaml
modules:
  consolidator:
    generalize-labels: 500    # support up to this number of labels
    ignore-labels: # sample labels starting with prefixes from this list 
      - ignore     # will be ignored by results reader (e.g. ignore_myrequest)
      
    buffer-scale-choice: 0.5  # choose middle value from following percentiles list (95.0)
    buffer-multiplier: 2  # make buffer two times bigger than need to receive 95% samples
    min-buffer-len: 2s      # minimal length of buffer (default: 2s)
    max-buffer-len: 2h      # maximal length of buffer (default: infinity)
    
    histogram-initial: 5s         # starting size of histograms to use, before auto-grow (default: 5s)  
    max-error-variety: 100  # max count of different error messages accepted (default: 100)
        
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

### Label and Error Folding

To handle a lot of test data correctly and efficiently, Taurus performs a few tricks when calculates statistics.

For example, the number of requests/labels Taurus can handle is capped by `generalize-labels` option (100 by default).
Similar labels can be folded into a single one. For example, if the number of labels is high,
`http://blazedemo.com/?foo=bar` and `http://blazedemo.com/?foo=baz` will be folded together and treated as two requests with the same label. This label merging only happens when you have more than 1/4 of `generalize-labels` utilized.

The sample folding mechanics also apply to test errors. Similar errors are folded together, and the upper limit of errors can be set with `max-error-variety` option.

To completely disable folding of labels or errors, you can set `generalize-labels` (or `max-error-variety`) to 0.
Disabled folding makes Taurus consume more memory and CPU for tests with lots of labels, so be prepared.
 
## Pass/Fail Criteria Subsystem
 
 Pass/Fail module is used to dynamically update test status based on some runtime criteria. For
 example, you can use it to automatically fail the test when response time exceeds some threshold.
 Here's a sample:
 
```yaml
reporting:
- module: passfail
  criteria:
  - avg-rt of IndexPage>150ms for 10s, stop as failed
  - fail of CheckoutPage>50% for 10s, stop as failed
```
 
You can learn more about Pass/Fail criteria capabilities at its [page](PassFail.md).
 
