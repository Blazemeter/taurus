# Pass/Fail Criteria

Every execution has pass/fail status and there is a way to set this status based on runtime criteria.
Special `passfail` module offers this functionality. Another useful feature of pass/fail criteria
is _auto-stop_ functionality, allowing to interrupt failed tests automatically, sparing the time and
resources.

Pass/fail criteria are specified as array of `criteria`, set through `reporting` item in config:
```yaml
reporting:
- module: passfail
  criteria:
  - avg-rt of IndexPage>150ms for 10s, stop as failed
  - fail of CheckoutPage>50% for 10s, stop as failed

scenarios:
  simple:
    requests:
    - label: IndexPage
      url: http://blazedemo.com/
    - label: CheckoutPage
      url: http://blazedemo.com/checkout.php

execution:
  scenario: simple
  hold-for: 1m
```

The above example use short form for criteria, its general format is
`subject of label{condition}threshold {logic} timeframe, action as status`, where:

  - `subject` is the KPI that will be compared, listed below
  - `label` is sample label, empty for overall
  - `{condition}` is the comparison operator, one of `>`, `<`, `>=`, `<=`, `=`, `==` (same as `=`)
  - `threshold` is the value to compare with, some KPIs allow percentage thresholds
  - `{logic}` is the way value aggregated withing timeframe, `for` means taking latest value, `within` means aggregating as average or sum (depends on criteria nature)
  - `timeframe` is number of seconds the comparison must be valid; if `timeframe` is omitted, then the cumulative value for whole test will be used for comparison.
  - `action` is one of `stop` or `continue`, default is `stop`, if you have chosen to continue, the fail status will be applied at the end of the test execution
  - `status` is one of `failed` (default) or `non-failed`.

Any non-required parameters might be omitted, the minimal form is `subject{condition}threshold`. 

Possible subjects are:
 - `avg-rt` - average response time, e.g. `avg-rt>2s500ms`
 - `avg-lt`- average latency, e.g. `avg-lt for mylabel>2`
 - `avg-ct` - average connect time, e.g. `avg-ct>100ms`
 - `stdev-rt` - standard deviation for full response time, e.g. `stdev-rt>0.5`
 - `p...` - percentile timing, e.g. `p90>1s for 10s`, `p99.9>10s, stop as failed`
 - `hits` - number of responses, e.g. `hits for my-label>100 for 5s, stop as non-failed`
 - `succ` or `success` - successful responses, supports percentage threshold, e.g. `succ\<100%` 
 - `fail` or `failures` - failed responses, supports percentage threshold, e.g. `failures>50% for 5s, stop as failed`
 - `rc...` - response codes criteria, supports percentage threshold, response code may be specified using wildcards `?` and `\*`, e.g. `rc500>20 for 5s, stop as failed`, `rc4??>20%`, `rc\*>=10 for 1m`, `rcException>99% for 1m, continue as failed`, 


The full form of the criteria is conducted by Taurus automatically from short form. You can also
specify it as this:

```yaml
reporting:
- module: passfail
  criteria:
  - subject: avg-rt  # required
    label: 'Sample Label'  # optional, default is ''
    condition: '>'  # required
    threshold: 150ms  # required
    timeframe: 10s  # optional, default is none
    logic: for  # optional, logic to aggregate values within timeframe. 
                # Default 'for' means take latest, 
                # 'within' means take sum/avg of all values within interval
    fail: true  # optional, default is true
    stop: true  # optional, default is true
```

## Custom Messages for Criteria

By default, Taurus uses criteria string to present it in messages. If you want
to change the message, you can do one of:
 - set `message` field for full form of criteria
 - set message by prepending it to criteria string, like this: `My message: avg-rt>10s`
 - use dictionary instead of array to specify message and criteria, like this:
 
```yaml
reporting:
- module: passfail
  criteria:
    My Message: avg-rt of Sample Label>150ms for 10s, stop as failed
    Sample Label fails too much: fail of Sample Label>50% for 10s, stop as failed
```

## Monitoring-Based Failure Criteria 

You can use special failure criteria based on monitoring data from target servers. Most of
parameters for criteria are same like in other fail criteria. You'll have to use full format
for metric specification because of the need to specify metric class `bzt.modules.monitoring.MonitoringCriteria`.
For example, to stop test once local CPU is exhausted, use:

```yaml
reporting:
- module: passfail
  criteria:
  - class: bzt.modules.monitoring.MonitoringCriteria
    subject: local/cpu
    condition: '>'
    threshold: 90
    timeframe: 5s
```
