# Pass/Fail Criteria

Every load test you run should have pass/fail status. There is a way to set this status in Taurus, based on runtime criteria. Special `passfail` module offers this functionality. 

Another useful feature of pass/fail module
is _auto-stop_ functionality, allowing to interrupt failed tests automatically, sparing the time and
resources.

Pass/fail criteria are specified as array of `criteria`, set through `reporting` item in config:
```yaml
reporting:
- module: passfail
  criteria:
  - avg-rt of IndexPage>150ms for 10s, stop as failed
  - fail of CheckoutPage>50% for 10s, stop as failed
```

The above example use short form for criteria, its general format is
`subject of label{condition}threshold {logic} timeframe, action as status`, where:

  - `subject` is the KPI that will be compared, listed below
  - `label` is sample label, empty for overall
  - `{condition}` is the comparison operator, one of `>`, `\<`, `>=`, `\<=`, `=`, `==` (same as `=`)
  - `threshold` is the value to compare with, some KPIs allow percentage thresholds
  - `{logic}` is the way value aggregated within timeframe, see more details [below](#Timeframe-Logic)
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


## Timeframe Logic 

If no timeframe logic is present, the pass/fail rule is processed at the very end of test, against total aggregate data. 
To apply checks in the middle of the test, please use one of possible timeframe logics:

- `for` means each value inside timeframe has to trigger the condition, for example `avg-rt>1s for 5s` means each of consecutive 5 seconds has average response time greater that 1 second
- `within` means all values inside timeframe gets aggregated as average or sum (depends on KPI nature), then comparison is made
- `over` is very similar to `within`, but the comparison is made only if full timeframe available (`within` will trigger even if partial timeframe matches the criteria)

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

## Per-Executor Criteria

Failure criteria you add under `reporting` section is global. In case you use multiple `execution` items, aggregate data from all of them is used to trigger criteria. To attach criteria only to certain execution items or scenarios, use same syntax of `criteria`:

```yaml
execution:
- scenario: s1
  criteria:  # this is per-execution criteria list
  - fail>0%
  - rt>1s
- scenario: s2

scenarios:
  s1:
    script: file1.jmx
  s2:
    script: file2.jmx
    criteria:  # this is per-scenario criteria list
    - fail>50%   
    - p90>250ms   
```
 

## Monitoring-Based Failure Criteria 

You can use special failure criteria based on [monitoring data](Monitoring) from target servers. Most of
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

## Internal Criteria Representation 

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
                # 'within' and 'over' means take sum/avg of all values within interval
    fail: true  # optional, default is true
    stop: true  # optional, default is true
```

