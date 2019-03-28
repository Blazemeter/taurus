# Changelog

## 1.13.4<sup>next</sup>

- bump up default JMeter version to 5.1.1
- for external results loader, allow specifying files under scenario
- fix external results loader hanging on small result files
- save some RAM if result readers have read enough data into buffers
- support `set-variables` action for Apiritif executor
- support CSV reading in Apiritif executor
- print stderr/stdout for failed subprocesses in tool checks
- walk into YAML transaction blocks for all executors
- for Thread Groups that have 0 concurrency, disable them
- for Apiritif threads, handle variable scope properly
- change `ignore-labels` behavior to prefix instead of exact match
- change the way detailed error info matched to datapoints, for future bug catch
- read more of error.jtl in single pass
- add url parameters support for JMeter
- fix 'None' iterations written into JMeter CTG
- remember rolling concurrency to avoid fuzziness in multi-executor case
- use HTTPS to check for version upgrade needs
- YAML syntax: support uniform, gaussian and poisson think-times for JMeter
- filter `token` and some other options from config when running `-cloud`
- make Java presence checks softer
- fix env vars not passed to Plugins Manager


## 1.13.3<sup>24 feb 2019</sup>

- use `safe_` version of YAML functions because of CVE-2017-18342
- add response message to assertion error in JMeter, controlled by flag
- copy TG properties from existing JMX into modified: thread delay, scheduler delay, iterations limit
- add `rawCode` action type to Python YAML scripting
- add generator of Gatling 3.X launcher
- support reading groups for Gatling 3.x
- fix broken GROUP reading from Gatling log
- for Gatling, parse `but actually XXX is not less than or equal to YYY` errors into RC
- allow specifying pass/fail criteria per-execution
- fix `ab` launching options for older versions of httpd-tools
- better cleanup for config sent to cloud from `-cloud` runs
- fix variable interpolation in Selenium scripts
- don't overwrite selenium runner class inside executor, it caused bugs
- run shellexec tasks with unified runner, don't allow PIPE output for background tasks to avoid hanging
- write exception details into log, when class not found in Java Selenium
- handle failed assertions and exceptions in Locust tests
- add assertion results to report for Newman
- expanded NUnitRunner to write test context properties to NUnitExecutor.ldjson for parsing later on.  Any objects in the test context will be listed in under `extras.test_context`
- remove shared env object from internal fields
- ignore SKIPPED samples from functional executors, when in load mode
- add `bzt-python` command to Taurus distribution for Windows
 
[Changelog for Year 2018](Changelog2018.md)
