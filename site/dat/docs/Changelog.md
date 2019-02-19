# Changelog

## 1.13.3<sup>next</sup>

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
