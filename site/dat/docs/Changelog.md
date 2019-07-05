# Changelog

## 1.13.9<sup>next</sup>


## 1.13.8<sup>29 may 2019</sup>

- add random CSV feeder to JMX generator
- expand on-the-fly convertion of soapui to all executors
- fix Gatling classpath in generated launcher
- add provisioning capacity feature
- add `bytes` passfail criteria
- add support of new Node.js (8+)
- fix Gatling csv handling
- fix Jmeter load defaults
- fix cumulative time stats on dashboard
- fix site deploying flow


## 1.13.7<sup>29 may 2019</sup>

- update default gatling version to 3.1.2
- set iteration default for Jmeter into 1
- fix gatling props encoding
- fix locust aggregation bug
- fix order of capabilities assembling
- filter out `included-configs` from cloud YAML
- fix site styles


## 1.13.6<sup>6 may 2019</sup>

- fix issues of pip package for previous version
- default empty concurrency in JMeter JMX as 1
- handle JMeter install check failures properly
- fix OSX mouse wheel binding, document dashboard scaling
- fix Gatling getting `u` prefix to simulation name (and other properties)
- handle quitting event from locust to ignore restrictions on teardown


## 1.13.5<sup>30 apr 2019</sup>

- default `iterations` are now `1` as opposed to `infinity` in the past 
- add `env` option reading for each execution
- set special env vars for ShellExec commands, containing Taurus exit code
- fix TST having 0 as start value in case test is very long
- add check and warning for Java version used with Gatling
- add jartool options (version, path, download_link)
- don't pass null address to Apiritif
- share webdriver instance between test methods of Selenium script
- store browser in lowercase
- send capabilities to webdriver as is
- check capabilities for 'browser' option
- avoid explicit RemoteDriver request
- roll back Gatling script changes for Windows


## 1.13.4<sup>31 mar 2019</sup>

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
- support keystore configuration for jmeter executor in YAML
- allow spaces to be used in Gatling properties


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
