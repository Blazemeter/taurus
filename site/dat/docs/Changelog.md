# Changelog

## 1.6.3 (next)
 - fix percentile value handling in passfail criteria
 - fix problem with sending included configs into cloud
 - fix cumulative fail criteria processing order
 - fix passfail criteria with no last datapoint available

## 1.6.2 <sup>8 jun 2016</sup>
 - fix passfail-related regression crash

## 1.6.1 <sup>1 jun 2016</sup>
 - add `run-at` option for unpacker module

## 1.6.0 <sup>31 may 2016</sup>
 - add [logic blocks](JMeter.md#Logic-Blocks) to `scenario.requests` syntax for JMeter
 - add `default-location` option for cloud provisioning
 - delete test files before uploading new ones to the cloud
 - add `delete-test-files` option to cloud provisioning
 - fix reading piped config from stdin
 - don't trap KeyboardInterrupt in tool install
 - remove default xmx set for JMeter
 - add zipping folders treatment for remote execution
 - add check for resources overlap (data loss danger) for remote execution
 - add raise for JSON body without corresponding header
 - cleanup config from null values before sending it to the cloud
 - pull cumulative percentiles from cloud provisioning
 - replace 'criterias' with 'criteria', add backward compatibility
 - fix double blazemeter reporting in cloud
 - make ui type configurable with `screen` option of console reporter
 - fix console reporter crash under Windows when curses is installed
 - add subject setting to regexp extractor (similar to assertions)
 - send some monitoring data into BlazeMeter reporting service
 - do not clear cloud test files when using Blazemeter reporting

## 1.5.1 <sup>30 may 2016</sup>
 - fix JMeter 3.0 installation issues
 - make JMeter 3.0 the default installed version
 - fix downloading older JMeter versions from Apache archives

## 1.5.0 <sup>4 may 2016</sup>
 - add [Tsung](Tsung.md) executor
 - support Gatling 2.2.0
 - fix Gatling `download-link` option handling
 - fix `browser-open` regression
 - allow CLI overrides to be arbitrary YAML values
 - make log widget in console reporter smaller to leave more space for sidebar widgets
 - order sidebar widgets by their priority
 - fix "junit xml reporter + passfail + monitoring" bug 

## 1.4.4 <sup>25 apr 2016</sup>
 - fix enhanced PBench schedule generation crash on Python 3
 - ensure that `script` option for PBench is looked at scenario level
 - do not CWD into artifacts directory when running PBench
 - fix PBench script lookup when using cloud/remote provisioning
 - do not change CWD when running JMeter
 - add forgotten Gatling script template to python egg
 - fix PassFail condition flaw with few datapoints
 - explicitly write default values in jmx2yaml
 - recognize JMX variables in jmx2yaml
 - don't fail execution because of web browser
 - fix schedule size estimation and progress reporting in PBench
 - fix PBench schedule reading crashes in Python3
 - fix empty jxm error listener when write-xml-jtl=none  

## 1.4.3 <sup>14 apr 2016</sup>
 - bump up version for jmeter plugins installation to 1.4.0
 - `javac` presence check fixed for selenium
 - deeper fix detection of resource files for full-form `data-sources` items
 
## 1.4.2 <sup>11 apr 2016</sup>
 - fix detection of resource files for full-form `data-sources` items
 - fix `body-file` and `data-sources` not being detected in cloud environment

## 1.4.1 <sup>7 apr 2016</sup>
 - improve slave id analysis for JMeter distributed test
 - do not append extra \r\n to files sent to cloud/remote prov

## 1.4.0 <sup>5 apr 2016</sup>
 - add XPath extractors and assertions for JMeter
 - show warning if no element matched `set-prop` modification
 - do not create hostaliases file when aliases are not specified
 - add `force-parent-sample` option to JMeter executor
 - add `compile-target-java` option for Selenium
 - add dynamic buffer scaling ability to ResultsReader
 - add scheduling ability with `start-at` parameter  
 - apply overrides and cli-aliases before creating artifacts-dir
 - fix multiple JMeter warnings when CSV delimiter isn't set 
 - add `memory-xmx` option to JMeter to configure JVM heap size 

## 1.3.3 <sup>24 mar 2016</sup>
 - add new `hostaliases` setting for all executors
 - add delay capability to engine 

## 1.3.2 <sup>23 mar 2016</sup>
 - fix lowercase hostname for JMeter HTTP request
 - fix binary varname crash for JMeter HTTP request 

## 1.3.1 <sup>22 mar 2016</sup>
 - fix JMeter crash when `data-sources` value is not a list
 - fix non-integer port in HTTP request
 - fix Selenium crash with when using cloud/remote provisioning

## 1.3.0 <sup>16 mar 2016</sup>
 - add [Gatling](Gatling.md) script generation 
 - fix [Gatling](Gatling.md) metric parsing
 - remove explicitly cwd setting for [Gatling](Gatling.md)
 - add detection of [Gatling](Gatling.md) simulation request in case of several simulations are found
 - set unique output dir for every [Gatling](Gatling.md) execution
 - add output buffer size control to [Gatling](Gatling.md) executor 
 - fix [Grinder](Grinder.md) crash
 - join [JMeter](JMeter.md) and its plugins installation procedures
 - fix unicode handling in [JMeter](JMeter.md)'s jmx script
 - add [Apache Benchmark](ApacheBenchmark.md) executor
 - extend script path recognition for [JMeter](JMeter.md)
 - fix [PBench](PBench.md) not working with cloud provisioning
 - fix schedule generation in original [PBench](PBench.md)

## 1.2.0 <sup>19 feb 16</sup>
 - maximize browser window in Selenium test, when possible
 - add graphite support to monitoring service
 - add local monitoring service
 - create [Docker](Docker.md) image with Taurus inside
 - one virtual display for all selenium executions
 - add link for cloud provisioning results into JUnit xml
 - add interface between Taurus and Gatling test script (scala)
 - fix selenium resource files list for cloud
 - fix forced delimiter detection for JMeter (failed for single-column csv)  

## 1.1.0 <sup>11 jan 16</sup>
 - support `iterations` and `hold-for` options for Selenium
 - add concurrency-driven load support for PBench
 - add `-locations` command alias to dump available locations for Cloud tests
 - support variables in time fields for JMeter
 - support POST body in application/json format for JMeter
 - ability to set JMeter log verbosity with `write-xml-jtl`

## 1.0.0 <sup>4 jan 16</sup>
 - support [cloud](Cloud.md) provisioning
 - parse URLs for JMeter a bit better
 - add `Siege` executor
 - add command line option `-n` to skip loading system and user configs

----

[Changelog for Year 2015](Changelog2015.md)
