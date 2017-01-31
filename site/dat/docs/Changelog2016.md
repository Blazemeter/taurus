# Changelog for year 2016

## 1.7.5 <sup>29 dec 2016</sup>
 - add `actions` into selenium to perform clicks, type keys and wait for items
 - result processing optimization: add limitation of response time list size to cumulative KPISets
 - bump up auto-installed JMeter to 3.1 and pmgr to 0.11
 - add `public-report` option to BlazeMeter reporter and cloud provisioning
 - add `retrieve-resources-regex` scenario-level option to JMeter
 - allow controling JMeter HTTP redirects with `follow-redirects` option
 - alter `loop` block to JMX compilation scheme to better reflect JMeter defaults (kudos to @rogerbramon)
 - add labels, concurrency and error messages to reporting from Grinder
 - add `throughput` ability to Gatling when building scripts from YAML
 - fix Gatling path lookup for JARs from execution's `files`
 - error logging improvements
 - fix a few minor issues with the Windows installer
 - fix public cloud reporting (unauthorised notes sending)
 - add test for paths of resource files
 - fix the crash when multiple BM reporters are in use
 - add warning message for concurrency setting in Selenium config
 - disable console if stdout isn't a tty (no need to do `modules.console.disable=1` anymore)
 - add creating of new stringProp ability to jmx modifications
 - support unicode labels for aggregator's `ignore-labels` and JMeter

## 1.7.4 <sup>11 nov 2016</sup>
 - fix Locust crash when used with 'requests'-style scenario and cloud provisioning
 - fix JSR223 block handling when using cloud/remote provisioning
 - allow sequential executions with `-sequential` alias
 - do not generate Selenium scripts for cloud tests
 - add a tool installer service and an `-install-tools` CLI alias
 - fix cloud Gatling crash related to script template

## 1.7.3 <sup>1 nov 2016</sup>
 - add TestNG-based runner for Selenium tests
 - add `content-encoding` request- and scenario-level option to JMeter
 - send `additional-classpath` jars into cloud when preparing a cloud test
 - add the ability to specify path to Ruby binary
 - jmx2yaml: use `HTTPSampler.path` even when there's no `HTTPSampler.domain` set
 - make Taurus shortcut (Windows) open command line
 - fix assertion disappearance in nose_plugin
 - improve exception logging in Engine/CLI
 - fix unicode issues in jmx2yaml
 - handle the absence of backtrace in RSpec
 - fix cumulative part of BlazeMeter reporting
 - remove MirrorsManager from Gatling
 - add final stats reporting for functional mode
 - change routing part for BlazeMeter reports opened in browser
 - fix log verbosity for HTTPError
 - fix proxy2jmx proxy settings for Chrome 
 - fix issue when user has too many tests and test is not found in BZA
 - fix gatling adding jar to classpath mistake
 - bring ab, aggregator, console, gatling, grinder, chrome to own exceptions style
 - query tests list from BZA more efficiently
 - fix list item deleting via cli overrides
 - use vanilla Chrome in Docker image

## 1.7.2 <sup>13 oct 2016</sup>
 - fix keep-alive processing in Gatling
 - add ability of JMeter variables usage in data-sources path options
 - add `jsr223` blocks support for JMeter
 - fix problem with jmx twins 

## 1.7.1 <sup>10 oct 2016</sup>
 - fix Mocha runner race condition
 - support more scenario options for Grinder script generation
 - add move check for executor results availability to provisioning
 - add common `run-at` option for services
 - fix CPU monitoring when multiple monitoring modules are used
 - fix `timeout` handling in Locust script generator
 - fix Locust crash when `concurrency` isn't specified
 - support scenario-level `headers` in Locust script generator
 - add exception throw to tools downloader
 - add Mocha autoinstallation for Selenium tests
 - add `selenium-webdriver` npm package installation for Mocha runner
 - make `-locations` respect `use-deprecated-api` flag
 - fix relative path processing in find_file()
 - don't exclude executable nose scripts for Selenium 
 - add setup of global `additional-classpath` ability to SeleniumExecutor
 - support `keepalive` flag for Locust and Grinder
 - make assertion messages of nose shorter and more informative
 - track `bytes received` KPI and attach it to BZA report

## 1.7.0 <sup>2 oct 2016</sup>
 - add RSpec tests runner for Selenium
 - add Mocha-based tests runner for Selenium
 - introduce `language` option for Selenium tests
 - add `action` blocks to JMeter's requests
 - fix JAR building for selenium projects with several folders
 - migrate to new online reporting format
 - add LocustIO script generation feature
 - support collection-based cloud tests
 - do not copy Selenium scripts in artifacts dir before running them
 - fix KPI merging error
 - fix nose plugin crash when setUp/setUpClass raises an exception 
 - fix percentiles values for cloud-based tests
 - add stubs for `screenshoter` and `capturehar` services
 - fix artifact upload in the cloud
 - Docker image now has all executors pre-installed
 - introduce `check-interval` option for cloud provisioning
 - rename generated Grinder script to avoid clashes with Locust

## 1.6.8 <sup>1 sep 2016</sup>
 - fix hamcrest installation for Java-based Selenium tests
 - fix Jmeter fail with unusual jmx property type
 - add variable sending example into Gatling documentation
 - fix results time and add widget for ApacheBenchmark
 - add `script: <jar-file>` capability to Gatling executor
 - handle JMeter functions in `default-address` option

## 1.6.7 <sup>23 aug 2016</sup>
 - add `cloud` and `local` aliases 
 - add `detach` mode to Cloud Provisioning
 - add worker id to cloud log file names
 - fix delay trouble in provisioning
 - fix Gatling keepalive type cast bug
 - fix grinder in the cloud with requests scenario
 - add `xml-jtl-flags` option for tuning of logging verbosity
 - add `waiting for data...` into titles of console blocks
 - fix SSL error handling for cloud provisioning
 - add Chrome profiler [service](ChromeProfiler.md) and reporter
 - add webdriver.log for requests-based Selenium tests
 - make Selenim test labels clearer

## 1.6.6 <sup>08 aug 2016</sup>
 - optimize aggregator by removing excessive calls to `BetterDict.get()`
 - use JMeter Plugins Manager for installation of plugins
 - build installer for Windows to simplify installation process
 - add Proxy2JMX service to convert tests from Selenium to JMeter format
 - fix occasional crash from inconsistent API result for cloud test KPIs
 - prefer user-supplied `download-link` over default download link for JMeter

## 1.6.5 <sup>12 jul 2016</sup>
 - fix master_id choosing for cloud reporting 
 - fix non-graceful shutdown on GUI window close
 - restructure reporting and services docs
 - do not crash when attempting to open browser in browserless env
 - update Gatling script sample in docs
 - solve slow post-processing by making reading speed of kpi.jtl adaptive

## 1.6.4 <sup>05 jul 2016</sup>
 - add short script syntax (`<scenario>: \<script>`)
 - rely on Gatling simulation auto-detection mechanism when `simulation` field is not set
 - fix 'non-existent scenario' error case
 - fix config cleanup in cloud provisioning
 - fix Selenium crash when used in multi-execution with a shared scenario
 - add `default-address` scenario option to Selenium
 - add custom gatling launcher for jar usage ability
 - fix gatling path fail on Windows
 - automatically rename Selenium Python script if it has undiscoverable name
 - fix null global headers failure
 - add `upload-files` option to JMeter requests for multipart/form-data uploads
 - `loop: false` in JMeter data source now means 'stop thread after CSV is exhausted'
 - force str for env vars in all shellexecs

## 1.6.3 <sup>17 jun 2016</sup>
 - fix percentile value handling in passfail criteria
 - add setting up of CWD on server side
 - fix problem with sending included configs into cloud
 - fix cumulative fail criteria processing order
 - limit the amount of monitoring data BlazeMeter uploader accumulates
 - fix passfail criteria with no last datapoint available
 - force str for env vars in all shellexecs

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
