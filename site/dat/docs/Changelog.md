# Changelog

## 1.5.0 (next)
 - add [Tsung](Tsung.md) executor
 - support Gatling 2.2.0
 - fix Gatling `download-link` option handling
 - add Tsung support for Mac OS (and for non-standard installations of Tsung)
 - fix `browser-open` regression
 - allow CLI overrides to be arbitrary YAML values
 - make log widget in console reporter smaller to leave more space for sidebar widgets
 - order sidebar widgets by their priority

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

# Year 2015
## 0.5.2 <sup>17 dec 15</sup>
 - fix JMeter installation on windows

## 0.5.1 <sup>11/16/15</sup>
 - fix shellexec env variables
 - add `assume-success` flag for JMeter assertions
 - flexibly handle '$1$' templates for JMeter regex extractor
 - fix null body parameter for JMeter

## 0.5.0 <sup>11/16/2015</sup>
 - allow using Xvfb for Selenium executor
 - don't use lynx browser when opening report links
 - add PBench executor type
 - show scenario alias in sidebar widget for JMeter
 - set `TAURUS\_ARTIFACTS\_DIR` environment variables for shellexec commands
 - add `cwd` option for shellexec tasks
 - add `env` option for shellexec tasks

## 0.4.5 <sup>22 oct 2015</sup>
 - load settings applied to Stepping Thread Groups
 - shellexec service: print to stdout/stderr when `out`/`err` options set to `null`
 - JMeter: append `user.properties` instead of overriding
 - move pass/fail to services
 - set BlazeMeter session note with test error message if present

## 0.4.4 <sup>22 sep 2015</sup>
 - limit max size of file that goes into zip for artifact upload (max-size option)
 - fix cumulative KPIset recalculating
 - JMeter, Grinder, Gatling, JUnit now use mirrors during installation
 - files from Listeners now excluded from resources
 
## 0.4.3 <sup>17 sep 2015</sup>
 - allow passing config as stdin to CLI like `./generate-config.sh | bzt`
 - allow having `report-name=ask` for BlazeMeter reporter
 - selenium test runners now using xml jtl for errors
 - console graph max height resets on data scroll
 - resource monitoring service implemented

## 0.4.2 <sup>11 sep 2015</sup>
 - bump up Gatling version to 2.1.7
 - selenium script from requests format
 - change file search algo to use paths relative to config locations
 - allow having script files relative to config locations
 - allow having `included-configs` instruction
 - allow setting path to artifacts from `settings` in config, deprecate `--data-dir` CLI option

## 0.4.1 <sup>9 sep 2015</sup>
 - fix Locust.io search paths
 - `generalize-labels` default value changed to `false`
 - fix JMeter properties not read from jmeter files
 - force JMeter to use epoch timestamp format for CSV files
 - Allow setting project ID instead of name, fail on project name clash
 - parameterize JMeter graceful shutdown time

## 0.4.0 <sup>31 aug 2015</sup>
 - allow dumping final stats in Jenkins-consumable format
 - implemented graceful shutdown for JMeter
 - for sample that failed because of embedded resources, actual error message used instead of "OK"
 - support Locust.io load generator
 - allow setting the `report-name` for blazemeter report
 - allow easy setting report/test/project options for blazemeter module

## 0.3.8 <sup>26 aug 2015</sup>
  - fixed bug when old jars were not removed during JMeter installation
  - add `project` to BlazeMeter report config, allowing to sort tests by projects
  - allow `message` for pass/fail criteria to improve readability
  - implement "services" top-level config section
  - implemented shellhook service

## 0.3.7 <sup>13 aug 2015</sup>
  - fail criteria without timeframe is checked at the end of the test
  - fixed shutdown on windows
  - fixed label names in junitxml reports
  - blazemeter report url added to every testcase in junitxml reports

## 0.3.6 <sup>13 aug 2015</sup>
  - added jmx2yaml tool
  - added updates check capability

## 0.3.5 <sup>16 jul 2015</sup>
  - fix Selenium executor logging
  - added CSS/JQuery extractor support in scenarios generated from requests option.
  - fixed JMeter installation path issue one more time

## 0.3.4 <sup>16 jul 2015</sup>
  - fixed JMeter path bug on windows
  
## 0.3.3 <sup>16 jul 2015</sup>
  - fixed tools check/installation on windows
  - fixed resource files gathering issue

## 0.3.2 <sup>14 jul 2015</sup>
  - use progressbar for download progress indicators
  - fix issue with unicode XML on Mac
  - use JTL format in Selenium results

## 0.3.1 <sup>7 jul 2015</sup>
  - fixed python3 installation issue (progressbar33 now used instead of progressbar)

## 0.3.0 <sup>6 jul 2015</sup>
  - implement `selenium` executor
  - fix crashing on second start jmeter under GUI mode
  - iterate requests forever if no limits were specified
  - fix test duration logic to respect iterations set
  - distributed tests are now supported with JMeter UI (`gui: true` option)
  - install JMeter-Plugins 1.3.0
  - all default tool locations are now under `~/.bzt` dir
  - FinalStatus reporter now provides test duration (test-duration option, True by default)
  - bzt now fails when no requests were given in requests scenario
  - six module version requirements was removed

## 0.2.23 <sup>6 jul 2015</sup>
  - rename `bulk-size` option into `send-interval` for BlazeMeter reporter
  - explicitly fail in case of wrong `body` option for HTTP request
  - fixed bug when JMeter test duration was not applied properly.

## 0.2.22 <sup>6 jul 2015</sup>
  - send data to BlazeMeter less frequently (30 secs)
  - added ability to access BZA feeding through proxy
  - fixed bug with modifying paths of resource files in distribute test mode
  
## 0.2.21 <sup>6 jul 2015</sup>
  - if `iterations` set, then duration for test will not be limited
  - added `steps` option to execution settings
  
## 0.2.20 <sup>5 jul 2015</sup>
  - add `within` logic to timeframed pass-fail criterias
  - added `use-dns-cache-mgr` option.
  - default-domain option renamed to default-address (scheme, hostname, port are now parsed from this option).
  
## 0.2.19 <sup>6 jul 2015</sup>
  - fixed bug when in distributed tests VU count was not calculated properly.
  - auto-append `${__machineName()}` for thread names in distributed test
  - fix module search path issue on MacOS

## 0.2.18 <sup>21 may 2015</sup>
  - set "clear each iteration" flag for cache and cookie managers when generating JMX from requests
  - allow wildcards in enable-disable modifications  

## 0.2.17 <sup>15 may 2015</sup>
  - added ability to change font size in Windows dashboard GUI on Ctrl + mousewheel event
  - reworked CSV reading for JMeter to support quoted data

## 0.2.16 <sup>14 may 2015</sup>
  - fix base config not copied because of broken imports
  - display console screen in separate window on windows

## 0.2.15 <sup>13 may 2015</sup>
  - replace digits and UUID sequences with N and U to decrease label count
  - fix not working `bzt 1.jmx 2.jmx 3.jmx`

## 0.2.14 <sup>13 may 2015</sup>
  - added support for user defined variables
  - fix reading for non-standard errors JTL

## 0.2.13 <sup>12 may 2015</sup>
  - Some more stats have been added to console screen
  - add `-gui` command-line alias to open JMeter UI for debugging
  - add support for JMeter distributed mode
  
## 0.2.12 <sup>5 may 2015</sup>
  - Added http request defaults options
  - Added support of RPS shaper component
  - Remove conflicting JAR libraries during JMeter installation procedure
  - Fixed bug when resource files were not properly copied to artifacts directory

## 0.2.11 <sup>5 may 2015</sup>
  - Base config fix on Windows and minor changes in setup.py

## 0.2.8 <sup>29 apr 2015</sup>
  - Fix base config not found on Windows with multiple disks
  - Added proper version of lxml in requirements

## 0.2.1 <sup>28 apr 2015</sup>
  - Added pass/fail criteria report on console reporter

## 0.2.0 <sup>15 apr 2015</sup>
  - Added JSON path assertion.
  - Added parameters for final_stats reporter
  - Added ability to generate query string based on parameters and request type.
