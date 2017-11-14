# Changelog

## 1.10.1 <sup>next</sup>
 - add [Postman/Newman](Postman.md) executor 
 - add support for [Robot Framework](Robot.md) as `robot` executor
 - add support for [WebdriverIO](WebdriverIO.md) as `wdio` executor
 - add support for API load testing with `nose` executor
 - add `-public` CLI alias to make BlazeMeter report public
 - fix crash on reading Gatling logs with assertions
 - fix Gatling reading same results twice for multiple executions 
 - send `java-opts` from Gatling settings into BlazeMeter cloud test
 - do not force "mode=Stripped" property for JMeter distributed tests
 - fix file uploads for JMeter and GET/PUT methods
 - don't crash `jmx2yaml` on unparsable JSON bodies
 - trying to fix exception write into log for Grinder
 - support `apiritif` transactions in `pytest` executor

## 1.10.0 <sup>26 oct 2017</sup>
 - fully migrate to python wheel distribution
 - provide Homebrew recipe for OSX users to install Taurus easily
 - add URL shorthands testing to `bzt` CLI, allowing config-less quick-test
 - handle "many executions in one location" cloud test properly
 - allow a fractional hatch-rate when using `locustio` as executor.
 - use chromedriver 2.33 by default
 - use geckodriver 0.19 by default
 - use selenium 3.6 for Java and inside Docker
 - don't use `window_maximize` in generated python script due to problems with virtual display
 - download 64-bit Geckodriver for 64-bit Windows
 - use `-r -l` options for `ab` executor, make its exit code to not fail test
 - add JMX path to `jmeter.classpath` property
 - fix property parsing for cases where JMeter expression (but not property access) is used inside YAML
 - add gatling reports prefix to `dir_prefix` setting
 - ensure executor startup logging is written into log file
 - fix elementwise merging in dicts for some edge cases
 - fix some file descriptor leaks
 - fix installation with old `pip` versions 
 - optimize BlazeMeter API interaction by querying only test's workspace locations
 - convert Groovy scripts in SoapUI to JMeter's JSR223 blocks
 - fix issue with JMeter not handling loop controllers properly

## 1.9.6 <sup>27 sep 2017</sup>
 - add `pytest` executor type
 - add [Molotov](https://github.com/loads/molotov)-based executor type: `molotov`
 - support `-cloud -func` combination for launching cloud functional tests
 - use Gatling 2.3.0 by default
 - use JMeter 3.3 by default
 - use plugins-manager for JMeter v0.16
 - report `<TestDuration>` as part of `final-stats` XML
 - allow using JMeter properties in load parameter specifications
 - fix `varables` option of `data-sources` in JMeter
 - use "install-for-jmx" feature of plugins manager
 - change approach to retrieve Grinder label-to-ID mapping
 - fix Grinder on python 3 not working
 - fix Grinder `HTTP 0` error and `DivisionByZero` error
 - fix passfail criteria parsing to work with float percentiles
 - fix dashboard encoding problems on Windows
 - call BZA session shutdown only if we were sending data into it

## 1.9.5 <sup>11 aug 2017</sup>
 - add `nunit` executor for NUnit-based tests (Selenium and others)
 - add executor self-diagnostics in case of failure (output STDOUT/STDERR and log files into Taurus log)
 - force Concurrency Thread Group for JMeter when possible
 - allow specifying manual cookies for JMeter tests
 - add JAR files from `files` into JMeter classpath
 - allow JMeter headers to be set as modification to existing JMX
 - use apiritif transactions in generated selenium scripts
 - install chromedriver and geckodriver automatically for Selenium tests
 - load errors info from BlazeMeter when using cloud provisioning
 - rearrange JUnit XML error reporting attributes
 - enable PBench to record microsecond values and BlazeMeter uploader to report them
 - eliminate potential race condition in Mocha plugin
 - `settings.verbose` switches verbosity on for CLI
 - bump up pmgr to 0.15
 - optimize Grinder kpi log reading
 - add configuration prefix `$` for elementwise list merging
 - allow spaces around comparison operators in passfail criteria
 - add `junit-xml` support for functional mode
 - fix doublequoting error in JTL reader
 - recover from invalid characters in JTL files
 - fix CSV quotation crash in Locust module
 - fix NPE with junit runner and null script
 - fix RSpec functional mode tests
 - fix Selenium concurrency and VU count for cloud provisioning
 - fix test status handling for skipped tests in Nose plugin
 - add `python -m bzt` launcher

## 1.9.4 <sup>28 jun 2017</sup>
 - add `clearCookies` and `pauseFor` actions for Selenium YAML
 - add `data-sources` support to Gatling YAML scripting
 - add [TeamCity](/kb/TeamCity) article to KB
 - send more `blazemeter` module settings into cloud
 - TestNG: Recognize `setUp()` failures as test failures
 - fix browser with report not open on recent MacOS
 - fix implementation field issues with Cookie Manager of JMeter
 - fix crash `-v` used on Windows with nose executor
 - minor fixes around PBench executor
 - fix functional mode breaks on JMeter 2.13

## 1.9.3 <sup>2 jun 2017</sup>
 - fix failure with JMeter cookie manager and "null"
 - install JMeter into per-version directories
 - support `data-sources` for Gatling script generating
 - auto-replace tabs with spaces, trying to workaround user's config issues
 - create .bzt-rc file template if it not exists
 - support dedicated IPs feature for BlazeMeter Cloud tests
 - add transaction controller "parent sample flag" support
 - add binding to all local IPs into JMeter
 - make virtual display to be service, detach it from Selenium
 - fix jmx2yaml password masking issue
 - fix locust crash when empty host specified
 - fix proxy2jmx chrome loader binary in windows installer
 - fix Windows installer to ship Python egg within
 - added Bamboo article
 - Refactor Gatling script generation to generate nicely formatted scripts

## 1.9.2 <sup>14 may 2017</sup>
 - fix grinder having 100% errors
 - extract apiritif into standalone PyPi project
 - improve console message in case of BZA failed response
 - support some of JMeter functions when translating into apiritif code
 - enable apiritif log printing into console when `-v` is used
 - support apiritif transactions
 - improve grinder RC matching algorithm
 - improve jmx2yaml conversion with encoded params
 - read errors info from distributed locustio test
 - bump up auto-installed JMeter to 3.2
 - avoid recursion in dummy screen dashboard
 - allow specifying link to download Grinder
 - allow using URL field for `extract-regex` in JMeter scripts
 - support jmx2yaml conversion of JMeter's native JSONPath Post Processors
 - stdin config read removed for CLI (it was hanging)
 - hostaliases support removed (was never working properly)
 - for JMeter, write scenario name into thread group name
 - add simple JMX downloading for Proxy2JMX

## 1.9.1 <sup>19 apr 2017</sup>
 - fix errors in reading Grinder KPI file
 - fix race condition in updates check
 - catch possible error in psutil memory KPI getting

## 1.9.0 <sup>16 apr 2017</sup>
 - per-technology executors are extracted from selenium executor
 - experimental release of `apiritif` framework scripts
 - use BZA workspace's `enabled` flag to filter
 - don't install `10-base.json` into `/etc/bzt.d` as step towards wheel dist
 - proxy2jmx now uses new-style API client
 - fix handling samples with empty RC in console dashboard
 - fix for too many labels requested from BlazeMeter API
 - suppress warning of windows env variables merge
 - change Grinder to single process + threads model, change the way results are read
 - work with test name mapping in Grinder
 - fix locations error with BlazeMeter API

## 1.8.4 <sup>29 mar 2017</sup>
 - support new-style Blazemeter API keys
 - introduce `over` timeframe logic to passfail
 - include report URL into `final-stats` module's `dump-xml`
 - fix concurrency values for many thread groups, when values are very low
 - add `final-stats` alias to conform our naming standards
 - send some more config options to for cloud test
 - documentation fixes
 - fix Grinder working with multiple source files

## 1.8.3 <sup>17 mar 2017</sup>
 - fix broken on MacOS due to security restrictions
 - use Java 8 as default compile target for Java

## 1.8.2 <sup>16 mar 2017</sup>
 - support `follow-redirects` option for Gatling
 - add connections count to local monitoring, send it to BlazeMeter API also
 - fix JSON configs with extra characters support
 - fix BlazeMeter API usage
 - bump up Java Selenium to 3.3.0 (will require latest geckodriver installed)
 - bump up JMeter plugins manager to 0.12
 - grab more details for JMeter's functional samples

## 1.8.1 <sup>13 mar 2017</sup>
 - make `---` not required for YAML files anymore
 - enable YAML multi-doc files support, now `---` divides several configs per file (by YAML standard)
 - use less verbose logging in the middle of run, can be overridden with `-v` option
 - don't write bzt.log into current dir, use temp dir instead, then move to artifacts
 - support arrays as JSON body of requests
 - capture jmeter's stderr/stdout into artifact files
 - allow setting `report-name` for cloud tests
 - download HAR+Screenshots report automatically from the cloud
 - improve JSON body extraction for jmx2yaml tool, inline short JSR223's into config, convert BeanShell into JSR233
 - allow changing kpi.jtl delimiter through JMeter properties

## 1.8.0 <sup>25 feb 2017</sup>
 - Added a capability to specify SoapUI scripts for JMeter executor
 - add services to start/stop appium server and android emulator
 - fix `default-address` handling for multi-execution use of single scenario
 - Used ENTRYPOINT in docker to allow command-line parameters passing
 - bump up autoinstalled selenium version to 3.0.1
 - Added a converter utility `soapui2yaml` for converting SoapUI projects into Taurus configs
 - add selector by link text to selenium actions
 - Support for auto setup of proxy server under Microsoft Windows for jmx recording purposes.
 - Passfail made back to be reporter, since it cannot serve as service, reporters need to have actual pass/fail status earlier
 - add support for inlined JSR223 code for JMeter post-processors
 - allow post-processors specified for non-request actions like pauses
 - send Jmeter properties onto remote servers for distributed tests
 - add regexp flag to jmeter jsonpath assertion
 - support `variable-names` option for JMeter's `data-sources`
 - fix passfail `rc404>10 within 1m` logic
 - fix overriding concurrency in case thread groups have non-int values inside
 - fix JMeter's treatment of resource files for cloud tests that have paths relative to the JMX
 - fix crash on cloud test results handling caused by API changes
 - migrate to v4 API of BlazeMeter
 - report non-zero exit code if cloud test has errors


[Changelog for Year 2016](Changelog2016.md)
