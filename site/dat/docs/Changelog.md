# Changelog

## 1.9.3 <sup>upcoming</sup>
 - fix failure with JMeter cookie manager and "null"
 - create .bzt-rc file template if it not exists
 - support dedicated IPs feature for BlazeMeter Cloud tests
 - fix jmx2yaml password masking issue
 - fix locust crash when empty host specified
 - fix proxy2jmx chrome loader binary in windows installer
 - fix Windows installer to ship Python egg within
 - add transaction controller "parent sample flag" support
 - add binding to all local IPs into JMeter
 - install JMeter into per-version directories
 - auto-replace tabs with spaces, trying to workaround user's config issues
 - make virtual display to be service, detach it from Selenium
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
