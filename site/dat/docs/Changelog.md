# Changelog

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
