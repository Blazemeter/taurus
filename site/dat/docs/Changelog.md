# Changelog

## 1.16.47<sup> 20 January 2026</sup>
- Playwright executor concurrency/duration fixes
- Gatling fix problem with string properties in yaml file
- Send test files to cloud in chunks (by 50)
- Dependency fixes (urllib3, npm, dotnet, junit, jmeter-plugins-manager)

## 1.16.46<sup> 16 September 2025</sup>
- K6 executor updated to 0.45.0
- Playwright executor added
- Support for latest Gatling 
- Dependency fixes (commons-lang3, commons-text, selenium, chromedriver, testng)
- Migrated to Ubuntu 24.04 and upgraded dependencies
- Removed custom vulnerability fixes for jmeter and gatling

## 1.16.45<sup> 21 July 2025</sup>
- Python has been upgraded to version 3.13.0
- Remove support for RSpec and WDIO

## 1.16.44<sup> 11 July 2025</sup>
- Urllib3 version update to 2.5.0
- 
## 1.16.43<sup> 10 July 2025</sup>
- Urllib3 version update to 2.4.0

## 1.16.42<sup> 14 May 2025</sup>
- Small fix in handling of huge xmls

## 1.16.41<sup> 28 April 2025</sup>
- Urwid dependency updated to 2.6.16

## 1.16.40<sup> 14 April 2025</sup>
- Newman executor re-added
- Newman executor fails test on newman exceptions
- Added support for _same user on next iteration_ in jmx

## 1.16.39<sup> 17 February 2025</sup>
- JMX parser enhancement
- Cloud documentation changes
- Update WDIO to 9.x and jinja2 to 3.1.5

## 1.16.38<sup> 9 January 2025</sup>
- Website update, fixed broken links

## 1.16.37<sup> 9 January 2025</sup>
- Website update, fixed broken links

## 1.16.36<sup> 9 January 2025</sup>
- Website update, fixed broken links
- Gatling sh script fix
- Vulnerability fixes
- Fix selenium webdriver.log

## 1.16.35<sup> 29 October 2024</sup>
- Upgrade to jmeter 5.6.3
- Upgrade to ruby 3.3.3

## 1.16.34<sup> 15 August 2024</sup>
- Fixed the execution of existing cloud tests
- Fixed the execution of TestNG Selenium tests

## 1.16.33<sup> 12 August 2024</sup>
- Fixed response haeaders extractor
- Fixed pagination in blazemeter api client
- Vulnerability fixes

## 1.16.32<sup> 25 June 2024</sup>
- Java helpers updated to 1.10 (Added support for JUnit's DisplayName annotation)
- Upgrade node to 18
- Limits error response bodies collection to 10 per test
- Modified RPS value to avoid 0.001 to reduce timeouts.
- Consume all passed ramp-up plan steps and execute last of them
- Properly handled cases where duration or steps are none
- Fixed ramp-up to start at the end of the increase interval, not the beginning
- Added coverage improvements
- Reverted changes that set concurrency at the end of the interval to match BlazeMeter UI
- Added the pyscripts module to the Taurus package
- Removed Newman executor from doc index

## 1.16.31<sup> 29 April 2024</sup>
- Fixed resolution of JMX test duration, that contains user variables.

## 1.16.30<sup> 16 April 2024</sup>
- Dependencies upgrade

## 1.16.29<sup> 26 February 2024</sup>
- Allow reports inside actions instead of requests
- Retrieve response content type from response headers
- Collect error response bodies
- Upgrade urllib to version 1.26.17

## 1.16.28<sup> 12 December 2023</sup>
- Gatling support for response extraction for regex, json, css, xpath
- Gatling support for include-scenario similar to support offered for Jmeter
- Gatling support to capture response point of interest using regex, json, css, xpath and use in downstream calls using ${param} conventions for substitution
- Gatling support for multi hosts when requests are chained by capturing responses and downstreaming from one host to another hosts - typical scenario authentication for auth fulfillment server provides a valid authentication token for use as follow up request to another host

## 1.16.27<sup> 22 September 2023</sup>
- Fix k6 aggregator
- Allow using custom name for the csv cloud report

## 1.16.26<sup> 11 December 2023</sup>
- Cloud provisioning fixes
- Proxy auth encoding fix
- Urwid dependency version set to 2.1.2

## 1.16.25<sup> 22 August 2023</sup>
- Latest chromedriver support
- Cloud test csv report fix
- Gatling version upgrade to 3.9.5

## 1.16.24<sup> 8 August 2023</sup>
- Selenium 4.10.0 support
- Upgrade to ruby 3.2.2
- Vulnerability fixes
- Small bugfixes

## 1.16.23<sup> 24 May 2023</sup>
- snakeyaml in Jmeter updated to 2.0 to address CVEs
- US47299 Upgrade .NET to 6.0 to fix vulnerabilities (#1721)
- Small bugfixes

## 1.16.22<sup> 31 March 2023</sup>
- #1706 Make influxdb application parameter optional
- Influxdb documentation fixes
- BlazeMeter websocket reporter
- Vulnerability fixes
- Allow user to use different syntax's for non-proxy variables

## 1.16.21<sup> 14 March 2023</sup>
- Added influxdb-reporter (thanks to community!)

## 1.16.20<sup> 13 March 2023</sup>
- Upgrade to Ubuntu:22.04 as base image
- Selenium >=4.1.4 support
- Vulnerability fixes
- Support applying HTTP Proxy settings from env. variables
- Fix jQuery extraction (thanks to community!)
- Add support for jmeter-grpc-request plugin. (thanks to community!)


[Changelog for Year 2022](Changelog2022.md)
