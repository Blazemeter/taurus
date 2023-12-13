# Changelog
## 1.16.28<sup> 12 December 2023</sup>
- Gatling support for response extraction for regex, json, css, xpath
- Gatling support for include-scenario similar to support offered for Jmeter
- Gatling support to capture response point of interest using regex, json, css, xpath and use in downstream calls using ${param} conventions for substitution
- Gatling support for multi hosts when requests are chained by capturing responses and downstreaming from one host to another hosts - typical scenario authentication for auth fulfillment server provides a valid authentication token for use as follow request to another host

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
