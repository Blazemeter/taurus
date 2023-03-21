# Changelog

## 1.16.19<sup> 5 December 2022</sup>
- Vulnerability fixes
- Improvement for Selenium WebDrivers version handling

## 1.16.18<sup> 14 November 2022</sup>
- Removing Vegeta test executor as it's not maintained for 2+ years
- Added net-tools to Dockerfile to fix startup loop finding `netstat` command (thanks to community!)
- Correct typo on Locust error occurrences datapoint (thanks to community!)
- Change NodeJS 12->14, include apt-utils (#1690) (thanks to community!)
- DE23863 Fix double counting of requests in Locust executor (#1691)

## 1.16.17<sup> 1 November 2022</sup>
- Vulnerability fixes

## 1.16.15<sup> 19 October 2022</sup>
- Fixes for pass-fail validation

## 1.16.14<sup> 13 September 2022</sup>
- Fixes for VU calculation

## 1.16.12<sup> 30 August 2022</sup>
- Vulnerability fixes and various version upgrades

## 1.16.11<sup> 04 August 2022</sup>
- Taurus base image reverted to Ubuntu 21.10 as 22.04 LTS breaks too many things for now
- Vulnerability fixes in pip, pillow and other libraries

## 1.16.10<sup> 25 July 2022</sup>
- Taurus base image updated to Ubuntu 22.04 LTS

## 1.16.9<sup> 20 July 2022</sup>
- fix handling of BlazeMeter backend timeouts

## 1.16.8<sup> 22 June 2022</sup>
- Vulnerability fixes

## 1.16.7<sup> 09 June 2022</sup>
- Vulnerability fixes, Moved to Ubuntu 21.10 as the base image

## 1.16.6<sup> 07 June 2022</sup>
- Default selenium version is set to 4.1.3 to fix blazemeter grid issue

## 1.16.5<sup> 18 May 2022</sup>
- Concurrency issues fix

## 1.16.4<sup> 13 Apr 2022</sup>
- Make all ExecutorWidgets more informative (“executor_name: scenario”)
- Refactor concurrency calculations
- Add ‘concurrency’ option to taurus based on ‘pytest -n’ to split tests between several CPUs
- Scriptless defect fixes

## 1.16.3<sup> 27 Jan 2022</sup>
- add support of WebKitGTKOptions for remote safari
- preserve PYTHONPATH priority
- update remote browser logic and capabilities
- migrate to apiritif 1.1.1

[Changelog for Year 2021](Changelog2021.md)
