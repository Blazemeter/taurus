# Changelog

## 1.15.4<sup> 06 Jul 2021</sup>
- add support of advanced browser options
- add `apiritif` external action handlers
- add `apiritif` graceful shutdown feature
- improve `jmeter` thread group conversion  
- fix iteration calculation in `apiritif`
- add option to pass cmd line to some executors
- implement `vegeta` auto installation
- update `k6` installation process in docker
- added error message that Shadow DOM slots are not supported yet
- fix tools stopping logic
- fix final stats table in bzt.log
- add code coverage check to travis


## 1.15.3<sup> 15 Apr 2021</sup>
- add support of `k6` and `vegeta` tools
- add support of `MQTT` protocol to jmeter executor
- add `Plugins-Manager` and `Command-Runner` configuration
- update gatling support to current (3.4+) version
- update molotov to current (2.1) version
- migrate from `elfinder` Cloud API
- fix selenium wait exception when ignored alert
- fix scripEval action issue (curly brackets in param)


## 1.15.2<sup> 21 Jan 2021</sup>
 - add support for client side certificates
 - add option to exclude ramp-up from cumulative stats
 - Docker image optimization
 - remove support of Yandex.Phantom tool
 - write interpreter and packages details into log
 - add waiter for page load after selected Apiritif actions
 - update chrome driver version
 - update junit version
 - avoid taurus crash due to psutil
 - fix when non-latin characters console bug
 - Improve docs (Apiritif, installation, etc.)

[Changelog for Year 2020](Changelog2020.md)