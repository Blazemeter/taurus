# Changelog

## 1.15.1<sup> 30 Oct 2020</sup>
- update C# tools to use .NET Core 3.1
- add `xUnit` executor
- add contextClick action to `Apiritif` executor
- improve debug features (snapshot version, interpreter info)
- fix switching windows by index `selenium` action
- fix `locust` run-time `concurrency` info
- fix `gatling` launch problem
- fix `selenium` desired capabilities format
- fix colors in windows console

## 1.15.0<sup> 03 Sep 2020</sup>
- allow to use new Apiritif external logging feature
- support variables in the Apiritif loop
- implemented support for parent context in foreach loop for Apiritif tests
- support some loop fields for Apiritif
- support Lightning Components using Shadow locator
- allow to set up Firefox proxy in Selenium test
- add `pip-installer` service
- support RTE Jmeter plugin
- support jsr223 processor at the scenario level
- improve Apiritif/Selenium Data Sources:
    - detect quoting
    - support encoding
    - add `tab` synonym as delimiter
- support PassFail criteria in cloud
- improve environment variables resolving
- move to modern Locust tool
- move to up-to-date Chrome Driver
- support Python v3.8 on Linux/Mac
- allow to specify where exactly Shellexec must be processed
- fix resource files gathering in once block for Cloud Provisioning
- improve Cloud test status reporting
- update pyvirtualdisplay dependency
- fix frames/windows switching in selenium tests
- fix `assertDialog` action treating
- fix get_locator behavior for selenium tests
- fix dialog replace call
- fix locust jtl errors
- fix usage statistics order on gettaurus.org

- improve docs:
    - describe artifact directory (folder for test results & logs)
    - describe custom python packages install process on Windows
    
## 1.14.2<sup> 12 Apr 2020</sup>
- implement new apritif actions:
  - `assertEval` and `storeEval`
  - `answerDialog` and `assertDialog`
  - `if`, `then`, `else` conditions
  - simple `loop`
  - `waitFor`
  - `foreach` loop
- add `csv-jtl-flags` option for tuning of logging verbosity
- fix resolving variables inside locators when using alternative multi locators syntax
- fix bza api calls for private cloud
- fix `virtual display` doc
- fix exit flow on windows
- fix result files encoding
- fix video link and https access on site

## 1.14.1<sup> 12 Feb 2020</sup>
- add alert windows handling to apiritif
- add user test type feature for Cloud Provisioning
- dump graphite and serveragent data
- update default version of jmeter tool to 5.2.1
- use 0 as infinity iterations limit in apiritif
- fix calculation of standard deviation
- fix bzt-pip entry point
- fix dashboard bug on Windows
- remove double timeout from selenium test generation
- remove nose executor as deprecated
- documentation fixes

[Changelog for Year 2019](Changelog2019.md)