# Changelog

## 1.12.1<sup>next</sup>

- set `LC_ALL` in Docker image so JVM's `file.encoding` property will be UTF-8
- support SSL certificates for proxies with `settings.proxy.ssl-cert` option
- limit max variety for error messages with `max-error-variety` option
- add Authorization Manager support for JMeter YAML scripting
- add Once Only Controller support for JMeter YAML scripting
- use special JMeter plugin for `set-variables`
- do not crash if tool reported negative response time, warn the user about it and proceed
- fix empty results read handling for Apiritif tests
- Fix JMeter installation under proxy
- remove whole disabled node (and also its hashTree, i.e. included elements)
- support conversion of variable parametrized LoopController
- improve check of conversion controllers (supported/unsupported, disabled, included, etc.)
- fix `AssertionError: monitoring` error for cloud tests



## 1.12.0<sup>4 jun 2018</sup>
- new frame management support was incorporated. Frames by id, name, relative parent and relative top (Selenium IDE and SideeX style)
- drag and drop support added to Nose Request (Selenium)
- `go` command added to Nose Request (Selenium)
- storeTitle, storeString, storeTextBy*, storeTitle and storeValueBy* added to Nose Request (Selenium)
- type command added to Nose Request (Selenium)
- summary Labels added to Final Stats Reporting module 
- built-in new support module for generated selenium script called selenium_taurus_extras
- services enhancing the Selenium executor can now be notified of test iteration progress
- variable support added to Selenium (Nose)
- new window management support was incorporated for selectWindow and closeWindow. Windows by name or sequential name incorporated (Selenium IDE and SideeX style)
- fix `'NoneType' object is not iterable` error for JMeter result reading
- load TestNG classes from JAR only if testng config isn't provided
- support for YAML files in utf-8 format
- fix connectTime inserted into JMX for JMeter 2.11
- add boundary extractor to JMeter YAML scripting
- make `ab`'s `headers` undestranding consistent with other executors
- pass/Fail module is not possible to use as service anymore - removed old deprecation
- use latest JMeter Plugins Manager 1.1 to speed-up startup
- fix jmx2yaml crash when parsing LoopController with continue_forever set
- proxy2jmx - because blazemeter backend has changed, we now first ask for the url of the jmx/smartjmx file and then downloading it like it used to be.
- add LANG=en_US.UTF-8 and locales into Docker image
- disable PYTHONPATH additions if interpreter is changed via config
- fix `swagger2yaml` crash when found schema with `type: file`
- fix `swagger2yaml` crash when an optional field is missing
- for cloud test, if no sandbox location found and `default-location` is not valid, use first location by default
- fix Gatling YAML scripts with CSV delimiter in `data-sources`


## 1.11.1<sup>26 apr 2018</sup>
 - fix JMeter 4.0 crash on Windows when `JMETER_HOME` var is unset
 - add `include-timers` block to JMeter transactions
 - add `match-no` and `use-namespaces` support into `extract-xpath` for JMeter
 - fix crash on Python 2 when adding custom fields to kpi.jtl
 - write connect time to kpi.jtl by default in JMeter
 - use `match-no=0` by default for JMeter JSONPath Extractor
 - do not generate `sleep` when `think-time` is zero for Nose executor
 - redo Apiritif code generation to split requests into separate methods
 - use master terminate API to shutdown not yet started BlazeMeter tests
 - fix the way default project is located for `cloud` tests
 - fix default pip inside Docker image to be pip2
 - fix `rt` field when exporting datapoints to JSON (convert msecs to secs)
 - add [doc page](KeywordIndex.md) with all config keywords we can find across doc pages.
 - support more error details internally, available for reporting services
 - add `simple-output` and `smart-output` option for `Proxy2JMX` service
 - don't limit env vars evaluating in scenarios blocks
 - evaluate only explicitly declared env variables across config
 - fix lost `basePath` when using `--scenarios-from-paths` in swagger2yaml
 - fix `swagger2yaml`: `default-address` should not contain endpoint path
 - SwaggerConverter.convert accepts fd
 - remove `options` object from SwaggerConverter
 - support auth (basic and API keys) for Swagger2YAML
 - swagger2YAML: Move base path to variable
 - support disabling explicit interpolation of parameters/headers
 - fix Gatling label groups read from simulation.log
 - support specifying properties for Gatling on scenario level
 - rename Gatling's `dir_prefix` option into `dir-prefix`

## 1.11.0<sup>26 mar 2018</sup>
 - don't force applying defaults into effective configuration (huge internal impact)
 - use HDR histograms from `hdrpy` package for storing response times
 - add environment variable evaluation in strings
 - bump up default JMeter to 4.0 and Plugins Manager to 0.20
 - handle non-string JMeter headers
 - properly recover if JMeter has written non-UTF8 chars into JTL
 - fix errors.jtl reading on MacOS + Python 3
 - remote webdriver support added: selenium grid, appium, local or remote browsers or thirdparty compatible services
 - add capabilities for remote webdriver - browser, version, javascript, platform, os_version, selenium, device, app
 - new browsers: Chrome-Android and Safari-iOS with local appium or remote webdriver support
 - keysBy* - Special keys are allowed using the prefix KEY_ List: http://selenium-python.readthedocs.io/api.html#module-selenium.webdriver.common.keys
 - new Selenium actions were added: selectBy*, doubleClickBy*, mouseDownBy*, mouseUpBy*, assertTextBy*, assertValueBy*, assertTitle
 - add `headless` switch for selenium-based tests (Chrome and Firefox)
 - move assertions inside transactions in Apiritif codegen
 - add function translation for Apiritif executor: `__base64Encode`, `__base64Decode`, `__UUID` and `__urlencode`
 - set `OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES` env variable to fight MacOS problems with Apiritif
 - fix `scenario`-level timeout setting for Apiritif
 - fix POST requests with form data generation for Apiritif
 - inherit from `unittest.TestCase` when generating Apiritif script
 - support `additional-classpath` for Gatling
 - add `swagger2yaml` converter
 - updated Geckodriver to 0.20.0 and Chromedriver to 2.37
 - fix Python2 + Robot issue with test durations rounded to seconds
 - make npm not touch `package.json` and `package-lock.json` for Node.js tests
 - provide icon for Taurus status screen
 - fix singletone service parameters merging
 - write debug messages to log file in case `-v` used
 - Make it to pick BZA user's account by default when possible
 - use own test type for functional test in BZA
 - use `name` filter in BZA /projects call
 - retry requests to BZA test status in case of network failure
 - always include OPL into `-locations` dump for BZA

## 1.10.5 <sup>8 feb 2018</sup>
 - add `-lint functionality for checking Taurus configs for errors/typos
 - support recursive `included-configs`
 - support `variables` for Robot executor
 - unify environment variables setting for executors
 - call `install-for-jmx` on modified JMX instead of original
 - wait a bit for JMeter plugins manager to complete plugin installation
 - for `cloud` provisioned test, don't fail on variable file name in upload
 - don't fail if unable to get connections count in self-monitoring
 - ensure that loaded configuration is dict, throw appropriate exception otherwise
 - remove chrome profiler as it was outdated and unfixable
 - apply load settings for JMX later than modifications disable thread groups
 - support Runtime Controller and Interleave Controller in `jmx2yaml`
 - make Java executors to prefer user-specified JAR files over default ones
 - make TestNG runner to fail less on missing transitional dependencies

## 1.10.4 <sup>9 jan 2018</sup>
 - collect `conn-all` monitoring metric with the help of `netstat` utility (note that that introduces depepdency on `net-tools` package for Linux)
 - don't level down logging if `settings.verbose` is set
 - set `write-xml-jtl=full` in JMeter if `settings.verbose` is set
 - fix files upload resolve for JMeter variables 
 - fix error when exception is not shown for failed JMeter run
 - fix JSONPath Extractor generated for JMeter
 - set default timeout for BlazeMeter integration to 30 seconds
 - support specifying cloud test by link to BM
 - fix launching cloud tests by id
 - fix crash when attempting to use external test as a cloud test
 - add `send-report-email` option for cloud tests
 - support `account` and `workspace` entities for cloud tests
 - use `requests.Session` for BZA requests (enabled keep-alive)
 - fix reporting for concurrent apiritif
 - fix duration reporting in `final-stats` for some cases
 - fix the way Taurus reads result file globally
 - optimize local resource monitoring collecting
 - fix mixed order of percentile and response code fields in `final-stats`'s CSV report
 - make 'effective.json' file to be JSON-strict (no infinity literals)
 
[Changelog for Year 2017](Changelog2017.md)
