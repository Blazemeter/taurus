# Changelog

## 1.10.6<sup>next</sup>
 - use HDR histograms from `hdrpy` package for storing response times
 - add environment variable evaluation in strings
 - bump up default JMeter to 4.0 and Plugins Manager to 0.20
 - remote webdriver support added: selenium grid, appium, local or remote browsers or thirdparty compatible services
 - add capabilities for remote webdriver - browser, version, javascript, platform, os_version, selenium, device, app
 - new browsers: Chrome-Android and Safari-iOS with local appium or remote webdriver support
 - keysBy* - Special keys are allowed using the prefix KEY_ List: http://selenium-python.readthedocs.io/api.html#module-selenium.webdriver.common.keys
 - new Selenium actions were added: selectBy*, doubleClickBy*, mouseDownBy*, mouseUpBy*, assertTextBy*, assertValueBy*, assertTitle
 - add `headless` switch for selenium-based tests (Chrome and Firefox)
 - provide icon for Taurus status screen
 - fix singletone service parameters merging
 - fix Python2 + Robot issue with test durations rounded to seconds
 - move assertions inside transactions in Apiritif codegen
 - add function translation for Apiritif executor: `__base64Encode`, `__base64Decode`, `__UUID` and `__urlencode`
 - add `swagger2yaml` converter
 - support `additional-classpath` for Gatling
 - don't force applying defaults into effective configuration (huge internal impact)
 - set `OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES` env variable to fight MacOS problems

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
