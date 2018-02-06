# Changelog

## 1.10.5 <sup>next</sup>
 - support recursive `included-configs`
 - support `variables` for Robot executor
 - add linter service for checking Taurus configs for errors/typos
 - unify environment variables setting for executors
 - call `install-for-jmx` on modified JMX instead of original
 - wait a bit for JMeter plugins manager to complete plugin installation
 - for `cloud` provisioned test, don't fail on variable file name in upload
 - don't fail if unable to get connections count in self-monitoring
 - ensure that loaded configuration is dict, throw appropriate exception otherwise
 - remove chrome profiler as it was outdated and unfixable
 - apply load settings for JMX later than modifications disable thread groups
 - support Runtime Controller and Interleave Controller in `jmx2yaml`

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
