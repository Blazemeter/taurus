# Changelog

## 0.4.3 (next)
 - allow passing config as stdin to CLI like `./generate-config.sh | bzt`
 - allow having `report-name=ask` for BlazeMeter reporter

## 0.4.2
 - bump up Gatling version to 2.1.7
 - selenium script from requests format
 - change file search algo to use paths relative to config locations
 - allow having script files relative to config locations
 - allow having `included-configs` instruction
 - allow setting path to artifacts from `settings` in config, deprecate `--data-dir` CLI option

## 0.4.1
 - fix Locust.io search paths
 - `generalize-labels` default value changed to `false`
 - fix JMeter properties not read from jmeter files
 - force JMeter to use epoch timestamp format for CSV files
 - Allow setting project ID instead of name, fail on project name clash
 - parameterize JMeter graceful shutdown time

## 0.4.0
 - allow dumping final stats in Jenkins-consumable format
 - implemented graceful shutdown for JMeter
 - for sample that failed because of embedded resources, actual error message used instead of "OK"
 - support Locust.io load generator
 - allow setting the `report-name` for blazemeter report
 - allow easy setting report/test/project options for blazemeter module

## 0.3.8
  - fixed bug when old jars were not removed during JMeter installation
  - add `project` to BlazeMeter report config, allowing to sort tests by projects
  - allow `message` for pass/fail criteria to improve readability
  - implement "services" top-level config section
  - implemented shellhook service

## 0.3.7
  - fail criteria without timeframe is checked at the end of the test
  - fixed shutdown on windows
  - fixed label names in junitxml reports
  - blazemeter report url added to every testcase in junitxml reports

## 0.3.6
  - added jmx2yaml tool
  - added updates check capability

## 0.3.5
  - fix Selenium executor logging
  - added CSS/JQuery extractor support in scenarios generated from requests option.
  - fixed JMeter installation path issue one more time

## 0.3.4
  - fixed JMeter path bug on windows
  
## 0.3.3
  - fixed tools check/installation on windows
  - fixed resource files gathering issue

## 0.3.2
  - use progressbar for download progress indicators
  - fix issue with unicode XML on Mac
  - use JTL format in Selenium results

## 0.3.1
  - fixed python3 installation issue (progressbar33 now used instead of progressbar)

## 0.3.0
  - implement `selenium` executor
  - fix crashing on second start jmeter under GUI mode
  - iterate requests forever if no limits were specified
  - fix test duration logic to respect iterations set
  - distributed tests are now supported with JMeter UI (`gui: true` option)
  - install JMeter-Plugins 1.3.0
  - all default tool locations are now under `~/.bzt` dir
  - FinalStatus reporter now provides test duration (test-duration option, True by default)
  - bzt now fails when no requests were given in requests scenario
  - six module version requirements was removed

## 0.2.23
  - rename `bulk-size` option into `send-interval` for BlazeMeter reporter
  - explicitly fail in case of wrong `body` option for HTTP request
  - fixed bug when JMeter test duration was not applied properly.

## 0.2.22
  - send data to BlazeMeter less frequently (30 secs)
  - added ability to access BZA feeding through proxy
  - fixed bug with modifying paths of resource files in distribute test mode
  
## 0.2.21
  - if `iterations` set, then duration for test will not be limited
  - added `steps` option to execution settings
  
## 0.2.20
  - add `within` logic to timeframed pass-fail criterias
  - added `use-dns-cache-mgr` option.
  - default-domain option renamed to default-address (scheme, hostname, port are now parsed from this option).
  
## 0.2.19
  - fixed bug when in distributed tests VU count was not calculated properly.
  - auto-append `${__machineName()}` for thread names in distributed test
  - fix module search path issue on MacOS

## 0.2.18
  - set "clear each iteration" flag for cache and cookie managers when generating JMX from requests
  - allow wildcards in enable-disable modifications  

## 0.2.17
  - added ability to change font size in Windows dashboard GUI on Ctrl + mousewheel event
  - reworked CSV reading for JMeter to support quoted data

## 0.2.16
  - fix base config not copied because of broken imports
  - display console screen in separate window on windows

## 0.2.15
  - replace digits and UUID sequences with N and U to decrease label count
  - fix not working `bzt 1.jmx 2.jmx 3.jmx`

## 0.2.14
  - added support for user defined variables
  - fix reading for non-standard errors JTL

## 0.2.13
  - Some more stats have been added to console screen
  - add `-gui` command-line alias to open JMeter UI for debugging
  - add support for JMeter distributed mode
  
## 0.2.12
  - Added http request defaults options
  - Added support of RPS shaper component
  - Remove conflicting JAR libraries during JMeter installation procedure
  - Fixed bug when resource files were not properly copied to artifacts directory

## 0.2.11
  - Base config fix on Windows and minor changes in setup.py

## 0.2.8
  - Fix base config not found on Windows with multiple disks
  - Added proper version of lxml in requirements

## 0.2.1
  - Added pass/fail criteria report on console reporter

## 0.2.0
  - Added JSON path assertion.
  - Added parameters for final_stats reporter
  - Added ability to generate query string based on parameters and request type.
