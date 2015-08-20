# Changelog

## 0.3.8 (next)
  - implement "services" top-level config section

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
