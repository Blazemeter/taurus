# Changelog

# 0.2.22 (next)
  - send data to BlazeMeter less frequently (30 secs)
  - added ability to access BZA feeding through proxy
  
# 0.2.21
  - if `iterations` set, then duration for test will not be limited
  - added `steps` option to execution settings
  
# 0.2.20
  - add `within` logic to timeframed pass-fail criterias
  - added `use-dns-cache-mgr` option.
  - default-domain option renamed to default-address (scheme, hostname, port are now parsed from this option).
  
# 0.2.19
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
 

![Analytics](https://ga-beacon.appspot.com/UA-63369152-1/taurus/changelog)