# Changelog for year 2015

## 0.5.2 <sup>17 dec 15</sup>
 - fix JMeter installation on windows

## 0.5.1 <sup>11/16/15</sup>
 - fix shellexec env variables
 - add `assume-success` flag for JMeter assertions
 - flexibly handle '$1$' templates for JMeter regex extractor
 - fix null body parameter for JMeter

## 0.5.0 <sup>11/16/2015</sup>
 - allow using Xvfb for Selenium executor
 - don't use lynx browser when opening report links
 - add PBench executor type
 - show scenario alias in sidebar widget for JMeter
 - set `TAURUS\_ARTIFACTS\_DIR` environment variables for shellexec commands
 - add `cwd` option for shellexec tasks
 - add `env` option for shellexec tasks

## 0.4.5 <sup>22 oct 2015</sup>
 - load settings applied to Stepping Thread Groups
 - shellexec service: print to stdout/stderr when `out`/`err` options set to `null`
 - JMeter: append `user.properties` instead of overriding
 - move pass/fail to services
 - set BlazeMeter session note with test error message if present

## 0.4.4 <sup>22 sep 2015</sup>
 - limit max size of file that goes into zip for artifact upload (max-size option)
 - fix cumulative KPIset recalculating
 - JMeter, Grinder, Gatling, JUnit now use mirrors during installation
 - files from Listeners now excluded from resources
 
## 0.4.3 <sup>17 sep 2015</sup>
 - allow passing config as stdin to CLI like `./generate-config.sh | bzt`
 - allow having `report-name=ask` for BlazeMeter reporter
 - selenium test runners now using xml jtl for errors
 - console graph max height resets on data scroll
 - resource monitoring service implemented

## 0.4.2 <sup>11 sep 2015</sup>
 - bump up Gatling version to 2.1.7
 - selenium script from requests format
 - change file search algo to use paths relative to config locations
 - allow having script files relative to config locations
 - allow having `included-configs` instruction
 - allow setting path to artifacts from `settings` in config, deprecate `--data-dir` CLI option

## 0.4.1 <sup>9 sep 2015</sup>
 - fix Locust.io search paths
 - `generalize-labels` default value changed to `false`
 - fix JMeter properties not read from jmeter files
 - force JMeter to use epoch timestamp format for CSV files
 - Allow setting project ID instead of name, fail on project name clash
 - parameterize JMeter graceful shutdown time

## 0.4.0 <sup>31 aug 2015</sup>
 - allow dumping final stats in Jenkins-consumable format
 - implemented graceful shutdown for JMeter
 - for sample that failed because of embedded resources, actual error message used instead of "OK"
 - support Locust.io load generator
 - allow setting the `report-name` for blazemeter report
 - allow easy setting report/test/project options for blazemeter module

## 0.3.8 <sup>26 aug 2015</sup>
  - fixed bug when old jars were not removed during JMeter installation
  - add `project` to BlazeMeter report config, allowing to sort tests by projects
  - allow `message` for pass/fail criteria to improve readability
  - implement "services" top-level config section
  - implemented shellhook service

## 0.3.7 <sup>13 aug 2015</sup>
  - fail criteria without timeframe is checked at the end of the test
  - fixed shutdown on windows
  - fixed label names in junitxml reports
  - blazemeter report url added to every testcase in junitxml reports

## 0.3.6 <sup>13 aug 2015</sup>
  - added jmx2yaml tool
  - added updates check capability

## 0.3.5 <sup>16 jul 2015</sup>
  - fix Selenium executor logging
  - added CSS/JQuery extractor support in scenarios generated from requests option.
  - fixed JMeter installation path issue one more time

## 0.3.4 <sup>16 jul 2015</sup>
  - fixed JMeter path bug on windows
  
## 0.3.3 <sup>16 jul 2015</sup>
  - fixed tools check/installation on windows
  - fixed resource files gathering issue

## 0.3.2 <sup>14 jul 2015</sup>
  - use progressbar for download progress indicators
  - fix issue with unicode XML on Mac
  - use JTL format in Selenium results

## 0.3.1 <sup>7 jul 2015</sup>
  - fixed python3 installation issue (progressbar33 now used instead of progressbar)

## 0.3.0 <sup>6 jul 2015</sup>
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

## 0.2.23 <sup>6 jul 2015</sup>
  - rename `bulk-size` option into `send-interval` for BlazeMeter reporter
  - explicitly fail in case of wrong `body` option for HTTP request
  - fixed bug when JMeter test duration was not applied properly.

## 0.2.22 <sup>6 jul 2015</sup>
  - send data to BlazeMeter less frequently (30 secs)
  - added ability to access BZA feeding through proxy
  - fixed bug with modifying paths of resource files in distribute test mode
  
## 0.2.21 <sup>6 jul 2015</sup>
  - if `iterations` set, then duration for test will not be limited
  - added `steps` option to execution settings
  
## 0.2.20 <sup>5 jul 2015</sup>
  - add `within` logic to timeframed pass-fail criteria
  - added `use-dns-cache-mgr` option.
  - default-domain option renamed to default-address (scheme, hostname, port are now parsed from this option).
  
## 0.2.19 <sup>6 jul 2015</sup>
  - fixed bug when in distributed tests VU count was not calculated properly.
  - auto-append `${__machineName()}` for thread names in distributed test
  - fix module search path issue on MacOS

## 0.2.18 <sup>21 may 2015</sup>
  - set "clear each iteration" flag for cache and cookie managers when generating JMX from requests
  - allow wildcards in enable-disable modifications  

## 0.2.17 <sup>15 may 2015</sup>
  - added ability to change font size in Windows dashboard GUI on Ctrl + mousewheel event
  - reworked CSV reading for JMeter to support quoted data

## 0.2.16 <sup>14 may 2015</sup>
  - fix base config not copied because of broken imports
  - display console screen in separate window on windows

## 0.2.15 <sup>13 may 2015</sup>
  - replace digits and UUID sequences with N and U to decrease label count
  - fix not working `bzt 1.jmx 2.jmx 3.jmx`

## 0.2.14 <sup>13 may 2015</sup>
  - added support for user defined variables
  - fix reading for non-standard errors JTL

## 0.2.13 <sup>12 may 2015</sup>
  - Some more stats have been added to console screen
  - add `-gui` command-line alias to open JMeter UI for debugging
  - add support for JMeter distributed mode
  
## 0.2.12 <sup>5 may 2015</sup>
  - Added http request defaults options
  - Added support of RPS shaper component
  - Remove conflicting JAR libraries during JMeter installation procedure
  - Fixed bug when resource files were not properly copied to artifacts directory

## 0.2.11 <sup>5 may 2015</sup>
  - Base config fix on Windows and minor changes in setup.py

## 0.2.8 <sup>29 apr 2015</sup>
  - Fix base config not found on Windows with multiple disks
  - Added proper version of lxml in requirements

## 0.2.1 <sup>28 apr 2015</sup>
  - Added pass/fail criteria report on console reporter

## 0.2.0 <sup>15 apr 2015</sup>
  - Added JSON path assertion.
  - Added parameters for final_stats reporter
  - Added ability to generate query string based on parameters and request type.
