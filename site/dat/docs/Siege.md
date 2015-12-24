# Siege Executor 

## About
Siege is an http load testing and benchmarking utility. Besides command line it's controlled by some resource and config files. Siege lets its user hit a server with a configurable modes and number of simulated clients. Those virtual clients place the web server "under siege".

## Supported features
Taurus is supported these facilities of Siege:
CONCURRENT (`concurrency` in Tarus terms): set number of simulated users (default 1).
URL-FILE(`script`): file with list of target URLs. Taurus allow you to set `variables` for this file. 
INTERNET & DELAY: randomly hitting of url-file with some delay if you set it with `think-time` option and
BENCHMARK: throughput test if `think-time` option is not found.
HEADER(`headers` in yml config file) allows you to  add additional header info.
REPS(`iterations`) or (if they not detected)
TIME(`hold-for`) - running the test for a selected period of time. Naturally you can set comfortable taurus time format.

## Some reasonable limitations
Keep in mind these rules:
You must specify at leas one test target with `url` param in `requests` section or as url-file (`script`).
It's needed to choose explicitly `iterations` or `hold-for` mode.
If you used `script` option no `variables` or `requests` are processed.
And of course you need Siege. If this program not in the system PATH help the Taurus to find it with `path` option.
 
## Configuration example fragments
Simplest working example - just use it for taste.
```yaml
---
execution:
  executor: siege
  concurrency: 3 
  repetition: 10
  scenario:
    requests:
      - url: 'blazedemo.com'
```
Five tests for every of hundred users without delay (it may be hardcore):
```yaml
---
settings:
  path: 'C:\SIEGE\SIEGE.EXE'
execution:
  executor: siege
  concurrency: 100
  repetition: 5
  scenario:
    requests:
      - url: 'server.com'
```
Test servers from nodes.list with 1000 typical user emulation for 5 minutes
```yaml
---
settings:
  path: '/usr/bin/siege'
execution:
  executor: siege
  concurrency: 1000
  hold-for: 5m
  scenario:
    think-time: 15s
    script: '~/tests/nodes.list'
```
Variables example:
```yaml
---
scenario:
    requests:
      - variables:
        - HOST0: 'http://mainhost.com/'
      - url: '$(HOST0)page1.html'
      - url: '$(HOST0)dir/page2.html'
```
