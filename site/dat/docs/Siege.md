# Siege Executor 

## About
[Siege](https://www.joedog.org/siege-home/) is an http load testing and benchmarking utility. Taurus supports these features of Siege:

 - CONCURRENT (`concurrency` in Taurus terms): set number of simulated users (default 1).
 - URL-FILE(`script`): file with list of target URLs. Taurus allow you to set `variables` for this file. 
 - INTERNET & DELAY: randomly hitting of url-file with some delay if you set it with `think-time` option and
 - BENCHMARK: throughput test if `think-time` option is not found.
 - HEADER(`headers` in config file) allows you to add additional header info.
 - REPS(`iterations`) repeat requests N times
 - TIME(`hold-for`) - running the test for desired period of time

Please keep in mind these rules when using `executor: siege`:
 - You must specify at least one URL with `url` param in `requests` section or as url-file (`script`).
 - It's needed to choose explicitly `iterations` or `hold-for` mode.
 - If you used `script` option no `variables` or `requests` are processed.
 - You need Siege too to be installed. If this program not in the system PATH help the Taurus to find it with `path` option.
 
## Configuration Examples
Simplest working example - just use it to get taste of the tool.
```yaml
---
execution:
- executor: siege
  concurrency: 3 
  repetition: 10
  scenario:
    requests:
    - http://blazedemo.com/
```

Five repeats by hundred of users without any delay (might hurt the server):
```yaml
---
execution:
- executor: siege
  concurrency: 100
  repetition: 5
  scenario:
    requests:
    - http://blazedemo.com/
```

Test URLs from `nodes.list` file with 50 users, hold load for 5 minutes
```yaml
---
execution:
- executor: siege
  concurrency: 50
  hold-for: 5m
  scenario:
    think-time: 15s
    script: ~/tests/nodes.list
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


## Module Settings

If you have Siege in non-standard location, please use `path` option like this:

```yaml
---
modules:
  siege:
    path: /home/user/sources/siege/bin/siege
```