# Siege Executor 

## About
[Siege](https://www.joedog.org/siege-home/) is an HTTP load testing and benchmarking utility. Taurus supports the following Siege features:

 - CONCURRENT (`concurrency` in Taurus terms): number of simulated users (default 1),
 - URL-FILE(`script`): a file with a list of target URLs. Taurus allow you to set `variables` for this file,
 - INTERNET & DELAY: randomly hitting of url-file with some delay if you set it with `think-time` option,
 - BENCHMARK: throughput test if `think-time` option is not found,
 - HEADER(`headers` in config file): set HTTP headers for request,
 - REPS(`iterations`): repeat requests N times,
 - TIME(`hold-for`): set execution time limit.
 - SIEGERC(`rc-file`): the siege config file to use (default: `None`)

Please keep in mind these rules when using Siege test executor:
 - You must specify at least one URL with `url` param in `requests` section or as url-file (`script`).
 - It's required to explicitly set either `iterations` or `hold-for` limit.
 - If you use `script` option - no `variables` or `requests` will be used.
 - You need to have Siege installed. If `siege` isn't in the system PATH - you can specify the path to Siege binary with `path` option.
 
## Configuration Examples

Simplest working example - use it to get taste of the tool.
```yaml
execution:
- executor: siege
  concurrency: 3 
  iterations: 10
  scenario: simplest

scenarios:
  simplest:
    requests:
    - http://blazedemo.com/
```

Five repeats by hundred of users without any delay (might hurt the server):
```yaml
execution:
- executor: siege
  concurrency: 100
  iterations: 5
  scenario: simplest
  rc-file: path/to/my.siegerc

scenarios: 
  simplest:
    requests:
    - http://blazedemo.com/
```

Test URLs from `nodes.list` file with 50 users, hold load for 5 minutes
```yaml
execution:
- executor: siege
  concurrency: 50
  hold-for: 5m
  scenario: external_urls
  
scenarios:
  external_urls:
    think-time: 15s
    script: ~/tests/nodes.list
```

Variables example:
```yaml
scenarios:
  variables_usage:
    variables:
      HOST0: 'http://mainhost.com/'
    requests:
      - url: '$(HOST0)page1.html'
      - url: '$(HOST0)dir/page2.html'
```

## Module Settings

If you have Siege in non-standard location, please use `path` option to point Taurus to `siege` binary:

```yaml
modules:
  siege:
    path: /home/user/sources/siege/bin/siege
```
