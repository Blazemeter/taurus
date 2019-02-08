# PBench Executor 

*High throughput, high precision HTTP load testing with low resource requirement*

## TL;DR
Take fresh Ubuntu 14.04 (or 16.04) and run on it:
```bash
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:yandex-load/main
sudo apt-get update
sudo apt-get install phantom phantom-ssl
sudo dpkg -i blazemeter-pbench-extras_0.0.6.1_amd64.deb  # built from https://github.com/Blazemeter/taurus-pbench
```

Then use it like this:
```yaml
execution:
- executor: pbench
  scenario: simple_usage

scenarios:
  simple_usage:  
    default-address: http://blazedemo.com/
    requests:
    - /
    - /vacation.html
```

## About
Taurus PBench executor relies on [Yandex.Phantom](https://github.com/mamchits/phantom) open source framework ([LGPL 2.1](https://github.com/mamchits/phantom/blob/master/COPYING)). That framework of C++ I/O engine, that can generate a high-throughput HTTP load with its `benchmark` module. It is Linux-only software, works perfectly on Ubuntu (other distributions possible with slight tweaking). Pre-compiled debian packages are located in Yandex-Load [Launchpad repository](https://launchpad.net/~yandex-load/+archive/ubuntu/main). There are also a support for UDP [module](https://github.com/slon/phantom-udp-benchmark) exists and possibility to implement other protocollers on C++ level.

### Stock PBench Mode
By default, pbench module uses stock phantom version. "Stock" means no enhancements developed by BlazeMeter. This means it will generage huge source data files for each test, this consumes hard drive space and takes a lot of time.

To install stock `phantom`, which is required to run both stock and enhanced modes, do:
```bash
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:yandex-load/main
sudo apt-get update
sudo apt-get install phantom phantom-ssl
```

### Enhanced PBench Mode
After installing enhancements package [blazemeter-pbench-extras](https://github.com/Blazemeter/taurus-pbench), it also installs settings file for Taurus that switches `pbench` executor into enhanced mode. This means it will generate source data fast and will not consume too much of disk space.

The package is compiled from sources and installed like this:
```bash
sudo dpkg -i blazemeter-pbench-extras_0.0.6.1_amd64.deb
```

If you don't want to build the package yourself, you can download it from Taurus website: [blazemeter-pbench-extras_0.0.6.1_amd64.deb](http://gettaurus.org/msi/blazemeter-pbench-extras_0.1.10.1_amd64.deb)

## Scenario Building
High throughput and precision comes with a price of lost requests logic. No loops, no extractors, no assertions, no think-times, no automatic cookie handling. Also, all requests must go to the same `proto://server:port` address.

First way to give it requests is to use usual Taurus requests spec of:
```yaml
scenarios:
  pbench-example:
    default-address: http://blazedemo.com
    timeout: 10s  # important setting, affects for how long its workers are busy with request
    headers:
      User-Agent: Mozilla
    requests:
    - /
    - url: /vacation.html
      headers:
        Accept: "*/*"
      method: POST
      body:
        formParam1: value1
        formParam2: value2
    - url: /do.php
      method: PUT
      body: '{"json": "body"}'
```
 
Second way is to generate the payload script yourself and use it. This is frequently done with custom scripts that takes production server logs as source data:
```yaml
scenarios:
  pbench-example:
    default-address: http://blazedemo.com
    timeout: 10s  # important setting, affects for how long its workers are busy with request
    script: payload.src
    keepalive: true # set HTTP connection reuse (true is default)
```

Where `payload.src` is a file of following format:
```
<len> <marker>\n
<request>\n
```

Having `len` is a length of `request`, `marker` is a sample label, `request` is HTTP request on TCP payload level. Note that it will work fine with `\r\n` instead of `\n`, this is for convenience. The easiest way to understand how it looks is to make a test using `requests` scenario and search for `*.src` file in Taurus artifacts. 


## Load Generation Specifics

Concurrency for PBench means how many workers will be allocated for a test. Worker is very much like "object that has HTTP connection", it reads source data, sends request and receives response. During this process worker is busy and counted as active. So if server's response time is high and concurrency is low there might be a situation when all workers are busy and PBench will be unable to sustain reqired throughput. To fix it, either increase `concurrency`, or decrease `timeout`. Decreasing `timeout` will lead to "Connection timed out" errors, meaning server failed to handle requests fast enough.

There is a way to change the load in runtime by appending schedule file with additional schedule. PBench will read this new data and execute it. If new data will have proper payload loop markers, it will switch into new schedule loop. In the future Taurus `pbench` module will have helper methods to automate this.


## Results Processing Specifics

VU count reflects the max number of busy workers during second, so the number might increase and decrease, depending on server response times.
