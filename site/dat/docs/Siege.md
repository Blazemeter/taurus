# Siege Executor 

## About
Siege is an http load testing and benchmarking utility. Besides command line it's controlled by some resource and config files. Siege lets its user hit a server with a configurable modes and number of simulated clients. Those clients place the web server "under siege".

## Supported Features
Taurus is supported these facilities of Siege:
CONCURRENT (`concurrency` in Tarus terms): set number of simulated users
URL-FILE(`script`): file with list of target URLs
INTERNET&DELAY: randomly hitting of url-file with some delay if you set it with `think-time` option and
BENCHMARK: throughput test if `think-time` option is not found
HEADER(`hearders` in yml config file) allows you to  add additional header info
REPS(`iterations`) or (if they not detected)
TIME(`hold-for`) - running the test for a selected period of time. Naturally you can set comfortable taurus time format.

## Scenario Examples
```yaml

```