# Gatling Executor

Gatling is load testing tool which most famous as choice for testing of HTTP servers.
 
In Taurus you have two way for run it: with native gatling script or with usual Taurus features: `requests`, `iterations`, etc. In last case scala script will be generated automatically.  

## Run Gatling Tool

```yaml
---
execution:
- executor: gatling
  scenario:
    script: tests/gatling/BasicSimulation.scala
    simulation: tests.gatling.BasicSimulation
```

The `simulation` option is canonical class name for main simulation class. It will be passed as-is to gatling with `-s` option.

## Load Configuration

 Taurus supports possibility to send values of execution options `concurrency`, `iterations`, `ramp-up` and `hold-for` to Gatling test script. Below you can see example of usage these parameters on the Gatling side:
 
```
 package mytest

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class BasicSimulation extends Simulation {

  val httpConf = http
    .baseURL("http://blazedemo.com/")

  val scn = scenario("Extra").exec(
          http("request_1").get("/")
      )

  val t_concurrency = Integer.getInteger("concurrency", 10).toInt
  val t_ramp_up = Integer.getInteger("hold-for", 1).toInt
  val t_hold_for = Integer.getInteger("ramp-up", 1).toInt

  setUp(scn.inject(rampUsers(t_concurrency) over (t_ramp_up))
    .protocols(httpConf)).maxDuration(t_ramp_up + t_hold_for)
}
```

## Building Test Script from Config

 If your scenario don't contains `script` parameter and contains at least one element of `requests` Taurus will build scala script for test. This script will be placed in `[artifact-dir](ConfigSyntax/#Top-Level-Settings)`: you can modify it and use with Gatling later. 
 
 Next abilities are supported: `default-address`, `requests`, `headers` on scenario and request levels, `body` of request, `think-time` and params that described in `[Load Configuration](#Load Configuration)`. Next yaml example can be converted to scala automatically:

```yaml
---
execution:
- executor: gatling
  iterations: 15
  concurrency: 3
  ramp-up: 2
  hold-for: 10
  scenario:
    default-address: blazedemo.com
    headers:
      HEADER_1: VALUE_1
      HEADER_2: VALUE_2
    requests:
    - url: /
      body: 'Some Body Data'
      headers:
        HEADER_11: VALUE_11
    - url: /reserve.php
      think-time: 2s
```
## Configuration options:

 Similar to other modules there is possibility of global configuration Gatling Executor by write some lines in `gatling` branch of modules setting. Next options can be set:
 - `path`: "/somepath/folder/bin/gatling_executable"
    Path to Gatling executable.
    If no Gatling executable found, it will be automatically downloaded and installed in "path".
    By default "~/.bzt/gatling-taurus/bin/gatling.sh".
    
 - `download-link`:"http://somehost/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    Link to download Gatling.
    By default: "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    
 -  `version`: "2.1.4"
    Gatling version, by default "2.1.4"
