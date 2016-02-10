# Gatling Executor

Configuration options:

 - `path`: "/somepath/folder/bin/gatling_executable"
    Path to Gatling executable.
    If no Gatling executable found, it will be automatically downloaded and installed in "path".
    By default "~/.bzt/gatling-taurus/bin/gatling.sh".
    
 - `download-link`:"http://somehost/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    Link to download Gatling.
    By default: "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    
 -  `version`: "2.1.4"
    Gatling version, by default "2.1.4"

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

 Taurus supports possibility to send values of execution options `concurrency`, `ramp-up` and `hold-for` to Gatling test script. Below you can see example of usage these parameters on the Gatling side:
 
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