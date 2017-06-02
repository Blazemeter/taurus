# Gatling Executor

Gatling is load testing tool which most famous as choice for testing of HTTP servers.

In Taurus you have two way for run it: with native gatling script or with usual Taurus features: `requests`,
`iterations`, etc. In last case scala script will be generated automatically.

## Run Gatling Tool

```yaml
execution:
- executor: gatling
  scenario: sample

scenarios:
  sample:
    script: tests/gatling/BasicSimulation.scala
    simulation: tests.gatling.BasicSimulation
    keepalive: true
```

The `simulation` option is canonical class name for main simulation class. It will be passed as-is to gatling
with `-s` option.
Also you can use `keepalive` and `timeout` scenario attributes to set appropriate HTTP feature and
limit request time accordingly.

## Load Configuration

Taurus supports possibility to send values of execution options `concurrency`, `iterations`, `ramp-up` `hold-for`
and `throughput` to Gatling test script. Below you can see how you can use these parameters on the Gatling side
to set up your test:

```scala
package tests.gatling

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class BasicSimulation extends Simulation {
  // parse load profile from Taurus
  val t_iterations = Integer.getInteger("iterations", 100).toInt
  val t_concurrency = Integer.getInteger("concurrency", 10).toInt
  val t_rampUp = Integer.getInteger("ramp-up", 1).toInt
  val t_holdFor = Integer.getInteger("hold-for", 60).toInt
  val t_throughput = Integer.getInteger("throughput", 100).toInt
  val httpConf = http.baseURL("http://blazedemo.com/")

  // 'forever' means each thread will execute scenario until
  // duration limit is reached
  val loopScenario = scenario("Loop Scenario").forever() {
    exec(http("index").get("/"))
  }

  // if you want to set an iteration limit (instead of using duration limit),
  // you can use the following scenario
  val iterationScenario = scenario("Iteration Scenario").repeat(t_iterations) {
    exec(http("index").get("/"))
  }

  val execution = loopScenario
    .inject(rampUsers(t_concurrency) over t_rampUp)
    .protocols(httpConf)

  setUp(execution).
    throttle(jumpToRps(t_throughput), holdFor(t_holdFor)).
    maxDuration(t_rampUp + t_holdFor)
}
```

## Running Complex Gatling Test Suites

If your Gatling test suite is really huge or has dependencies on other files - you can bundle it in a jar
(with the help of sbt or Maven) and then run this jar with Taurus. Just specify it as a `script` value in scenario.

```yaml
execution:
- executor: gatling
  scenario: sample

scenarios:
  sample:
    script: simulations.jar
    simulation: tests.gatling.BasicSimulation
```

## Building Test Script from Config

If your scenario don't contains `script` parameter and contains at least one element of `requests` Taurus will build
scala script for test. This script will be placed in `[artifact-dir](ConfigSyntax/#Top-Level-Settings)`:
you can modify it and use with Gatling later.

Following features are supported: request generation, `default-address`, `follow-redirect`, `headers`, `think-time`
on scenario and request levels, `body` of request and params that described in
`[Load Configuration](#Load-Configuration)`.
Some asserts can be added to request. Assert describes templates and area for search (`contains` and `subject`
accordingly), regexp and inverse marks. You can look for particular response code in `http-code` part or for string
and regular expression in `body` of request.
 Next yaml example shows the way these features can be used and ready to conversion to scala automatically:

```yaml
execution:
- executor: gatling
  iterations: 15
  concurrency: 3
  ramp-up: 2
  hold-for: 10
  scenario: complex_sample

scenarios:
  complex_sample:
    data-sources:
    - path: buyouts.csv  # path to CSV file
      delimiter: ','  # optional, set to comma by default
      loop: true  # loop over data source file, true by default
    default-address: blazedemo.com
    headers:
      HEADER_1: VALUE_1
      HEADER_2: VALUE_2
    requests:
    - url: /
      assert:
      - subject: body # subject for search (defalut: body)
        regexp: true  # whether expression is regular (default: false)
        not: true     # invert condition (default: false)
        contains: # expression list for assertion (mandatory)
        - .+sometext.+  
      body: 'Some Body Data'
      follow-redirect: false    #   true by default
      headers:
        HEADER_11: VALUE_11
    - url: /reserve.php
      think-time: 2s
      assert:
      - contains:
        - 200
        subject: http-code
        not: true
    - /purchase.php?username=${username}&email=${email}  # usage of variables from the CSV data source
```

## Configuration Options

 Similar to other modules there is possibility of global configuration Gatling Executor by write some lines in
 `gatling` branch of modules setting. Next options can be set:
 - `path`: "/somepath/folder/bin/gatling_executable"
    Path to Gatling executable.
    If no Gatling executable found, it will be automatically downloaded and installed in "path".
    By default "~/.bzt/gatling-taurus/bin/gatling.sh".

 - `java-opts`: string with some java options for Gatling

 - `download-link`:"http://somehost/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    Link to download Gatling.
    By default: "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"

 -  `version`: "2.1.7"
    Gatling version, 2.1.7 by default

 - `properties`: dictionary for tuning of gatling tool behaviour (see list of available parameters in gatling
 documentation) and sending your own variables into Scala program:

```yaml
modules:
  gatling:
    properties:
      gatling.data.file.bufferSize: 512 # output buffer size, 256 bytes by default
      your_variable: 1024               # user variable
```
You can read values of variables in Scala code similar to `[Load Configuration](#Load-Configuration)` style:

```scala
class BasicSimulation extends Simulation {
  val your_scala_variable = Integer.getInteger("your_variable", 0).toInt
}
```

## External Java Libraries Usage

Thanks to Taurus you can use additional Java classes in your scala code. For this add required jar files or
contained dir to `files` list:

```yaml
execution:
- executor: gatling
  concurrency: 10
  hold-for: 1h
  scenario: example
  files:
  - first.jar
  - second.jar
  - folder_with_jars
scenarios:
  example:
    script: my_file.scala
```

## Gatling 2.2.0 Support

Taurus works with Gatling 2.2.0. However, with Gatling 2.2.0 it's not possible to extract such network stats
as latency and connection time, as Gatling removed them from report data. Because of that, Taurus installs Gatling 2.1.7
by default.
