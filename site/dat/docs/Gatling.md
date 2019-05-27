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

Following features are supported: 
  - request generation
  - on scenario level: `think-time`, `default-address`, `follow-redirects`, `headers`, `store-cache`, `keepalive`, 
  `timeout`, `retrieve-resources`, `retrieve-resources-regex`
  - on request level: `think-time`, `body`
  - params that described in `[Load Configuration](#Load-Configuration)`.
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
    store-cache: true  # cache HTTP responses, true by default
    retrieve-resources: true # false by default, retrieves all embedded resources from HTML pages
    retrieve-resources-regex: (.*)boo(.*) # regular expression used to match any resource (white list)
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
      body: 'Some Body Data'    # can be string or dictionary
      follow-redirects: false    #   true by default
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
 `gatling` section of modules setting. Next options can be set:
 - `path`: Path to Gatling executable. In case no Gatling executable found, it will be automatically downloaded and installed into `path` location. By default `~/.bzt/gatling-taurus/{version}/bin/gatling.sh`.
 - `java-opts`: string with some java options for Gatling
 - `download-link`: Link to download Gatling from. By default: `https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip`
 -  `version`: Gatling version, `3.1.2` by default
 -  `dir-prefix`: Gatling report prefix, `gatling-%s` by default. Used by taurus to find gatling reports. If you use Gatling property `gatling.core.outputDirectoryBaseName`, you may use also this setting.
 - `properties`: dictionary for tuning of gatling tool behaviour (see list of available parameters in gatling
 documentation) and sending your own variables into Scala program:

```yaml
modules:
  gatling:
    properties:
      gatling.data.file.bufferSize: 512 # output buffer size, 256 bytes by default
      your_variable: 1024               # user variable
```
You can then read values of properties in Scala code similar to `[Load Configuration](#Load-Configuration)` style:

```scala
class BasicSimulation extends Simulation {
  val your_scala_variable = Integer.getInteger("your_variable", 0).toInt
}
```

You can also specify properties per-scenario, which will add to module-level properties:
```yaml
scenarios:
  my-scenario:
    properties:
      propname: value
      your_variable: 256
      
    # ...
```

## External Java Libraries Usage

Thanks to Taurus you can use additional Java classes in your scala code.
For that purpose add required jar files or contained dir to `additional-classpath` list:

```yaml
execution:
- executor: gatling
  concurrency: 10
  hold-for: 1h
  scenario: example
scenarios:
  example:
    script: my_file.scala
    additional-classpath:
    - deps/gson-1.0.1.jar
    - deps/common-utils-0.15.1.jar
modules:
  gatling:
    additional-classpath:
    - most-important-lib.jar  #   global way to specify required libraries
```
