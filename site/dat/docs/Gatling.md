# Gatling Executor

Gatling is a load testing tool which is most famous as a choice for the testing of HTTP servers.

In Taurus, you have two ways how to run it: 
Either with a native gatling script, or with the usual Taurus features: `requests`,
`iterations`, etc. In the latter case, a scala script is generated automatically.

We support Gatling versions 3 or higher. Default is version 3.8.

## Run Gatling Tool

```yaml
execution:
- executor: gatling
  scenario: sample

scenarios:
  sample:
    script: tests/gatling/BasicSimulation.scala
    simulation: tests.gatling.BasicSimulation
```

The `simulation` option is the canonical class name for the main simulation class. 
It will be passed as-is to gatling with the `-s` option.

## Load Configuration

Taurus supports the possibility to send the values of the execution options 
`concurrency`, `iterations`, `ramp-up` `hold-for`, and `throughput` 
to the Gatling test script. Below you can see how you can use these parameters 
on the Gatling side to set up your test:

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
  val httpConf = http.baseUrl("http://blazedemo.com/")

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
    .inject(rampUsers(t_concurrency) during t_rampUp)
    .protocols(httpConf)

  setUp(execution).
    throttle(jumpToRps(t_throughput), holdFor(t_holdFor)).
    maxDuration(t_rampUp + t_holdFor)
}
```

## Running Complex Gatling Test Suites

If your Gatling test suite is really huge or has dependencies on other files - you can bundle it in a jar
(with the help of sbt or Maven) and then run this jar with Taurus. Just specify it as the `script` value in the scenario.

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

If your scenario doesn't contain the `script` parameter and contains at least one element of `requests`,
Taurus will build a scala script for the test. This script will be placed in `[artifact-dir](ConfigSyntax/#Top-Level-Settings)`:
You can modify it and use it with Gatling later.

The following features are supported: 
  - request generation
  - on scenario level: `think-time`, `default-address`, `follow-redirects`, `headers`, `store-cache`, `keepalive`, 
  `timeout`, `retrieve-resources`, `retrieve-resources-regex`
  - on request level: `think-time`, `body`
  - params that are described in `[Load Configuration](#Load-Configuration)`.
Some Asserts can be added to request. An Assert describes templates and an area for search (`contains` and `subject` accordingly), regexp, and inverse marks. You can look for a particular response code in the `http-code` part or for string,
and for a regular expression in the `body` of request.

The following yaml example shows the way these features can be used and made ready for the automatic conversion to scala:

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
    data-sources:  # this is a list of data-sources options for Gatling. See more info below.
    - path: buyouts.csv 
      delimiter: ','
      loop: true
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
    - set-variables:  # setting variables in runtime
        var1: val1
        var2: val2
```
See more info about [data-sources here](DataSources.md).

### Extractors

Extractors are the objects that are attached to the request to take a piece of the response and use it in the following requests.
The following types of extractors are supported:

- by regular expression
- by JSONPath expression
- by CSS/JQuery selectors
- by XPath query

To specify extractors in shorthand form, use the following configuration:

```yaml
scenarios:
  my-req:
    requests:
    - url: http://blazedemo.com/
      extract-regexp: # dictionary under it has form <var name>: <regular expression>
        page_title: <title>(\w+)</title>  #  must have at least one capture group
      extract-jsonpath: # dictionary under it has form <var name>: <JSONPath expression>
        varname: $.jsonpath[0].expression
    - url: http://blazedemo.com/${varname_1}/${page_title}  # that's how we use those variables
      extract-css-jquery: # dictionary under it has form <var name>: <CSS/JQuery selector>
        extractor1: input[name~=my_input]
    - url: http://blazedemo.com/${varname}/${extractor1}.xml
      extract-xpath:
        title: /html/head/title
```

The full form for extractors is:

```yaml
scenarios:
  my-req:
    requests:
    - url: http://blazedemo.com/
      extract-regexp:
        page_title:
          regexp: <title>(\w+)</title>  # regular expression
          default: NOT_FOUND  # default value to use when regexp not found
      extract-jsonpath:
        varname:
          jsonpath: $.jsonpath[0]  # jsonpath expression
          default: NOT_FOUND  # default value to use when jsonpath not found
    - url: http://blazedemo.com/${varname}/${page_title}
      extract-css-jquery:
        extractor2:
          expression: input[name=JMeter]
    - url: http://blazedemo.com/${varname}/${extractor2}.xml
      extract-xpath:
        destination:
          xpath: /order/client/address
          default: NOT_FOUND
    - url: http://blazedemo.com/${varname}.xml
```

### Include Scenario Blocks
The `include-scenario` block makes it possible to include one scenario in another one. You can use it to split your test plan into
several independent scenarios that can be reused. In addition, an `include-scenario` can refer to another scenario which contains `include-scenario` on any level of depth.

Example:
```yaml
scenarios:
  login:
    data-sources:
    - logins.csv
    requests:
    - url: http://example.com/login
      method: POST
      body:
        user: ${username}
        password: ${password}
  logout:
    requests:
    - url: http://example.com/logout
      method: POST
  shop-session:
    requests:
    - include-scenario: login
    - http://example.com/shop/items/1
    - http://example.com/checkout
    - include-scenario: logout
```

```yaml

execution:
- executor: gatling
  scenario: shop-session

scenarios:
  shop-session:
    requests:
    - include-scenario: login
    - http://example.com/shop/items/1
    - http://example.com/checkout
    - include-scenario: logout

included-configs:  # it must be a list of string values
- ./login.yml 
```

## Configuration Options

Similar to other modules, you can define a global configuration for the Gatling Executor 
by writing certain lines in the `gatling` section of the modules setting. 
 
 The following options are supported:
 - `path`: Path to the Gatling executable. In case no Gatling executable is found, it will be automatically downloaded and installed into the `path` location. By default, `~/.bzt/gatling-taurus/{version}/bin/gatling.sh`.
 - `java-opts`: A string with some Java options for Gatling
 - `maven-opts`: A string with some Maven options for Gatling 3.11+.
 - `download-link`: A link where to download Gatling from. By default: `https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip`
 -  `version`: The Gatling version, for example, `3.14.3`.
 -  `dir-prefix`: The Gatling report prefix, `gatling-%s` by default. Used by taurus to find gatling reports. If you use the Gatling property `gatling.core.outputDirectoryBaseName`, you can also use this setting.
 - `properties`: The dictionary for tuning the gatling tool behaviour and sending your own variables into Scala program. For the list of available parameters, see the gatling documentation. 

```yaml
modules:
  gatling:
    properties:
      gatling.data.file.bufferSize: 512 # output buffer size, 256 bytes by default
      your_variable: 1024               # user variable
```
You can then read values of properties in Scala code similar to the `[Load Configuration](#Load-Configuration)` style:

```scala
class BasicSimulation extends Simulation {
  val your_scala_variable = Integer.getInteger("your_variable", 0).toInt
}
```

You can also specify properties per scenario, which will be added to the module-level properties:
```yaml
scenarios:
  my-scenario:
    properties:
      propname: value
      your_variable: 256
      
    # ...
```

## External Java Libraries Usage

For gatling 3.8 and newer, the `additional-classpath` option is no longer supported. As a workaround, copy additional libraries into the user lib folder under `GATLING_HOME/user-files/lib`.

For older gatling versions, you can use additional Java classes in your scala code.
For that purpose, add the required jar files or the contained dir to the `additional-classpath` list, like this:

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
    - most-important-lib.jar  #  a global way to specify required libraries
```

## How to define Maven options (Gatling 3.11+)

If you are using Gatling version 3.11 or higher, specify the version in your yml config.
If you need to define settings for Maven (for example, memory increase) in these versions, 
use the property `maven-opts`, not `java-opts`.

```
execution:
  - executor: gatling  # Gatling
    concurrency: 5
    hold-for: 20s
    iterations: 1
    throughput: 3
    scenario:
      script: taurusExamples.jar
      simulation: com.example.TaurusSimulationUrl
      properties:
        url: https://dummyurl/test
        username: admin
        password: password
modules:
  gatling:
    version: 3.14.3                   # specify the version
    java-opts: "-Dtest=value"         # specify Java options
    maven-opts: "-Xms512m -Xmx512m"   # specify Maven options
    properties:
      your_variable: 1024
      targetUrl: "https://vs196680svc355399.mock.blazemeter.com/test"
```