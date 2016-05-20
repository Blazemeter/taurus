# JMeter Executor

This executor type is used by default, it uses [Apache JMeter](http://jmeter.apache.org/) as underlying tool.

## JMeter Location & Auto-Installation

If there is no JMeter installed at the configured `path`, Taurus will attempt to install latest JMeter and Plugins into
this location, by default `~/.bzt/jmeter-taurus/bin/jmeter`. You can change this setting to your preferred JMeter location (consider putting it into `~/.bzt-rc` file). All module settings that relates to JMeter path and auto-installing are listed below:
```yaml
---
modules:
  jmeter:
    path: ~/.bzt/jmeter-taurus/bin/jmeter
    download-link: http://apache.claz.org/jmeter/binaries/apache-jmeter-{version}.zip
    version: 2.13
    plugins-download-link: http://jmeter-plugins.org/files/JMeterPlugins-{plugin}-1.2.1.zip
```

## Run Existing JMX File
```yaml
---
execution:
- scenario:
    script: tests/jmx/dummy.jmx
```

or simply `bzt tests/jmx/dummy.jmx`

TODO: explain how multi-thread group will accept concurrency with maintained proportion

## JMeter Properties
There are two places to specify JMeter properties: global at module-level and local at scenario-level. Scenario properties are merged into global properties and resulting set comes as input for JMeter, see corresponding `.properties` file in artifacts.
You may also specify system properties for JMeter in system-properties section. They comes as system.properties file in artifacts.

Global properties are set like this:

```yaml
---
modules:
  jmeter:
    properties:
      my-hostname: www.pre-test.com
      log_level.jmeter: WARN
      log_level.jmeter.threads: DEBUG
    system-properties:
      sun.net.http.allowRestrictedHeaders: "true"
```

Scenario-level properties are set like this:

```yaml
---
execution:
- scenario: 
    properties:
        my-hostname: www.prod.com
        log_level.jmeter: DEBUG
```

## Open JMeter GUI
When you want to verify or debug the JMX file that were generated from your requests scenario, you don't need to search for the file on disk, just enable GUI mode for JMeter module:

```yaml
---
modules:
  jmeter:
    gui: false  # set it to true to open JMeter GUI instead of running non-GUI test
```

For the command-line, use alias `-gui` or option `-o modules.jmeter.gui=true`, without the need to edit configuration file.

## Run JMeter in Distributed Mode
Distributed mode for JMeter is enabled with simple option `distributed` under execution settings, listing JMeter servers under it:

```yaml
---
execution:
- distributed: 
  - host1.mynet.com
  - host2.mynet.com
  - host3.mynet.com
  scenario:
    script: my-test.jmx
```
For accurate load calculation don't forget to choose different hostname values for slave hosts. 

## Shutdown Delay
By default, Taurus tries to call graceful JMeter shutdown by using its UDP shutdown port (this works only for non-GUI). There is option to wait for JMeter to exit before killing it forcefully, called `shutdown-wait`. Bu default, its value is 5 seconds.

## Modifications for Existing Scripts

JMeter executor allows you to apply some modifications to the JMX file before running JMeter (this affects both existing JMXes and generated from requests):

```yaml
---
execution:
- scenario:
    script: tests/jmx/dummy.jmx
    variables: # add User Defined Variables component to test plan, 
               # overriding other global variables
      user_def_var: http://demo.blazemeter.com/api/user
      user_def_var2: user_def_val_2
    modifications:
        disable:  # Names of the tree elements to disable
        - Thread Group 1
        enable:  # Names of the tree elements to ensable
        - Thread Group 2
        set-prop:  # Set element properties, selected as [Element Name]>[property name]
          "HTTP Sampler>HTTPSampler.connect_timeout": "0"
          "HTTP Sampler>HTTPSampler.protocol": "https"
```

## Building Test Plan from Config

Scenario that has `requests` element makes Taurus to generate the script for underlying tools automatically. For now, this is available for JMeter and partially available for some other tools. 

The `requests` element must contain a list of requests, each with its settings and child elements (assertions, extractors). Also there are additional configuration elements for requests-based scenario, described below.

Scenario is the sequence of steps and some settings that will be used by underlying tools (JMeter, Grinder, Gatling) on execution stage. There is two ways to specify scenarios for executions: _inline in execution_ and _referred by alias_.

Inline form is useful for quick start and for single-config executions, full scenario is set in `scenario` item of `execution`:

```yaml
---
execution:
- scenario:
    # scenario is specified inline
    requests:
    - http://localhost/1
    - http://localhost/2
```

Referred form is useful when you use separate configs to store scenarios and executions, it is recommended for all cases. Scenarios are listed in top-level `scenarios` element and referred from executions by their alias:

```yaml
---
scenarios:
  get-requests:  # the alias for scenario
    requests:
    - http://localhost/1
    - http://localhost/2

execution:
- scenario: get-requests  # alias from above is used 
```


### Global Settings

Scenario has some global settings:

```yaml
---
scenarios:
  get-requests:  
    store-cache: true  # browser cache simulation, enabled by default
    store-cookie: true  # browser cookies simulation, enabled by default
    headers: # global headers
      header-name: header-value
    think-time: 1s500ms  # global delay between each request
    timeout: 500ms  #  timeout for connecting, receiving results
    default-address: "https://www.blazedemo.com:8080"  # http request defaults scheme, domain, port
    keepalive: true  # true by default, applied on all requests in scenario
    retrieve-resources: true  # true by default, retrieves all embedded resources from HTML pages
    concurrent-pool-size: 4  # concurrent pool size for resources download, 4 by default
    use-dns-cache-mgr: true  # use DNS Cache Manager to test resources 
                             # behind dns load balancers. True by default.
    force-parent-sample: true  # generate only parent sample for transaction controllers.
                               # True by default
    data-sources: # list of external data sources
    - path/to/my.csv  # this is a shorthand form
    - path: path/to/another.csv  # this is full form, path option is required
      delimiter: ';'  # CSV delimiter, auto-detected by default
      quoted: false  # allow quoted data
      loop: true  # loop over in case of end-of-file reached
```

Note that `timeout` also sets duration assertion that will mark response failed if response time was more than timeout.

### Requests

Request objects can be of two kinds:
1. Plain HTTP requests
2. Logic blocks that allow user to control execution flow of test session.

#### HTTP Requests
The base element for requests scenario is HTTP Request. In its simplest form it contains just the URL as string:

```yaml
---
scenarios:
  get-requests:  
    requests:
    - http://localhost/1
    - http://localhost/2
```

The full form for request is dictionary, all fields except `url` are optional:

```yaml
---
scenarios:
  my-req: 
    requests:
    - url: http://blazedemo.com/  # url to hit
      method: GET  # request method (GET, POST, PUT, DELETE)
      label: homepage  # sampler label

      body: 'request-body-string'  # if present, will be used as body 
      body:  # generate query string based on parameters and request type
        param1: value1
        param2: value2
      body-file: path/to/file.txt  # this file contents will be used as post body

      headers:  # local headers that override global
        Authentication: Token 1234567890
        Referer: http://taurus.blazemeter/docs
      think-time: 1s  # local think-time, overrides global
      timeout: 1s  # local timeout, overrides global

      extract-regexp: {}  # explained below
      extract-jsonpath: {}  # explained below
      assert: []  # explained below
```

##### Extractors

Extractors are the objects that attached to request to take a piece of the response and use it in following requests.
The concept is based on JMeter's extractors. The following types of extractors are supported:

- by regular expression
- by JSONPath expression
- by CSS/JQuery selectors
- by XPath query

To specify extractors in shorthand form, use following configuration:

```yaml
---
scenarios:
  my-req: 
    requests:
    - url: http://blazedemo.com/  
      extract-regexp: # dictionary under it has form <var name>: <regular expression>
        page_title: <title>(\w+)</title>  #  must have at least one capture group
      extract-jsonpath: # dictionary under it has form <var name>: <JSONPath expression>
        varname: $.jsonpath[0].expression
    - url: http://blazedemo.com/${varname}/${page_title}  # that's how we use those variables
      extract-css-jquery: # dictionary under it has form <var name>: <CSS/JQuery selector>
        extractor1: input[name~=my_input]
    - url: http://blazedemo.com/${varname}/${extractor1}.xml
      extract-xpath:
        title: /html/head/title
```

The full form for extractors is:

```yaml
---
scenarios:
  my-req: 
    requests:
    - url: http://blazedemo.com/  
      extract-regexp:
        page_title:
          regexp: <title>(\w+)</title>  # regular expression
          default: NOT_FOUND  # default value to use when regexp not found
          match-no: 1  # if multiple values has matched, which match use (0=random)
          template: 1  # which capture group to take, integer or template string
      extract-jsonpath:   
        varname:
          jsonpath: $.jsonpath[0]  # jsonpath expression
          default: NOT_FOUND  # default value to use when jsonpath not found
    - url: http://blazedemo.com/${varname}/${page_title}
      extract-css-jquery:
        extractor2:
          expression: input[name=JMeter]
          attribute: value
          match-no: 1
          default: NOT_FOUND
    - url: http://blazedemo.com/${varname}/${extractor2}.xml
      extract-xpath:
        destination:
          xpath: /order/client/address
          default: NOT_FOUND
          validate-xml: false
          ignore-whitespace: true
          use-tolerant-parser: false
```

##### Assertions

Assertions are attached to request elements and used to set fail status on the response. Fail status for the response is
not the same as response code for JMeter.
Currently three types of response assertions are available.

First one checks http response fields, its short form looks like this:

```yaml
---
scenarios:
  my-req: 
    requests:
    - url: http://blazedemo.com/  
      assert:  # contains list of regular expressions to check
      - .+App.+
```

The full form has following format:

```yaml
---
scenarios:
  my-req: 
    requests:
    - url: http://blazedemo.com/  
      assert:
       - contains:  # list of strings to check
         - .+App.+ 
         subject: body  # subject for search
         regexp: true  # treat string as regular expression
         not: false  # invert condition - fail if found
         assume-success: false  # mark sample successful before asserting it
```

Possible subjects are:
  - `body`
  - `headers`
  - `http-code`


The second assertion type is used to perform validation of JSON response against JSONPath expression.

```yaml
---
scenarios:
  my-req:
    requests:
    - url: http://blazedemo.com/
      assert-jsonpath:  # contains list of options
        - "$."  # if this JSONPath not found, assert will fail
        - "$.result[0]" # there can be multiple JSONPaths provided            
```

Full form:

```yaml
---
scenarios:
  my-req:
    requests:
    - url: http://blazedemo.com/
      assert-jsonpath:
      - jsonpath: "$." # path to value, validation fails if path not exists
        validate: true # validate against expected value
        expected-value: "value" # the value we are expecting to validate
        expect-null: false  # expected value is null
        invert: false # invert condition
```

And the third assertion type uses XPath query to validate XML response.

```yaml
---
scenarios:
  assertion-demo:
    requests:
    - url: http://blazedemo.com/
      assert-xpath:  # contains list of xpath queries
        - "/bookstore/book"  # if this XPath won't be matched, assert will fail
        - "/html/head/title" # you can provide multiple XPath queries
```

Full form:

```yaml
---
scenarios:
  my-req:
    requests:
    - url: http://blazedemo.com/
      assert-xpath:
      - xpath: "/html/head/title/text()='My title'" # query that compares XPath query result with some value
        use-tolerant-parser: false  # use error-tolerant XML parser
        ignore-whitespace: true # ignore whitespaces in XML (has no effect when `use-tolerant-parser` is true)
        validate: false # validate XML against its schema (has no effect when `use-tolerant-parser` is true)
        invert: false # invert condition
```


#### Logic Blocks


Taurus allows to control execution flow with the following constructs:
- `if` blocks
- `loop` blocks
- `while` blocks
- `foreach` blocks
- `transaction` blocks

##### If Blocks

`if` blocks allow conditional execution of HTTP requests.

Each `if` block should contain a mandatory `then` field, and an optional `else` field. Both `then` and `else` fields
should contain lists of requests.

Here's a simple example:

```yaml
scenario:
  variables:
    searchEngine: google
  requests:
  - if: '"${searchEngine}" == "google"'
    then:
      - https://google.com/
    else:
      - https://bing.com/
```

Note that Taurus compiles `if` blocks to JMeter's `If Controllers`, so `<condition>` must be in JMeter's format.


Logic blocks can also be nested:

```yaml
scenario:
  requests:
  - if: <condition1>
    then:
    - if: <condition2>
      then:
      - https://google.com/
      else:
      - https://yahoo.com/
    else:
    - https://bing.com/
```

And here's the real-world example of using `if` blocks:

```yaml
scenario:
  requests:
  # first request is a plain HTTP request that sets `status_code`
  # and `username` variables
  - url: https://api.example.com/v1/media/search
    extract-jsonpath:
      status_code: $.meta.code
      username: $.data.[0].user.username

  # branch on `status_code` value
  - if: '"${status_code}" == "200"'
    then:
      - https://example.com/${username}
```

##### Loop Blocks

`loop` blocks allow repeated execution of requests. Nested requests are to be specified with `do` field.

```yaml
scenario:
  requests:
  - loop: 10
    do:
    - http://blazedemo.com/
```

If you want to loop requests forever, you can specify string `forever` as `loop` value.
```yaml
scenario:
  requests:
  - loop: forever
    do:
    - http://blazedemo.com/
```

Note that `loop` blocks correspond to JMeter's `Loop Controllers`.

##### While Blocks

`while` block is similar to `while` loops in many programming languages. It allows conditional repeated execution of
requests. `while` blocks are compiled to JMeter's `While Controllers`.

```yaml
scenario:
  requests:
  - while: ${JMeterThread.last_sample_ok}
    do:
    - http://blazedemo.com/
```

##### Foreach Blocks

`foreach` blocks allow you to iterate over a collection of values. They are compiled to JMeter `ForEach Controllers`.

Syntax:
```yaml
requests:
- foreach: <elementName> in <collection>
  do:
  - http://${elementName}/
```

Concrete example:
```yaml
scenario:
  requests:
  - url: https://api.example.com/v1/media/search
    extract-jsonpath:
      usernames: $.data.[:100].user.username  # grab first 100 usernames
  - foreach: name in usernames
    do:
    - https://example.com/user/${name}
```

##### Transaction Blocks

`transaction` blocks allow wrapping http requests in a transaction. `transaction` blocks correspond to JMeter's
`Transaction Controllers`.

Example:
```yaml
scenario:
  requests:
  - transaction: Customer Session
    do:
    - http://example.com/shop
    - http://example.com/shop/items/1
    - http://example.com/shop/items/2
    - http://example.com/card
    - http://example.com/checkout
```

## JMeter Test Log
You can tune JTL file verbosity with option `write-xml-jtl`. Possible values are 'error' (default), 'full', or any other value for 'none'. Keep in mind: max verbosity can seriously load your system.
```yaml
---
execution
- write-xml-jtl: full
  scenario:
    script: my.jmx

```

## JMeter JVM Memory Limit

You can tweak JMeter's memory limit (aka, `-Xmx` JVM option) with `memory-xmx` setting.
Use `K`, `M` or `G` suffixes to specify memory limit in kilobytes, megabytes or gigabytes.

Example:
```yaml
---
execution
- executor: jmeter
  hold-for: 5m
  scenario:
    script: my_test.jmx
    
modules:
  jmeter:
    memory-xmx: 4G  # allow JMeter to use up to 4G of memory
```
