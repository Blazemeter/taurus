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
    download-link: https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-{version}.zip
    version: 3.0
    plugins:
    - jpgc-json=2.2
    - jmeter-ftp
    - jpgc-casutg
```
`plugins` option lets you describe list of JMeter plugins you want to use. If `plugins` option isn't found only following plugins will be installed: jpgc-casutg, jpgc-dummy, jpgc-ffw, jpgc-fifo, jpgc-functions, jpgc-json, jpgc-perfmon, jpgc-prmctl, jpgc-tst. Keep in mind: you can change plugins list only for clean installation. If you already have JMeter placed at `path` you need to remove it for plugins installation purpose.  
    
## Run Existing JMX File
```yaml
---
execution:
- scenario: simple

scenarios:
  simple:
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
scenarios:
  prop_example: 
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
  scenario: some_scenario
  
scenarios:
  some_scenario:
    script: my-test.jmx
```
For accurate load calculation don't forget to choose different hostname values for slave hosts. 

## Shutdown Delay
By default, Taurus tries to call graceful JMeter shutdown by using its UDP shutdown port (this works only for non-GUI). There is option to wait for JMeter to exit before killing it forcefully, called `shutdown-wait`. By default, its value is 5 seconds.

## Modifications for Existing Scripts

JMeter executor allows you to apply some modifications to the JMX file before running JMeter (this affects both existing JMXes and generated from requests):

```yaml
---
scenarios:
  modification_example:
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
If selector for set-prop isn't found, taurus tries to create stringProp jmx element with last element of selector as name and sets given value for it. So you can create simple properties in jmx if it's necessary.

## Building Test Plan from Config

Scenario that has `requests` element makes Taurus to generate the script for underlying tools automatically. For now, this is available for JMeter and partially available for some other tools. 

The `requests` element must contain a list of requests, each with its settings and child elements (assertions, extractors). Also there are additional configuration elements for requests-based scenario, described below.

Scenario is the sequence of steps and some settings that will be used by underlying tools (JMeter, Grinder, Gatling) on execution stage. 

Scenarios are listed in top-level `scenarios` element and referred from executions by their alias:

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
    retrieve-resources-regex: ^((?!google|facebook).)*$  # regular expression used to match any resource
                                                         # URLs found in HTML document against. Unset by default
    concurrent-pool-size: 4  # concurrent pool size for resources download, 4 by default
    use-dns-cache-mgr: true  # use DNS Cache Manager to test resources
                             # behind dns load balancers. True by default.
    force-parent-sample: true  # generate only parent sample for transaction controllers.
                               # True by default
    content-encoding: utf-8  # global content encoding, applied to all requests.
                             # Unset by default
    follow-redirects: true  # follow redirects for all HTTP requests
    data-sources: # list of external data sources
    - path/to/my.csv  # this is a shorthand form
    - path: path/to/another.csv  # this is full form, path option is required
      delimiter: ';'  # CSV delimiter, auto-detected by default
      quoted: false  # allow quoted data
      loop: true  # loop over in case of end-of-file reached if true, stop thread if false
```

Note that `timeout` also sets duration assertion that will mark response failed if response time was more than timeout.

If you want to use JMeter properties in `default-address`, you'll have to specify mandatory scheme and separate address/port. Like this: `default-address: https://${__P(hostname)}:${__P(port)}`.

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

      upload-files:  # attach files to form (and enable multipart/form-data)
      - param: summaryReport  # form parameter name
        path: report.pdf  # path to file
        mime-type: application/pdf  # optional, Taurus will attempt to guess it automatically

      headers:  # local headers that override global
        Authentication: Token 1234567890
        Referer: http://taurus.blazemeter/docs
      think-time: 1s  # local think-time, overrides global
      timeout: 1s  # local timeout, overrides global
      content-encoding: utf-8  # content encoding (at JMeter's level), unset by default
      follow-redirects: true  # follow HTTP redirects

      extract-regexp: {}  # explained below
      extract-jsonpath: {}  # explained below
      assert: []  # explained below
      jsr223: []  # explained below
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
        subject: body                     #  subject for search
      extract-jsonpath: # dictionary under it has form <var name>: <JSONPath expression>
        varname: $.jsonpath[0].expression
    - url: http://blazedemo.com/${varname}/${page_title}  # that's how we use those variables
      extract-css-jquery: # dictionary under it has form <var name>: <CSS/JQuery selector>
        extractor1: input[name~=my_input]
    - url: http://blazedemo.com/${varname}/${extractor1}.xml
      extract-xpath:
        title: /html/head/title
```

Possible subjects for regexp are:
  - `body`
  - `headers`
  - `http-code`

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

##### JSR223 Blocks

Sometimes you may want to use a JSR223 Pre/Post Processor to execute a code block before or
after each request. Taurus allows that with `jsr223` block.

Minimal example that will generate one JSR223 Post Processor.
```yaml
---
scenarios:
  jsr-example:
    requests:
    - url: http://blazedemo.com/
      jsr223:
        language: javascript  # required field
        script-file: postproc.js  # required field
```

Each jsr223 element can define the following fields:
- `language` - script language ('beanshell', 'bsh', 'ecmascript', 'groovy', 'java', 'javascript', 'jexl', 'jexl2')
- `script-file` - path to script file
- `parameters` - string of parameters to pass to script, empty by default
- `execute` - whether to execute script before or after the request

If `execute` field is set to `after` - Taurus will generate a JSR223 PostProcessor, if set to `before` - a PreProcessor.
By default it's set to `after`.

Full form:
```yaml
---
scenarios:
  jsr-example:
    requests:
    - url: http://blazedemo.com/
      jsr223:
      - language: javascript
        script-file: preproc.js
        parameters: foo bar
        execute: before
      - language: beanshell
        script-file: postproc.bsh
        execute: after
```

#### Logic Blocks


Taurus allows to control execution flow with the following constructs:
- `if` blocks
- `loop` blocks
- `while` blocks
- `foreach` blocks
- `transaction` blocks
- `include-scenario` blocks
- `action` blocks

##### If Blocks

`if` blocks allow conditional execution of HTTP requests.

Each `if` block should contain a mandatory `then` field, and an optional `else` field. Both `then` and `else` fields
should contain lists of requests.

Here's a simple example:

```yaml
scenarios:
  if_example:
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
scenarios:
  nested_example:
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
scenarios:
  complex:
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
scenarios:
  loop_example:
    requests:
    - loop: 10
      do:
      - http://blazedemo.com/
```

If you want to loop requests forever, you can specify string `forever` as `loop` value.
```yaml
scenarios:
  forever_example:
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
scenarios:
  while_example:
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
scenarios:
  complex_foreach:
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
scenarios:
  transaction_example:
    requests:
    - transaction: Customer Session
      do:
      - http://example.com/shop
      - http://example.com/shop/items/1
      - http://example.com/shop/items/2
      - http://example.com/card
      - http://example.com/checkout
```

##### Include Scenario blocks
`include-scenario` block allows you to include scenario in another one. You can use it to split your test plan into
a few of independent scenarios that can be reused.

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

Taurus translates each `include-scenario` block to a JMeter's `Simple Controller` and puts all scenario-level
settings and requests there.

##### Action Blocks

`action` block allows you to specify a thread-specific action that will be performed. You can use it to pause or stop
the current thread, or force it to go to the next loop iteration.

The following actions are available:
- `pause` - pause the target thread (pause duration is controlled with `pause-duration` field)
- `stop` - stop the target thread gracefully
- `stop-now` - stop the test without waiting for samples to complete
- `continue` - send target thread to the next iteration of the loop

Actions can be applied to the following targets:
- `current-thread` - set by default
- `all-threads` - action will be applied to all threads (unavailable for `continue` action)

Examples:
```yaml
scenarios:
  action_example:
    requests:
    - action: pause
      target: current-thread
      pause-duration: 1s500ms
    - action: stop-now
      target: all-threads
```

## JMeter Test Log
You can tune JTL file content with option `write-xml-jtl`. Possible values are 'error' (default), 'full', or any other value for 'none'. Keep in mind: max `full` logging can seriously load your system.
```yaml
---
execution:
- write-xml-jtl: full
  scenario: simple_script
  
scenarios:
  simple_script:
    script: my.jmx

```
Another way to adjust verbosity is to change flags in `xml-jtl-flags` dictionary. Next example shows all flags with default values (you don't have to use full dictionary if you want to change some from them):
```yaml
---
modules:
  jmeter:
    xml-jtl-flags:
      xml: true
      fieldNames: true
      time: true
      timestamp: true
      latency: true
      success: true
      label: true
      code: true
      message: true
      threadName: true
      dataType: true
      encoding: true
      assertions: true
      subresults: true
      responseData: false
      samplerData: false
      responseHeaders: true
      requestHeaders: true
      responseDataOnError: true
      saveAssertionResultsFailureMessage: true
      bytes: true
      threadCounts: true
      url: true      
```

Remember: some logging information might be used by `[assertions](#Assertions)` so change log verbosity can affect them. 

## JMeter JVM Memory Limit

You can tweak JMeter's memory limit (aka, `-Xmx` JVM option) with `memory-xmx` setting.
Use `K`, `M` or `G` suffixes to specify memory limit in kilobytes, megabytes or gigabytes.

Example:
```yaml
---
modules:
  jmeter:
    memory-xmx: 4G  # allow JMeter to use up to 4G of memory
```