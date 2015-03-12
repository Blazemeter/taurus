# Scenario Building

Scenario is the sequence of steps and some settings that will be used by underlying tools (JMeter, Grinder, Gatling) on execution stage. There is two ways to specify scenarios for executions: _inline in execution_ and _referred by alias_.

Inline form is useful for quick start and for single-config executions, full scenario is set in `scenario` item of `execution`:

```yaml
---
execution:
  scenario:
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
  scenario: get-requests  # alias from above is used 
```

## Existing Script Scenario

If you have pre-existing script for your tool, you can use it as a scenario like this:
```yaml
execution:
  scenario:
    script: tests/jmx/dummy.jmx
```

Read the [execution settings](ExecutionSettings.md) section for you tool to learn additional options available with existing scripts.

## Requests Scenario

Scenario that has `requests` element makes Taurus to generate the script for underlying tools automatically. For now, this is available for JMeter, partially available for Grinder and not available for Gatling. You may contribute your efforts in supporting requests-scenarios for your favorite tool by discussing this on [project forums](https://groups.google.com/forum/#!forum/codename-taurus).

The `requests` element must contain a list of requests, each with its settings and child elements (assertions, extractors). Also there are additional configuration elements for requests-based scenario, described below.

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
    data-sources: # list of external data sources
      - path/to/my.csv  # this is a shorthand form
      - path: path/to/another.csv  # this is full form, path option is required
        delimiter: ';'  # CSV delimiter, autodetected by default
        quoted: false  # allow quoted data
        loop: true  # loop over in case of end-of-file reached
```

Note that `timeout` also sets duration assertion that will mark response failed if response time was more than timeout.

### HTTP Requests

The base element for requests scenario is HTTP Request. In its simpliest form it contains just URL as string:

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
        body-file: path/to/file.txt  # this file contents will be used as post body

        headers: {}  # local headers that override global
        think-time: 1s  # local think-time, overrides global
        timeout: 1s  # local timeout, overrides global

        extract-regexp: {}  # explained below
        extract-jsonpath: {}  # explained below
        assert: []  # explained below
```

### Extractors

Extractors are the objects that attached to request to take a piece of the response and use it in following requests. The concept is based on JMeter's extractors. Right now, two types of the extractors are supported: by regular expression and by JSONPath expression. To specify extractors in shorthand form, use following config:

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
      - http://blazedemo.com/${varname}/${page_title}  # that's how we use those variables
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
            template: 1  # which capture group to take
        extract-jsonpath:   
          varname:
            jsonpath: $.jsonpath[0]  # jsonpath expression
            default: NOT_FOUND  # default value to use when jsonpath not found
      - http://blazedemo.com/${varname}/${page_title}  
```

### Assertions

Assertions are attached to request elements and used to set fail status on the response (it is not the same as response code). Currently only one type of response assertion is available. Its short form looks like this:

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
```

Possible subjects are:
  - `body`
  - `headers`
  - `http-code`
