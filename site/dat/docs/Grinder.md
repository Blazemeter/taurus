# Grinder Executor

## Notes
Grinder supports following taurus execution parameters: `concurrency`, `ramp-up`, `hold-for` and `iterations` (all except of `throughput`). As duration is sent to every worker, you'll get the same ramp-down together with `ramp-up`. When you configure limit of `iterations`, it applies to every worker so effective limit can be up to `concurrency` * `iterations` (it depends on start and shutdown processes).
All parameters are passed with .properties file, so they will work for your script too (not for generated script only). 
  
## Running Grinder
Running Grinder with existing script and properties file.
```yaml
execution:
- executor: grinder
  concurrency: 3
  ramp-up: 10s
  iterations: 20
  scenario: script_sample
  
scenarios:
  script_sample:
    script: tests/grinder/helloworld.py
    properties-file: tests/grinder/grinder.properties
    properties:
      grinder.useConsole: false
```

Generating Grinder script from scenario:
```yaml
execution:
- executor: grinder
  concurrency: 20
  ramp-up: 1m
  iterations: 100
  scenario: requests_sample

scenarios:
  requests_sample:
    default-address: http://blazedemo.com  # base address for requests
    think-time: 1s                         # pause for 1s after each request
    timeout: 30s                           # specify timeout for requests
    headers:                               # global headers, applied to all requests
      X-Api-Key: my-fresh-token
    store-cookie: false                    # simulate browser cookie storage (default value is `true`)
    keepalive: true                        # flag to use keep-alive for connections, default is `true`  
    requests:
    - /                                    # short form, URL only
    - url: /reserve.php                    # full form
      method: POST
      think-time: 5s
    - url: /payment.php
      method: POST
      headers:                             # request-specific headers
        Content-Type: application/json
      think-time: 1s                       # overrides scenario-level `think-time`
```

## Module settings
 - `path: "/somepath/folder/"` - Path to Grinder. If no grinder.jar found in folder/lib/, Grinder tool will be automatically downloaded and installed in "path". By default "~/.bzt/grinder-taurus/".
 - `download-link:"http://somehost/grinder-{version}-binary.zip"`  - Link to download Grinder. By default "http://switch.dl.sourceforge.net/project/grinder/The%20Grinder%203/{version}/grinder-{version}-binary.zip"
 -  `version: "3.11"` - Grinder version, by default "3.11"
 -  `report-by-url: false` - change results analysis to use URLs instead of test ID/test name

TODO: document how to access load properties from inside grinder

Thread ramp up doc: http://grinder.sourceforge.net/g3/script-gallery.html#threadrampup.py