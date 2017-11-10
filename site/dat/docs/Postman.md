# Postman (Newman) Executor

Executor type `newman` allows running [Postman collections](https://www.getpostman.com/docs/postman/collections/creating_collections). It is mostly useful for functional testing. Internally, [Newman](https://github.com/postmanlabs/newman) utility is used to run tests, providing 100% Postman compatibility.

Simplest example to run a Postman collection:
```yaml
execution:
- executor: newman
  iterations: 1
  scenario: functional/postman-sample-collection.json
```

Supported features:
* run existing collection
* perform several iterations over same collection
* specify environment variables via YAML config file
* specify global variables via YAML config file

Concurrency, ramp-up and hold-for options are not supported at the moment.

## Timeouts and Pauses Between Requests
Two options `timeout` and `think-time` enable you to specify timeout for requests, and also a pause between requests to throttle down the load.

```yaml
execution:
- executor: newman
  scenario: simple

scenarios:
  simple:
    script: functional/postman-sample-collection.json
    timeout: 1s
    think-time: 0.5s
```

## Specifying Environment and Global Variables

To specify environment or global variables, you need to provide `environment` and/or `globals` item under scenario description section. If the value of option is string, it is treated as path to JSON file containing variables description. Otherwise, it is interpreted as key/value pairs.

```yaml
execution:
- executor: newman
  scenario: simple

scenarios:
  simple:
    script: functional/postman-sample-collection.json
    environment: path/to/environment.json
    globals:
      glob1: val1
      glob2: val2
```