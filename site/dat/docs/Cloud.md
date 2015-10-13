# Cloud Provisioning

The default mode for taurus is to use `local` provisioning, which means all the tools will be started on local machine. This is not much scalable, so there is a way to delegate actual tool execution into BlazeMeter cloud. It is done by setting `cloud` provisioning like this:

```yaml
---
provisioning: cloud
```

To access BlazeMeter cloud, Taurus would require to have API key set inside `cloud` module settings:
```yaml
---
modules:
  cloud:
    token: ******  # API key
    timeout: 1s  # BlazeMeter API client timeout
    browser-open: start  # auto-open browser on test start/end/both/none
```

## Load Settings for Cloud

By default, cloud-provisioned execution will read `concurrency` and `throughput` options normally. There's a notation that allows configuring values for `local` and `cloud` at once, to remove the need to edit load settings when switching `provisioning` during test debugging from `local` to `cloud` and back:

```yaml
---
execution:
  - scenario: my-scen
    concurrency:
      local: 5
      cloud: 5000
    throughput:
      local: 100
      cloud: 10000
```

Then you can just switch `provisioning` and load settings will be taken accordingly. For example, running `bzt config.yml -o provisioning=cloud` is an easy way to toggle on `cloud` provisioning. The `concurrency` and `througput` are always *total* value for execution, no matter how many locations will be involved.

## Configuring Cloud Locations

Cloud locations are specified per-execution. Specifying multiple cloud locations for execution means that its `concurrency` will be distributed among the locations. Locations is the map of location id's and their relative weights.

```yaml
---
execution:
  - locations:
      eu-west: 1
      eu-east: 2
```

weighted vs absolute vs no concurrency set

list of available locations is at https://a.blazemeter.com/api/latest/user

logic to get default location


when no duration provided

Don't need and should not use `blazemeter` reporting module