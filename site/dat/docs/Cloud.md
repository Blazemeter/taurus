# Cloud Provisioning

The default mode for taurus is to use `local` provisioning, which means all the tools will be started on local machine. This is not much scalable, so there is a way to delegate actual tool execution into [BlazeMeter cloud](http://blazemeter.com/). Even free accounts can execute cloud tests, according to BlazeMeter's free-tier plan.

It is done by setting `cloud` provisioning like this:

```yaml
provisioning: cloud
```

To access BlazeMeter cloud, Taurus would require to have API key and secret set inside `cloud` module settings:
```yaml
modules:
  cloud:
    token: ******:**************  # API id and API secret divided by :
    timeout: 10s  # BlazeMeter API client timeout
    browser-open: start  # auto-open browser on test start/end/both/none
    check-interval: 5s  # interval which Taurus uses to query test status from BlazeMeter
    public-report: false  # make test report public, disabled by default
```

All folders among your resource files (scripts) will be packed automatically before sending and unpacked on cloud workers with `unpacker` service.   

<div class="alert alert-danger">
Never put API key into your main config files! 

Never post it to support forums!

It is recommended to place the token setting in your personal
[per-user config](CommandLine.md#configuration-files-processing) `~/.bzt-rc` to prevent it from
being logged and collected in artifacts.
</div>

## Load Settings for Cloud

By default, cloud-provisioned execution will read `concurrency` and `throughput` options normally. There's a notation that allows configuring values for `local` and `cloud` at once, to remove the need to edit load settings when switching `provisioning` during test debugging from `local` to `cloud` and back:

```yaml
execution:
- scenario: my-scen
  concurrency:
    local: 5
    cloud: 5000
  throughput:
    local: 100
    cloud: 10000
```

Then you can just switch `provisioning` and load settings will be taken accordingly. For example, running `bzt config.yml -o provisioning=cloud` is an easy way to toggle on `cloud` provisioning. Short form `bzt config.yml -cloud` is available and contrariwise you can turn off cloud provisioning by the similar way: `bzt config.yml -local`   
The `concurrency` and `througput` are always *total* value for execution, no matter how many locations will be involved.

## Detach Mode
You can start Cloud test and stop Taurus without waiting for test results with attribute `detach`:
```yaml
modules:
  cloud:
    token: ******    
    detach: true # run cloud test and exit    
```
or use appropriate alias for this: `bzt config.yml -cloud -detach`

## Configuring Cloud Locations

Cloud locations are specified per-execution. Specifying multiple cloud locations for execution means that its `concurrency` and/or `throughput` will be distributed among the locations. Locations is the map of location id's and their relative weights. Relative weight determines how much value from `concurrency` and `throughput` will be put into corresponding location. 

```yaml
execution:
- locations:
    us-west-1: 1
    us-east-1: 2
```

If no locations specified for cloud execution, default value from `modules.cloud.default-location` is taken with weight of 1. To get the list of all available locations, run `bzt -locations -o modules.cloud.token=<API Key>`. The list of available locations is taken from [User API Call](https://a.blazemeter.com/api/latest/user) and may be specific for particular user. See `locations` block and `id` option for each location.

By default, Taurus will calculate machines count for each location based on their limits obtained from *User API Call*. To switch to manual machines count just set option `locations-weighted` into `false`. Exact numbers of machines for each location will be used in that case:

```yaml
execution:
- locations:
    us-west-1: 2
    us-east-1: 7
  locations-weighted: false
```

```yaml
execution: 
  - scenario: dummy 
    concurrency:
      local: 5
      cloud: 1000
    ramp-up: 10s
    hold-for: 5m
    locations: 
      eu-central-1: 1
      eu-west-1: 1
      us-east-1: 1
      us-west-1: 1
      us-west-2: 1
provisioning: cloud

scenarios:
  dummy:
    script: Dummy.jmx    
```

## Reporting Settings

```yaml
modules:
  cloud:
    test: Taurus Test  # test name
    report-name: full report    # name of report
    project: Project Name  # project name or id
```

## Deleting Old Test Files

By default, Taurus will delete all test files from the cloud before uploading any new ones. You can disable
this behaviour by setting `delete-test-files` module setting to `false`.

Example:
```yaml
modules:
  cloud:
    delete-test-files: false
```

## Specifying Additional Resource Files
If you need some additional files as part of your test and Taurus fails to detect them automatically, you can attach them to execution using `files` section:

```yaml
execution:
- locations:
    us-east-1: 1
  scenario: test_sample    
  files:
  - path/to/file1.csv
  - path/to/file2.csv
  
scenarios:
  test_sample:
    script: testplan.jmx  
```


## Specifying Where to Run for Shellexec Service

In shellexec service, the `run-at` parameter allows to set where commands will be executed. Surprisingly, `local` means the cloud worker will execute it, `cloud` means the controlling CLI will execute it.


## Installing Python Package Dependencies

If you need to install additional python modules via `pip`, you can do it by using `shellexec` service and running `pip install <package>` command at `prepare` stage:

```yaml
services:
- module: shellexec
  prepare: 
  - pip install cryptography  # 'cryptography' is the library from PyPi
```

You can even upload your proprietary python eggs into workers by specifying them in `files` option and then installing by shellexec:

```yaml
execution:
- executor: locust
  scenario: locust-scen
  files:
  - my-modules.zip
        
services:
- module: shellexec
  prepare: 
  - unzip my-modules.zip
  - pip install -r requirements.txt
```

## Enabling Delayed Start Feature

This tells the server not to start each engine as soon as it's ready, but rather to wait until all engines are ready, then start them all at once:
```yaml
modules:
  blazemeter:
    delayed-start: true

## Enabling Dedicated IPs Feature

When your account in BlazeMeter allows you to use "Dedicated IPs" feature, you can enable it by setting in config file:
```yaml
modules:
  blazemeter:
    dedicated-ips: true
```

## Worker Number Info

There is a way to obtain worker index which can be used to coordinate distributed test data. For example, you can make sure that different workers will use different user logins or CSV file parts. To achieve that, you get some `env` variables for `shellexec` modules and some `properties` for `jmeter` module:

  * `TAURUS\_INDEX\_ALL` - absolute worker index in test
  * `TAURUS\_INDEX\_EXECUTION` - per-execution worker index
  * `TAURUS\_INDEX\_LOCATION` - per-location worker index

## Cloud Execution Notes

Please note that for `cloud` provisioning actual Taurus execution will be done on remote machines, so:
  * the test will not run if your account has no enough engines allowed
  * if you don't specify any duration for test with `hold-for` and `ramp-up` options, some default duration limit will be used
  * you should not use `-report` commmand-line option or `blazemeter` reporter, all reports will be collected automatically by BlazeMeter
  * only following config sections are passed into cloud: `scenarios`, `execution`, `services`
  * `shellexec` module has `artifacts-dir` set as `default-cwd`
  * cloud workers execute Taurus under isolated [virtualenv](https://virtualenv.readthedocs.org/en/latest/)
