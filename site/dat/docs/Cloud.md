# Cloud Provisioning

The default mode for taurus is to use `local` provisioning, which means all the tools are started on the local machine. This is not very scalable, so there is a way to delegate actual tool execution to the [BlazeMeter cloud](http://blazemeter.com/). Even free accounts can execute cloud tests, according to BlazeMeter's free-tier plan.

To delegate execution to the cloud, set up `cloud` provisioning in the YAML file like this:

```yaml
provisioning: cloud
```

To access the BlazeMeter cloud, Taurus requires an API key and secret which you have to configure in the `cloud` module settings:
```yaml
modules:
  cloud:
    token: '******:**************' # API id and API secret divided by :
    timeout: 10s                   # BlazeMeter API client timeout
    browser-open: start            # auto-open browser on test start/end/both/none
    check-interval: 5s             # interval which Taurus uses to query test status from BlazeMeter
    public-report: false           # make test report public, disabled by default
    send-report-email: false       # send report email once test is finished, disabled by default
    request-logging-limit: 10240   # use this to dump more of request/response data into logs, for debugging
```

All folders among your resource files (scripts) are packed automatically before sending and unpacked on the cloud workers using the `unpacker` service.

<div class="alert alert-danger">
Never put the API key into your main config files!

Never post it to support forums!

Place the token setting in your personal
[per-user config](CommandLine.md#configuration-files-processing) `~/.bzt-rc` file to prevent it from
being logged and collected in artifacts!
</div>

## Load Settings for the Cloud

By default, cloud-provisioned execution reads `concurrency` and `throughput` options normally. There's a notation that allows configuring values for `local` and `cloud` at once, to remove the need to edit load settings when switching `provisioning` during test debugging from `local` to `cloud` and back:

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

Then you can just switch `provisioning`, and the load settings are taken accordingly. For example, running `bzt config.yml -o provisioning=cloud` is an easy way to toggle on `cloud` provisioning. The short form `bzt config.yml -cloud` is available, and conversely, you can turn off cloud provisioning in the similar way: `bzt config.yml -local`   
The `concurrency` and `througput` are always the *total* value for execution, no matter how many locations are involved.

## Modules Settings

There are some rules for sending the test config to cloud machines.
Taurus cleans up and removes unused modules and user-specific classes (as there aren't any such classes in Cloud by default).
To suppress this behaviour, you can use `send-to-blazemeter` parameter.

```yaml
execution:
- executor: jmeter
  iterations: 10
  files:
  - my_own.py
  scenario:
    requests:
    - http://blazedemo.com
modules:
  jmeter:
    send-to-blazemeter: true    # keep class value for jmeter module
    class: my_own.JMeterClass
    path: /path/to/local/jmeter.sh
  unused_mod:                   # will be removed
    class: some.class.Name
```

## Specifying Account, Workspace and Project

Accounts, Workspaces, and Projects are BlazeMeter features that help to exactly specify the access rights and support
shared access to tests and BlazeMeter features. You can learn more about Workspaces and Projects from BlazeMeter docs, e.g.
an article [Workspaces and Projects](https://help.blazemeter.com/docs/guide/administration-workspaces-and-projects.html).

With Taurus, it is possible to specify both names and identifiers for all entities listed.

Example:
```yaml
execution:
- scenario: my-scenario

scenarios:
  my-scenario:
    requests:
    - http://blazedemo.com/
    
modules:
  cloud:
    account: My Account  # numeric identifier can also be specified
    workspace: Shared Workspace
    project: Taurus tests
    test: Example test
```

If the test can be resolved (when account, workspace, project and test do exist) — then the test is
updated with the provided Taurus configuration and then the test is launched.

If the cloud test doesn't exist, it is created and launched.

By default, Taurus will use the default user's account and the default workspace, so it's not required to specify
account, workspace, and project every time.

There's also a useful shortcut that allows to specify all parameters at once by using a link to an existing BlazeMeter test:

```yaml
execution:
- scenario: my-scenario

scenarios:
  my-scenario:
    requests:
    - http://blazedemo.com/
    
modules:
  cloud:
    test: https://a.blazemeter.com/app/#/accounts/99/workspaces/999/projects/9999/tests/99999
```

## Launching Existing Cloud Tests

Taurus provides a way to launch pre-configured cloud tests by their name or id. This is the default behaviour
of cloud provisioning when the `execution` section is empty.

This configuration launches the cloud test named "Taurus Test" and await for its execution:
```yaml
provisioning: cloud

modules:
  cloud:
    test: Taurus Test
    launch-existing-test: true  # you can omit this field if your `execution` section is empty
```

Just like in the previous section, it is possible to specify `account`, `workspace` and other fields and
to use identifiers.

It's also possible to use the link to the test to launch it:
```yaml
provisioning: cloud

modules:
  cloud:
    test: https://a.blazemeter.com/app/#/accounts/99/workspaces/999/projects/9999/tests/99999
```

It also makes it possible to launch a cloud test with a single command line command:
```bash
$ bzt -cloud -o modules.cloud.test=https://a.blazemeter.com/app/#/accounts/97961/workspaces/89846/projects/132315/tests/5817816
```

## Detach Mode

You can start the Cloud test and stop Taurus without awaiting test results with the `detach` attribute:
```yaml
modules:
  cloud:
    token: '******'    
    detach: true  # launch cloud test and immediately exit    
```
Or use the following alias for this attribute: `bzt config.yml -cloud -detach`

## Configuring Cloud Locations

Cloud locations are specified per execution. Specifying multiple cloud locations for execution means that its `concurrency` and/or `throughput` will be distributed among the locations. Locations are a map of location id's and their relative weights. The relative weight determines how much of the `concurrency` and `throughput` values are put into the corresponding location. 

```yaml
execution:
- locations:
    us-west-1: 1
    us-east-1: 2
```

If no locations are specified for cloud execution, the default value from `modules.cloud.default-location` is taken with a weight of 1. To get the list of all available locations, run `bzt -locations -o modules.cloud.token=<API Key>`. The list of available locations is taken from [User API Call](https://a.blazemeter.com/api/latest/user) and may be specific for particular user. See the `locations` block and the `id` option for each location.

By default, Taurus calculates the machines count for each location based on their limits obtained from the *User API Call*. To switch to a manual machines count, set the option `locations-weighted` to `false`. In this case, the exact provided numbers of machines are used for each location:

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
    test: Taurus Test        # test name
    report-name: full report # name of report
    project: Project Name    # project name or id
```

## Deleting Old Test Files

By default, Taurus deletes all test files from the cloud before uploading any new ones. You can disable
this behaviour by setting the `delete-test-files` module setting to `false`.

Example:
```yaml
modules:
  cloud:
    delete-test-files: false
```

## Specifying Additional Resource Files

If you need some additional files as part of your test, and Taurus fails to detect them automatically, you can attach them to execution using the `files` section:

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

In the shellexec service, the `run-at` parameter lets you specify where commands are executed. Counterintuitively, `local` means the cloud worker executes it, and `cloud` means the controlling CLI executes it.

## Using Separate Pass/Fail Criteria for the Cloud

If you want to use separate pass/fail criteria for cloud execution versus local execution, use the `run-at` parameter to distinguish. For example:

```yaml
reporting:
- module: passfail
  run-at: cloud
  criteria:
  - avg-rt>100ms
  
- module: passfail
  run-at: local
  criteria:
  - avg-rt>5s
```


## Installing Python Package Dependencies

If you need to install additional python modules via `pip`, you can do it by using the `shellexec` service and running the `pip install <package>` command at the `prepare` stage:

```yaml
services:
- module: shellexec
  prepare: 
  - pip install cryptography  # 'cryptography' is the library from PyPi
```

You can even upload your proprietary python eggs into workers by specifying them in the `files` option, and then install them through shellexec:

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

## Changing the Java version

You can choose one of the Java versions from the ones available in the taurus-cloud image.
To specify the Java version, use the version-switch module:

```yaml
modules:
  version-switch:
    switch-java: 21
```

## Enabling the Dedicated IPs Feature

When your account in BlazeMeter allows you to use the "Dedicated IPs" feature, enable it in your config file though the following setting:
```yaml
modules:
  blazemeter:
    dedicated-ips: true
```

## Worker Number Info

You can obtain a worker index which yo ucan used to coordinate distributed test data. For example, you can make sure that different workers use different user logins or CSV file parts. To achieve that, set the following `env` variables for `shellexec` modules and some `properties` for the `jmeter` module:

  * `TAURUS\_INDEX\_ALL` - absolute worker index in test
  * `TAURUS\_INDEX\_EXECUTION` - per-execution worker index
  * `TAURUS\_INDEX\_LOCATION` - per-location worker index

## Cloud Execution Notes

Please note that for `cloud` provisioning, the actual Taurus execution is done on remote machines, so:
  * The test will not run if your account has not enough engines allowed.
  * If you don't specify any duration for a test using the `hold-for` and `ramp-up` options, the default duration limit is used.
  * Do not use the `-report` commmand-line option or the `blazemeter` reporter; all reports are collected automatically by BlazeMeter.
  * Only the following config sections are passed into the cloud: `scenarios`, `execution`, `services`.
  * The `shellexec` module has `artifacts-dir` set as `default-cwd`.
  * Cloud workers execute Taurus in an isolated [virtualenv](https://virtualenv.readthedocs.org/en/latest/).
