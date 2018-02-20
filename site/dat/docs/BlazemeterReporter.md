# BlazeMeter Reporting Service

Like it always happens with tools that focused on _executing_ tests, they are unable to provide
sufficient reporting functionality. As professional user, you need some centralized storage to
be able to access test results in convenient and interactive way, compare different executions,
see trends over time and collaborate with your colleagues. [BlazeMeter.com](http://blazemeter.com)
offers such service, it has both commercial and free of charge versions.

![BlazeMeter Report](blazemeter-rsz.png)

## Anonymous Usage

The simplest way to get a taste of BlazeMeter reporting is to use `-report` command-line switch.
This will enable result feeding to service without any other settings required. You will receive
the link for your report in the console text, and the link will be automatically opened in your
system browser, see `browser-open` option for more tuning.

The official policy for BlazeMeter reports uploaded from Taurus, is that anonymous reports are
kept for 7 days and if you're using your own account, then reports are kept according to the
retention policy of your account. For details see BlazeMeter service [website](https://blazemeter.com/). 

## Personalized Usage

If you want the results to be stored in your existing BlazeMeter account, you'll need to specify
the reporting settings in your configuration file. Get the API token from BlazeMeter.com (find it
under your [Settings => API Keys](https://a.blazemeter.com/app/#settings/api-keys)) and put it
into `token` option. Join key ID and secret with single colon:

```yaml
modules:
  blazemeter:
    token: TDknBxu0hmVnJ7NqtG2F:DFadfgdsljasdfkKSKSDDFKSDFJKSDJFKSDJFsdjfksjfjDSF
```

<div class="alert alert-danger">
Never put API key into your main config files! 

Never post it to support forums!

It is recommended to place the token setting in your personal
[per-user config](CommandLine.md#configuration-files-processing) `~/.bzt-rc` to prevent it from
being logged and collected in artifacts.
</div>

Now you can use `-report` command-line switch, or you can set BlazeMeter reporting as part of
your config, the `test` option specifies test name to use, `project` names group of tests:

```yaml
reporting:
- module: blazemeter
  report-name: Jenkins Build 1
  test: Taurus Demo
  project: Taurus Tests Group
```

Advanced settings:

```yaml
modules:
  blazemeter:
    address: https://a.blazemeter.com  # reporting service address
    data-address: https://data.blazemeter.com  # data service address
    browser-open: start  # auto-open the report in browser, 
                         # can be "start", "end", "both", "none"
    send-interval: 30s   # send data each n-th second
    report-times-multiplier: 1000  # multiplying factor for response times, advanced option
    timeout: 5s  # connect and request timeout for BlazeMeter API
    artifact-upload-size-limit: 5  # limit max size of file (in megabytes)
                                   # that goes into zip for artifact upload, 10 by default
    public-report: false  # set to true to create a public link to the report

    # following instructions will have effect when no per-reporter settings
    report-name: My Next Test  # if you will use value 'ask', it will ask it from command line
    test: Taurus Test
    project: My Local Tests
```

Note how easy is to set report settings from command line, i.e. from inside Jenkins build step:
```bash
bzt mytest.yml -o modules.blazemeter.report-name="Jenkins Build ${BUILD_NUMBER}"
```

Also, there is CLI alias `-public` to automatically set `public-report=true`.