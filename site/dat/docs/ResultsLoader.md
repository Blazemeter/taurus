# Existing Results Loader

Taurus provides `external-results-loader` executor for cases, where you want to load test
results from existing output files, those generated outside of Taurus (JTL files from JMeter, `simulation.log` files from Gatling, etc).

For example, it can be used to quickly generate online [BlazeMeter report](BlazemeterReporter.md) for test files that you obtained from standalone JMeter run.

There is shorthand for running this directly from command-line: `bzt path/to/file.jtl -report`

If you need to use some more options to loader, here's YAML config example:
```yaml
execution:
- executor: external-results-loader
  data-file: kpi.jtl  # required field
  errors-file: error.jtl  # optional field

reporting:
- module: blazemeter
  test: Taurus Data Loader (JMeter)
```

List of supported file formats:
- `kpi.jtl`/`error.jtl` (JMeter CSV & XML files)
- `simulation.log` (Gatling)
- `grinder-bzt-kpi.log` (Grinder)
- TSV files (Apache Benchmark)
- `pbench-kpis.txt` (PBench)

## Dynamic File Detection

It is possible to tell results loader that file might appear on disk later, and also have dynamic name. For that, two additional options can be specified (either on executor level, or in module settings). Consider following config:

```yaml
execution:
- executor: external-results-loader 
  data-file-pattern: external-*.jtl  # (1)
  wait-for-file: 1m                  # (2)
  results-timeout: 10s               # (3)
```

It configures loader to expect file that matches pattern "external-*.jtl" in the current directory (1). Taurus will wait for this file to appear, polling it for 1 minute, as configured by option `wait-for-file` (2). If multiple files match, last in alphabetical order will be taken.

Additionally, Taurus will look at results it reads from file, and if there are no new data appearing, and timestamp of last result is more than 10 seconds into past, it will assume there will be no more data written into file (3). This is configured by `results-timeout` option.

Abovementioned three options enable you to have custom command launching testing tool and making Taurus to read these results. For that, you can additionally utilize [ShellExec](ShellExec.md) module and configure background process. Full example of this approach can be found as [external.yml](https://github.com/Blazemeter/taurus/blob/master/examples/jmeter/external.yml) and [gradle-gatling.yml](https://github.com/Blazemeter/taurus/blob/master/examples/gatling/gradle/gradle-gatling.yml) examples.
