# Existing Results Loader

Taurus provides `external-results-loader` executor for cases, where you want to load test
results from existing output files, those generated outside of Taurus (JTL files from JMeter, `simulation.log` files from Gatling, etc).

For example, it can be used to quickly generate online [BlazeMeter report](BlazemeterReporter.md) for test files that you obtained from standalone JMeter run.

Quick example:
```yaml
execution:
- executor: external-results-loader
  data-file: kpi.jtl  # required field
  errors-file: error.jtl  # optional field

reporting:
- module: blazemeter
  test: Taurus Data Loader (JMeter)
```

List of supported formats:
- `kpi.jtl`/`error.jtl` (JMeter CSV & XML files)
- `simulation.log` (Gatling)
- `grinder-bzt-kpi.log` (Grinder)
- TSV files (Apache Benchmark)
- `pbench-kpis.txt` (PBench)
