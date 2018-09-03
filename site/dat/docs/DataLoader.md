# Data Loader Executor

Taurus provides `external-data-loader` executor for cases, where you want to load test
results from existing report files generated outside of Taurus control (JTL files from JMeter, `simulation.log` files
from Gatling, etc).

For example, it can be used to quickly create BlazeMeter report for test data after the test.

Quick example:
```yaml
execution:
- executor: external-data-loader
  data-file: kpi.jtl  # required field
  errors-file: error.jtl  # optional field

reporting:
- module: blazemeter
  test: Taurus Data Loader (JMeter)
```

List of supported formats:
- `kpi.jtl`/`error.jtl` (JMeter)
- `pbench-kpis.txt` (PBench)
- `simulation.log` (Gatling)
- `grinder-bzt-kpi.log` (Grinder)
- TSV files (Apache Benchmark)
