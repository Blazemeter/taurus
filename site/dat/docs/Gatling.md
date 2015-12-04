# Gatling Executor

Configuration options:

 - `path`: "/somepath/folder/bin/gatling_executable"
    Path to Gatling executable.
    If no Gatling executable found, it will be automatically downloaded and installed in "path".
    By default "~/.bzt/gatling-taurus/bin/gatling.sh".
    
 - `download-link`:"http://somehost/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    Link to download Gatling.
    By default: "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    
 -  `version`: "2.1.4"
    Gatling version, by default "2.1.4"

## Run Gatling Tool

```yaml
execution:
  - executor: gatling
    scenario:
      script: tests/gatling/BasicSimulation.scala
      simulation: tests.gatling.BasicSimulation
```

The `simulation` option is canonical class name for main simulation class. It will be passed as-is to gatling with `-s` option.
