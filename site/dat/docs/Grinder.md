# Grinder Executor

Configuration options:

 - `path`: "/somepath/folder/"
    Path to Grinder.
    If no grinder.jar found in folder/lib/, Grinder tool will be automatically downloaded and installed in "path".
    By default "~/.bzt/grinder-taurus/".
 - `download-link`:"http://somehost/grinder-{version}-binary.zip"
    Link to download Grinder.
    By default "http://switch.dl.sourceforge.net/project/grinder/The%20Grinder%203/{version}/grinder-{version}-binary.zip"
 -  `version`: "3.11"
    Grinder version, by default "3.11"

## Run Grinder Tool

```yaml
---
execution:
- executor: grinder
  concurrency: 3
  ramp-up: 10
  iterations: 20
  scenario:
    script: tests/grinder/helloworld.py
    properties_file: tests/grinder/grinder.properties
    properties:
      grinder.useConsole: false

- executor: grinder
  concurrency: 2
  ramp-up: 5
  iterations: 10
  scenario:
    requests:
    - http://demo.blazemeter.com/
    - http://demo.blazemeter.com/api
```