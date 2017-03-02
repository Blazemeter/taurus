# SoapUI Support

## SoapUI support

You can specify existing SoapUI scripts for JMeter executor. The executor will extract specified test case from SoapUI project
and convert it to Taurus scenario.

Example:
```yaml
execution:
- executor: jmeter
  concurrency: 20
  hold-for: 1m
  scenario:
    script: project.xml  # SoapUI project
    test-case: index  # name of SoapUI test case to use
```

If your SoapUI project contains only one test case — you can omit the `test-case` option.

##  soapui2yaml — Converting Existing SoapUI Project into Taurus YAML

Taurus includes a command-line utility named `soapui2yaml` that can be used to convert existing SoapUI scripts
into Taurus YAML-based configs.

Supported command-line options:
  - `-h, --help` - show help message and exit
  - `-q, --quiet` - only errors printed to console
  - `-v, --verbose` - prints all logging messages to console
  - `-j, --json` - use json format instead of yaml
  - `-o FILE\_NAME, --out=FILE\_NAME` - change output file name, by default is input file name + `.yml` in current directory
  - `-t TEST\_CASE, --test-case TEST\_CASE` - extract single test case by name

Usage:
  - `soapui2yaml project.xml` - convert whole SoapUI project
  - `soapui2yaml project.xml --test-case SmokeTest -o soapui-smoke-test.yml`  - extract only one test case

Note that when converting SoapUI project, Taurus uses the following rules:
1. Each test case is translated to a separate Taurus `scenario`.
2. If test case has a load test - it's converted to an `execution` element.
3. Only REST and HTTP requests are supported.
4. The following HTTP request features are supported: HTTP headers, request body, parameters, assertions.
5. Properties are converted to JMeter variables.
6. Property transfer is translated to JMeter extractors (XPath and JSONPath).

