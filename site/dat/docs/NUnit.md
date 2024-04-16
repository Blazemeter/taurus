# NUnit Executor

This executor allows running tests written with NUnit framework. It uses .Net Core 8.0 with NUnit 4.1.0.

Follow execution params are possible for NUnit Executor: `concurrency`, `iterations`, `hold-for` and `ramp-up`.

Usage:
```yaml
execution:
- executor: nunit
  concurrency: 2
  iterations: 5
  scenario:
    script: bin/Release/TestAssembly.dll  # assembly with tests
```

Taurus will run NUnit through a custom runner that will extract all tests from DLL assembly
and pass them to NUnit to run them.

Note that Taurus won't try to build your test suite solution. You should build it yourself,
either with VisualStudio/Rider or with command line tools. You can find the example of
test suite project with building instructions in Taurus's repo at
[Github](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/nunit/).

Note: when running tests, you should have dotnet installed on your machine.