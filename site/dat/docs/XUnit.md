# xUnit Executor

This executor allows running tests written with xUnit framework. It uses .Net Core 8.0 with xUnit 2.7.0.

Follow execution params are possible for NUnit Executor: `concurrency`, `iterations`, `hold-for` and `ramp-up`.

Usage:
```yaml
execution:
- executor: xunit
  concurrency: 2
  iterations: 5
  scenario:
    script: bin/Release/TestAssembly.dll  # assembly with tests
```

Taurus will run xUnit through a custom runner that will extract all tests from DLL assembly
and pass them to xUnit to run them.

Note that Taurus won't try to build your test suite solution. You should build it yourself,
either with VisualStudio/Rider or with command line tools.

Note: when running tests, you should have dotnet installed on your machine.