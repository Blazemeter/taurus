# NUnit Executor

This executor allows to run tests written with NUnit framework. It uses Mono under Linux and Mac OS
and native Windows functions under Windows. There is a full support for NUnit 3.

Taurus can loop test suite execution until desired number of `iterations` will complete or
`hold-for` time will be exceeded.

Usage:
```yaml
execution:
- executor: nunit
  iterations: 5
  scenario:
    script: bin/Release/TestAssembly.dll  # assembly with tests
```

Taurus will run NUnit through a custom runner that will extract all tests from DLL assembly
and pass them to NUnit to run them.

Note that Taurus won't try to build your test suite solution. You should build it yourself,
either with VisualStudio/MonoDevelop or with command line tools. You can find the example of
test suite project with building instructions in Taurus's repo at
[Github](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/nunit/).

Under non-Windows operating systems, Taurus requires Mono to be installed. Mono 4 is the preferred
version.

Note: when running tests, you should have NUnit's DLL (nunit.framework.dll) lying in the same
directory as DLL with your tests.