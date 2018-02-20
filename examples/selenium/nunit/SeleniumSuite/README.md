# Selenium Test Suite with NUnit and Taurus

This is an example test suite that uses Taurus to run Selenium/NUnit-based functional tests.

It uses Chrome for running tests, so be sure to have it installed.

## Building Test Suite

### Building with Mono

There are two ways to build this project: using MonoDevelop or with command line tools.

#### Building with MonoDevelop

Just open the solution file with (fairly recent) MonoDevelop, let it resolve referenced dependency packages and build a 'Release' target.

#### Building with Command Line Tools

This requires Mono 4 and Nuget to be installed.

1. Resolve dependencies.

```bash
nuget restore SeleniumSuite/packages.config -SolutionDirectory .
```

This requires fairly recent Nuget (4.x will work).

2. Build the project

```bash
xbuild /p:Configuration=Release SeleniumSuite.sln
```

### Building with Windows Build Tools

Just fire up the Visual Studio and build the solution.

## Running the tests

Use the following command:

```bash
bzt test.yml
```
