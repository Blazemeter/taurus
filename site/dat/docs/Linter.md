# Taurus Configuration Linter

Due to the non-strict nature of YAML, syntactic and semantic errors in the configuration files are not a rare thing.
Linting subsystem is an attempt to automatically warn user about errors, misprints and straight-out errors in the
configuration files.

Linting is done right after all configuration files are loaded and just before any modules are prepared.

## Linting output

Here's the example of linter output:
```
WARNING: at path 'execution.0.ramp': unfamiliar name 'ramp'. Did you mean 'ramp-up'?
WARNING: at path 'execution.0.throuhhput': unfamiliar name 'throuhhput'. Did you mean 'throughput'?
WARNING: at path 'execution.0.scenario': scenario 'foobar' is used but isn't defined
```

For each issue found, linter prints one warning. Each warning contains a path to the problematic code
(e.g. `execution.0.scenario`) and a warning message. The paths are the same as used in CLI overrides and can be read
pretty easily. For example, path `execution.0.ramp` means the key `ramp` that is defined inside the first element of
the `execution` list.

## Running the linter

Linter is always enabled by default and is invoked right after configuration is loaded. By default, linter prints
a warning for each issue found, but it doesn't prevent test execution.

If you need to lint the config without actually running the test, you can use the `-lint` command-lint flag.

Example:
```bash
$ bzt -lint my-config.yml
```

Using `-lint` results in the following exit codes. The exit code is equal 0 if there were no issues found in the
configuration and 1 if issues were found.

If for some reason you want to disable the linting, you can pass a `-nolint` flag, which will disable linting
completely.

## Configuring Linter

Just like everything in Taurus, linting can be configured within a configuration file.

```yaml
cli:
  linter:
    disable: false  # set to true to disable linting
    lint-and-exit: true  # stop Taurus after linting, thus preventing test execution
    ignored-warnings:  # list of warning ids to ignore. empty by default
    - single-execution
    - possible-typo
```

## Warnings Produced by Linter

Taurus implements the following checks:

- `possible-typo`: when Taurus detects a possible typo in the name of some field.
- `single-execution`: when `execution` is a dict instead of the list. This is a supported case by Taurus, but it's use is discouraged.
- `execution-non-list`: when `execution` is not a list, which is an error.
- `no-scenario`: when `execution` item doesn't define the scenario to use for the test.
- `scenario-non-dict`: when scenario (defined either inside execution, or in `scenarios`) is not a dict.
- `undefined-scenario`: when execution item specifies scenario that isn't defined by the configuration.
- `no-script-or-requests`: when scenario doesn't define neither `script` nor `requests`.
- `script-and-requests`: when scenario defines both `script` and `requests`, making `requests` useless.


