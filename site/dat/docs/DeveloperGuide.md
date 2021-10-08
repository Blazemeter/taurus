# Rules for Contributing
 1. All contributions go via pull requests and code review
 1. Most of pylint warnings should be fixed ![](https://api.codacy.com/project/badge/Grade/9ed495a3e5df4ba2ad05e19a690121d2?ext=.svg)
 1. All new files must contain Apache License header (unit test files may not have it)
 1. All changes must be reflected in [Changelog](Changelog)
 1. All changes must be properly documented 
 1. All changes must be covered with unit tests, no broken tests in master ![](https://api.travis-ci.org/Blazemeter/taurus.svg?branch=master&ext=.svg)  ![](https://ci.appveyor.com/api/projects/status/github/Blazemeter/taurus?svg=true&ext=.svg) 
 
# Release Process
 - Make sure you're on up-to-date master
 - Compile [Changelog.md](Changelog) record
   - Use `.change` files, remove them later
   - Set version and date
 - Make sure DockerHub image builds for master
 - Send announce if needed
 - ... code freeze ...
 - Set `Release <version>` as commit name
 - Create git tag with version, make `git push`, including tag
 - Deployment of wheel, site and docker image will be made by Jenkins after testing
 - Notify all interested parties (Twitter, mailing lists)
 
# Developing Taurus Extensions

Taurus is an extensible project. You can develop additional modules for it (executors, services, reporters)
in a completely separate codebase. See `bzt/resources/base-config.yml` to get a feeling of how Taurus
loads all its components.

Additionally, Taurus has a mechanism for automatically detecting configuration files for Taurus plugins.
Here's the conditions:
1. Your plugin has to be a Python package installed with the same Python that you're using to run Taurus.
1. Your plugin has to have a `bzt-configs.json` file in the project dir (right next to `_\_init\_\_.py`).
1. In this file should be a JSON list of configuration file names that your project uses to define Taurus modules.

For example, you are developing a Taurus extension with the following structure:

```
bzt_plugin_hello
├── __init__.py  # package init file, standard for Python
├── hello.py  # your plugin module, contains class HelloService
├── 10-hello.yml  # configuration file, analogous to base-config.yml
├── bzt-configs.json  # configuration index
```

Contents of `bzt-configs.json` should be `["10-hello.yml"]`.

Contents of `10-hello.yml`:
```yaml
modules:
  hello:
    class: bzt_plugin_hello.hello.HelloService
```

# Adding new Executor

Here is our [checklist](NewExecutorChecklist.md) for a new executor. Also, we have an [article](../kb/AddingExecutor.md) 
about that.