# New executor checklist

This is a checklist for adding new test executor to Taurus.

There's [another guide](AddingExecutor.md) that focuses on the implementation of test executor.

## Code

* [ ] Executor class
* [ ] (for load executors) `ResultsReader` class
* [ ] (for functional) — test engine plugin to write test results in Taurus-compatible LDJSON
* [ ] Update `bzt/resources/base-config.yml`
* [ ] Sidebar widget for console dashboard


## Tests

* [ ] Unit tests
* [ ] Manual testing


## Docs

* [ ] Add new executor doc page in `site/dat/docs/`
    * [ ] Short description
    * [ ] List of features
    * [ ] Configuration examples
    * [ ] Notes and specifics
* [ ] Mention new executor to [Index.md](Index.md) and [ExecutionSettings.md](ExecutionSettings.md) 
* [ ] Add `.change` file to `site/dat/docs/changes/`


## Miscellaneous

* [ ] Add to `examples/all-executors.yml`
* [ ] Update `Dockerfile` and verify that Docker build works
* [ ] Add usage example to `examples/`
* [ ] Update MANIFEST.in and ensure that `python setup.py sdist` includes all necessary files
