# Supporting New Load Testing Tool with Taurus

Note: this article is a work-in-progress. It will be completed later.
Feel free to ask your questions at Taurus [support channel](https://groups.google.com/forum/#!forum/codename-taurus)
or to open a pull requst at Github.

There is good example of minimal custom executor code here: [examples/custom][https://github.com/Blazemeter/taurus/tree/master/examples/custom]

So you want to support a new testing tool with Taurus. For that you'll have to write a new test executor.

Test executor has two main tasks:

* Run a testing tool in a subprocess, configured according to Taurus config file
* Extract test results from load testing tool and pass them to Taurus's aggregator

Overview of steps:
1. Create a new Python module inside `bzt/modules/`
2. Declare a class inherited from `bzt.engine.ScenarioExecutor`
3. Implement `prepare()`, `startup()`, `check()`, `shutdown()` and `post\_process()` phase methods
4. Add a class mapping for the new executor into `modules` section of `bzt/resources/base-config.yml`
5. Write unittests, put your unittest file in in `tests/modules/` dir


## Step 1 - Creating a Runner

Each executor has 5 phase methods:
- `prepare()` - load configuration, prepare test executor to be launched
- `startup()` - start test executor process
- `check()` - check if test executor process finished, returns True if finished
- `shutdown()` - shut down executor process
- `post\_process()` - post-process executor, close all opened resources

Note that `prepare()`-`post\_process()` and `startup()`-`post\_process()` are mirrored phases.
It means that if executor's `prepare()` was called - the engine will always call executor's `post\_process()`.

This makes `prepare()` phase a right place to open any resources and `post\_process()` — a right place to close them.
Just like that, `startup()` is a good place to launch subprocess, while taking it down in `shutdown()`.
 

## Step 2 - Reading Test Results

If executor wants Taurus to pick up test results from load testing tool — it should declare a results reader class.
This class should inherit from `bzt.modules.aggregator.ResultsReader`, implementing `\_read()` generator method.

Reader is expected to be able to extract the following nonaggregated data about
each HTTP request from the load testing tool:

* URL of the request
* request timestamp
* response (success or failure)
* response code
* response time
* error message

Then, at the `prepare` stage, executor should instantiate reader class and add it as an underling to Taurus's aggregator.

For a working example you can take a look at `bzt/modules/ab.py`, which implements Taurus executor for `ab` tool
from Apache's HTTP tool set.


Additionally, there's a [checklist](NewExecutorChecklist.md) we use when adding new
executors to ensure that everything is considered.