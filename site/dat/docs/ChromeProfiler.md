# Chrome Profiler Service

This service can use Chrome performance logs to calculate frontend performance metrics.

Note that it only makes sense to use `chrome-profiler` service in parallel with Chrome-based
Selenium tests. Your test script also has to write Chrome performance logs to the specific files.
Profiler service will then use these files to extract and calculate performance metrics from them.

Calculated metrics will be automatically attached to BlazeMeter reports and plotted in a timeline
report. There also exists a reporter (named `chrome-metric-reporter`) that will print all
calculated metrics to the terminal at the end of the test.

Here's an example of Taurus configuration to use this service:
```yaml
---
execution:
- executor: selenium
  scenario: simple

scenarios:
  simple:
    script: test_frontend.py  # test script has to satisfy specific criteria, read more below

services:
- module: chrome-profiler
  trace-file: trace.json  # trace file to use, more on that below
  cpuprofile: js.cpuprofile  # cpu profile file to use, more on that below

reporting:
- module: chrome-metric-reporter
```

Note that having multiple `iterations` for your tests is fine, Taurus will pick up updated performance
log files after each iteration and will recalculate metrics on the go.


## Test Script Requirements

Your Selenium test script (be it Java or Python) has to satisfy the following criteria:
- It has to use Chrome (obviously)
- It should set up Chrome to log tracing data
- It should write Chrome trace data to file in artifacts dir (filename is specified with `trace-file` service option)
- It should also write Chrome CPU Profile data to file in artifacts dir (filename is specified with `cpuprofile` service option)

Here's an example of Python-based Selenium test script that does all listed things:

```python
import json
import time
from selenium import webdriver
import unittest


# Chrome tracing categories that are required to calculate performance stats
trace_cats = [
    "blink.net",
    "blink.user_timing",
    "v8",
    "devtools.timeline",
    "disabled-by-default-memory-infra",
    "disabled-by-default-devtools.timeline",
    "disabled-by-default-devtools.timeline.frame",
]

# Chrome arguments to enable logging performance data
args = [
    "--no-sandbox",
    "--enable-tracing",
    "--native",
]


class ChromeTest(unittest.TestCase):
    def setUp(self):
        # configure chromedriver to redirect Chrome tracing data to performance logs
        self.driver = webdriver.Chrome(
            desired_capabilities = {
                'loggingPrefs': { 'performance': 'ALL' },
                'chromeOptions': {
                   "args" : args,
                   "perfLoggingPrefs" : {
                       "traceCategories": ",".join(trace_cats),
                   },
                }
            },
        )
        self.start_time = time.time()
        # start recording Chrome CPU Profile
        self.driver.execute_script(":startProfile")

    def test_local_fractals(self):
        self.driver.get('https://github.com/')

    def tearDown(self):
        # save chrome tracing data (viewable by chrome://tracing) to 'trace.json' file
        with open(r"trace.json", 'w') as f:
            start_event = {"name": "start_time", "args": {"timestamp": self.start_time}, "ts": 0, "cat": "__metadata", "ph": "M"}
            trace = [start_event]
            for log_entry in self.driver.get_log('performance'):
                event = json.loads(log_entry['message'])
                if event['message']['method'] == 'Tracing.dataCollected':
                    trace.append(event['message']['params'])
            f.write(json.dumps(trace))
        # save Chrome CPU profile (viewable by Chrome devtools)
        profile = self.driver.execute_script(":endProfile")
        if profile:
            with open("js.cpuprofile", 'w') as f:
                f.write(json.dumps(profile["profile"]))
        # close browser
        self.driver.quit()


if __name__ == '__main__':
    unittest.main()
```
