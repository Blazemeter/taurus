# Chrome Profiler Service

This service can use Chrome performance logs to calculate frontend performance metrics.

Note that it only makes sense to use `chrome-profiler` service in parallel with Chrome-based
Selenium tests. Your test script also has to write Chrome performance logs to the specific files.
Profiler service will then use these files to extract and calculate performance metrics from them.

Calculated metrics will be automatically attached to BlazeMeter reports and plotted in a timeline
report. There also exists a reporter (named `chrome-metric-reporter`) that will print all
calculated metrics to the terminal at the end of the test.

Here's an example of Taurus configuration that uses profiler service:
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
  procesors:
  - class: bzt.modules.chrome.TraceProcessor
    file: trace.json  # trace file to extract metrics from, more on that below

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
        # close browser
        self.driver.quit()


if __name__ == '__main__':
    unittest.main()
```

## Example of performance stats output

Here's the sample of output provided by Chrome profiler service:

```
14:05:10 INFO: Chrome metrics for tab 'How people build software · GitHub':
14:05:10 INFO: 
14:05:10 INFO: DOM metrics:
14:05:10 INFO: Label | Value
14:05:10 INFO: Number of DOM documents at the end | 2
14:05:10 INFO: Number of DOM nodes at the end | 722
14:05:10 INFO: Number of event listeners at the end | 38
14:05:10 INFO: 
14:05:10 INFO: Network metrics:
14:05:10 INFO: Label | Value
14:05:10 INFO: Number of HTTP requests | 21
14:05:10 INFO: Network footprint | 0.81
14:05:10 INFO: Time to first byte | 0.60
14:05:10 INFO: Number of AJAX requests | 1
14:05:10 INFO: Time for full page load | 1.83
14:05:10 INFO: 
14:05:10 INFO: HTTP requests:
14:05:10 INFO: Start time | End time | Method | URL | HTTP Status | MIME | Size
14:05:10 INFO: 14:05:07.094 | 14:05:07.586 | GET | https://github.com/ | 200 | text/html | 9974
14:05:10 INFO: 14:05:07.591 | 14:05:07.855 | GET | https://assets-cdn.github.com/assets/frameworks-be4c6e0e4... | 200 | text/css | 46393
14:05:10 INFO: 14:05:07.592 | 14:05:07.844 | GET | https://assets-cdn.github.com/assets/github-2fc372026e1af... | 200 | text/css | 69064
14:05:10 INFO: 14:05:07.592 | 14:05:07.765 | GET | https://assets-cdn.github.com/assets/site-c31d97cdffdd1a1... | 200 | text/css | 9020
14:05:10 INFO: 14:05:07.592 | 14:05:07.998 | GET | https://assets-cdn.github.com/assets/frameworks-ff9877ee4... | 200 | application/javascript | 106258
14:05:10 INFO: 14:05:07.592 | 14:05:07.999 | GET | https://assets-cdn.github.com/assets/github-3aee017820ac6... | 200 | application/javascript | 123282
14:05:10 INFO: 14:05:07.592 | 14:05:07.997 | GET | https://assets-cdn.github.com/images/modules/site/univers... | 200 | image/png | 69211
14:05:10 INFO: 14:05:07.592 | 14:05:08.001 | GET | https://assets-cdn.github.com/images/modules/site/home-il... | 200 | image/png | 6747
14:05:10 INFO: 14:05:07.592 | 14:05:08.004 | GET | https://assets-cdn.github.com/images/modules/site/home-il... | 200 | image/png | 14277
14:05:10 INFO: 14:05:07.592 | 14:05:08.004 | GET | https://assets-cdn.github.com/images/modules/site/home-il... | 200 | image/png | 10936
14:05:10 INFO: 14:05:07.592 | 14:05:08.005 | GET | https://assets-cdn.github.com/images/modules/site/home-il... | 200 | image/png | 19421
14:05:10 INFO: 14:05:07.592 | 14:05:08.101 | GET | https://assets-cdn.github.com/images/modules/site/org_exa... | 200 | image/png | 97808
14:05:10 INFO: 14:05:07.592 | 14:05:08.000 | GET | https://assets-cdn.github.com/assets/frameworks-ff9877ee4... | 200 | application/javascript | 106258
14:05:10 INFO: 14:05:07.592 | 14:05:08.000 | GET | https://assets-cdn.github.com/assets/github-3aee017820ac6... | 200 | application/javascript | 0
14:05:10 INFO: 14:05:07.874 | 14:05:08.006 | GET | https://assets-cdn.github.com/images/modules/site/home-he... | 200 | image/jpeg | 116611
14:05:10 INFO: 14:05:07.877 | 14:05:08.002 | GET | https://assets-cdn.github.com/static/fonts/roboto/roboto-... | 200 | application/x-font-woff | 14002
14:05:10 INFO: 14:05:07.879 | 14:05:08.003 | GET | https://assets-cdn.github.com/static/fonts/roboto/roboto-... | 200 | application/x-font-woff | 13951
14:05:10 INFO: 14:05:07.882 | 14:05:08.003 | GET | https://assets-cdn.github.com/static/fonts/roboto/roboto-... | 200 | application/x-font-woff | 13892
14:05:10 INFO: 14:05:08.232 | 14:05:08.435 | POST | https://www.google-analytics.com/r/collect | 200 | image/gif | 324
14:05:10 INFO: 14:05:08.233 | 14:05:08.809 | GET | https://collector.githubapp.com/github/page_view?dimensio... | 200 | image/gif | 545
14:05:10 INFO: 
14:05:10 INFO: AJAX requests:
14:05:10 INFO: Start time | URL
14:05:10 INFO: 14:05:08.435 | https://www.google-analytics.com/r/collect
14:05:10 INFO: 
14:05:10 INFO: JavaScript metrics:
14:05:10 INFO: Label | Value
14:05:10 INFO: Time spent doing GC in JS engine | 0.02
14:05:10 INFO: Average JS heap size | 5.54
14:05:10 INFO: 
14:05:10 INFO: Page load times:
14:05:10 INFO: Label | Value
14:05:10 INFO: Time to page load | 1.83
14:05:10 INFO: Time to DOMContentLoad event | 1.10
14:05:10 INFO: Time to first paint | 1.03
```
