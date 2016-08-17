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

    def test_chrome_trace(self):
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
