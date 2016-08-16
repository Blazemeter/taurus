import json
import time
from selenium import webdriver
import unittest


trace_cats = [
    "blink.net",
    "blink.user_timing",
    "v8",
    "devtools.timeline",
    "disabled-by-default-memory-infra",
    "disabled-by-default-devtools.timeline",
    "disabled-by-default-devtools.timeline.frame",
]

args = [
    "--no-sandbox",
    "--enable-tracing",
    "--native",
]


class ChromeTest(unittest.TestCase):
    def setUp(self):
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
        self.driver.execute_script(":startProfile")

    def test_local_fractals(self):
        self.driver.get('http://blazedemo.com/')

    def tearDown(self):
        # save chrome trace (viewable by chrome://tracing)
        with open(r"trace.json", 'w') as f:
            f.write("[\n")
            lines = [json.dumps({"name": "start_time", "args": {"timestamp": self.start_time}, "ts": 0, "cat": "__metadata", "ph": "M"})]
            for d in self.driver.get_log('performance'):
                event = json.loads(d['message'])
                if event['message']['method'] == 'Tracing.dataCollected':
                    lines.append(json.dumps(event['message']['params']))
            f.write(",\n".join(lines))
            f.write("\n]\n")
        # save Chrome CPU profile (viewable by Chrome devtools)
        profile = self.driver.execute_script(":endProfile")
        if profile:
            with open("js.cpuprofile", 'w') as f:
                f.write(json.dumps(profile["profile"]))
        # close browser
        self.driver.quit()


if __name__ == '__main__':
    unittest.main()
