Extended logging tool allows running custom methods inbetween actions in runtime, and can be used to track progress, gather extra data, or execute some user script.

To enable it, you can use flag `extended-logging: true` in  scenario settings (all actions will be logged on start and finish) or use action `log('some string')` directly
```yaml
execution:
- executor: selenium
  scenario: sample

scenarios:
  sample:
    actions:
    - go(http://blazedemo.com/)
    - log('leaving blazedemo')
    - go(https://gettaurus.org/docs/Index/)
    - log('finished part 1 of my test')
```
or 
```yaml
execution:
- executor: selenium
  scenario: sample

scenarios:
  sample:
    extended-logging: true
    requests:
    - label: Test
      actions:
      - go(http://blazedemo.com/)
      - go(https://gettaurus.org/docs/Index/)
```


Next you need to create a handler. Handler is a method which will be executed by **Apiritif** during runtime when Apiritif tries to log.
For example, this is handler that outputs into file:
```python
def _output_into_file(logString):
    with open('/tmp/taurus/extended.log', 'at') as log:
        log.write(f"{datetime.datetime.now()} {logString} \n")
```

then handler should be appended in user script with `add_logging_handlers`:
```python
from bzt.resources.selenium_extras import add_logging_handlers

class TestSample(unittest.TestCase):
    def setUp(self):
        add_logging_handlers(_output_into_file)
```
if you set it up correctly, `_output_into_file` method will be executed each time there is a log.