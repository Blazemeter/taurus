# Nose Executor
Allows to run functional Python tests based on nose library.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Usage:
```yaml
execution:
- executor: nose  
  scenario:
    script: tests/
```

## Supported file types:

It is valid to specify both single Python module (single .py file) and a Python package (folder with Python modules
and packages).

Configuration options:

```yaml
modules:
  nose:
    working-dir: classes  # set name of runner working directory within artifacts dir
    interpreter: /home/user/interpreter/python  # path to custom interpreter.
```

## Request Scenario

Nose executor supports building test script from the `requests` option of `scenario`. In that case Taurus will
generate a Python script that will be launched with `nose`.

Supported features:
  - select browser
  - set timeout/think-time on both scenario and request levels
  - assertions (only requested page source inspected)
  - request method GET (only)
  - send keys, wait for items, click on items selected by ID/Name/CSS/XPath
   
Action names are built as `<action>By<selector type>(<selector>)`. Sometimes actions can have value. Options are:
  - `waitByID`, `waitByName`, `waitByLinkText`, `waitByCSS` and `waitByXPath` - to wait until desired option becomes present on page.
  Timeout is taken from scenario-level `timeout` option. Optionally, you can specify parameter `visible` to wait
  for visibility rather than presence (like `waitByName(elmName): visible`)
  - `clickByID`, `clickByName`, `clickByLinkText`, `clickByCSS` and `clickByXPath` - no parameter, just click on object
  - `keysByID`, `keysByName`, `keysByLinkText`, `keysByCSS` and `keysByXPath` - to enter keys into input items, requires parameter.
  Like this: `keysByName(MyFormInputName): Value To Enter`

There is special action `pauseFor(<time>)` which makes script to sleep for specified amount of time. Also, calling action `clearCookies()` will force `delete_all_cookies` method to be called on WebDriver object.

Sample request scenario:
```yaml
scenarios:
  request_example:
    browser: Firefox  # available browsers are: ["Firefox", "Chrome", "Ie", "Opera"]
    headless: true  # available only for Chrome/Firefox and only on Selenium 3.8.0+, disabled by default
    timeout: 10  #  global scenario timeout for connecting, receiving results, 30 seconds by default
    think-time: 1s500ms  # global scenario delay between each request
    default-address: http://demo.blazemeter.com  # specify a base address, so you can use short urls in requests
    requests:
    - url: /  # url to open, only get method is supported
      actions:  # holds list of actions to perform
      - waitByCSS(body)
      - clickByID(mySubmitButton)
      - pauseFor(5s)
      - clearCookies()
      - keysByName(myInputName): keys_to_type
      - waitByID(myObjectToAppear): visible
      assert: # assert executed after actions
      - contains:
        - blazemeter  # list of search patterns
        - Trusted
        subject: body # only body subject supported
        regexp: false  # treat string as regular expression
        not: false  # inverse assertion condition
```
