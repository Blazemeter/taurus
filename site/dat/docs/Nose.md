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
  - select browser Chrome, Firefox, Android-Chome (Appium), iOS-Safari (Appium) 
  - local webdriver, local remote webdriver or remote webdriver (selenium grid or others like browserstack, saucelabs and similars)
  - capabilities for remote webdriver - browser, version, javascript, platform, os_version, selenium, device, app
  - set timeout/think-time on both scenario and request levels
  - assertions (requested page source inspected use the new assertTitle or assertTextBy* for item level)
  - pauseFor (pause for n seconds) 
  - request method GET (only)
  - selenium commands:
    - keysBy*   
    - waitBy* 
    - clickBy* 
    - doubleClickBy* 
    - mouseDownBy* 
    - mouseUpBy* 
    - assertTextBy* Assert text on element
    - selectBy* Select value in drop down list
    - assertTitle

Note: * selected by ID/Name/CSS/XPath 
   
Action names are built as `<action>By<selector type>(<selector>)`. Sometimes actions can have value. Options are:
  - `waitByID`, `waitByName`, `waitByLinkText`, `waitByCSS` and `waitByXPath` - to wait until desired option becomes present on page.
  Timeout is taken from scenario-level `timeout` option. Optionally, you can specify parameter `visible` to wait
  for visibility rather than presence (like `waitByName(elmName): visible`)
  - `clickByID`, `clickByName`, `clickByLinkText`, `clickByCSS` and `clickByXPath` - no parameter, just click on object
  - `keysByID`, `keysByName`, `keysByLinkText`, `keysByCSS` and `keysByXPath` - to enter keys into input items, requires parameter.
  Like this: `keysByName(MyFormInputName): Value To Enter`. Special keys are allowed using the prefix `KEY_` . Like this: `keysByName(MyFormInputName): KEY_ENTER`.  See some of the possible values supported [here](http://selenium-python.readthedocs.io/api.html#module-selenium.webdriver.common.keys)

There is special action `pauseFor(<time>)` which makes script to sleep for specified amount of time. Also, calling action `clearCookies()` will force `delete_all_cookies` method to be called on WebDriver object.

Sample request scenario:
```yaml
scenarios:
  request_example:
    browser: Firefox  # available browsers are: ["Firefox", "Chrome", "Android-Chrome", "iOS-Safari"]
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

Sample Remote Webdriver scenario:
```yaml
scenarios:
  request_example:
    remote: http://user:key@remote_web_driver_host:port/wd/hub
    capabilities:
      - browser: firefox  # Depends on the capabilities of the remote selenium server
      - version: "54.0"
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

Sample Mobile (Appium) scenario:
```yaml
scenarios:
  request_example:
    browser: Android-Chrome
    capabilities:
      - device: id_device # set the id of the device here (adb devices)
    # remote: custom_appium_url # You can specify a custom url 
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

services:
- appium
- android-emulator

modules:
  android-emulator:
    avd: android10_arm128
```
