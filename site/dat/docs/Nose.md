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
  - select browser Chrome, Firefox, Ie, Opera, Android-Chome, iOS-Safari, Remote
  - local webdriver, local remote webdriver or remote webdriver (selenium grid or others)
  - capabilities for remote webdriver - browser, version, javascript, platform, os_version, selenium, device, app
  - set timeout/think-time on both scenario and request levels
  - assertions (requested page source inspected use the new assertTitle, assertTextBy or assertValueBy* for item level)
  - pauseFor (pause for n seconds) 
  - request method GET (only)
  - selenium commands:
    - window control (selectWindow, closeWindow)
    - selectFrameBy* Switch to frame
    - keysBy* Send keys to element
    - editContent Change text in editable field (checks contenteditable prop)
    - submitBy* Send data of form by any its element
    - runScript Execute JS command
    - waitBy* 
    - clickBy* 
    - doubleClickBy* 
    - mouseDownBy* 
    - mouseUpBy* 
    - assertTextBy* Assert text on element
    - assertValueBy* Assert value attribute
    - selectBy* Select value in drop down list
    - assertTitle

Note: * selected by ID/Name/CSS/XPath 
   
Action names are built as `<action>By<selector type>(<selector>)`. Sometimes actions can have value. Options are:
  - `waitByID`, `waitByName`, `waitByLinkText`, `waitByCSS` and `waitByXPath` - to wait until desired option becomes present on page.
  Timeout is taken from scenario-level `timeout` option. Optionally, you can specify parameter `visible` to wait
  for visibility rather than presence (like `waitByName(elmName): visible`)
  - `clickByID`, `clickByName`, `clickByLinkText`, `clickByCSS` and `clickByXPath` - no parameter, just click on object
  - `keysByID`, `keysByName`, `keysByLinkText`, `keysByCSS` and `keysByXPath` - to enter keys into input items, requires parameter.
  Like this: `keysByName(MyFormInputName): Value To Enter`. Special keys are allowed using the prefix `KEY\_` . Like this: `keysByName(MyFormInputName): KEY\_ENTER`.  See some of the possible values supported [here](http://selenium-python.readthedocs.io/api.html#module-selenium.webdriver.common.keys)

There is special action `pauseFor(<time>)` which makes script to sleep for specified amount of time. Also, calling action `clearCookies()` will force `delete\_all\_cookies` method to be called on WebDriver object.

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
      - clickByID(mySubmitButton)   # link is open in new window (#1)
      - selectWindow(0)     # switch to the first window (#0)
      - closeWindow(1)      # close the second window (#1)
      - pauseFor(5s)
      - clearCookies()
      - keysByName(myInputName): keys_to_type
      - submitByName(myInputName)
      - waitByID(myObjectToAppear): visible
      - runScript("alert('This is Sparta');")
      assert: # assert executed after actions
      - contains:
        - blazemeter  # list of search patterns
        - Trusted
        subject: body # only body subject supported
        regexp: false  # treat string as regular expression
        not: false  # inverse assertion condition
```
All action names are case insensitive. Despite it misprint in action names or usage of unsupported actions break your scenario execution. To avoid it you can use 'safe-mode' scenario param and taurus will show warning when unknown action occurs.

## Remote WebDriver

It is possible to use the browser remotely using Remote WebDriver. It must be indicated as the browser name `Remote` and indicate in the `remote` property the URL in which the webdriver service is located to control the browser.

To specify the capabilities of the Remote WebDriver, it is necessary to be able to configure properties necessary for remote instantiation. You must use the `capabilities` structure where you can specify the main properties required by the remote webdriver.

Note: The capabilities are a way in which the remote service filters and selects the device or browser to be selected for the test, depending on its configuration according to the configured specifications. It is recommended to read the documentation of who provides the service

### Capabilities commonly used 

  - browser
  - version
  - platform
  - device # Id of the device (Mobile browser)
  - os_version # commonly used only for mobile

Note: Currently it is possible to perform basic tests in mobile browsers using the available actions commands, in the future more commands related to mobile will be incorporated to allow a better interactivity.

Sample Remote Webdriver scenario:
```yaml
scenarios:
  request_example:
    browser: Remote
    remote: http://user:key@remote_web_driver_host:port/wd/hub
    capabilities:
      browser: firefox  # Depends on the capabilities of the remote selenium server
      version: "54.0"
    requests:
    - url: http://demo.blazemeter.com  # url to open, only get method is supported
      actions:  # holds list of actions to perform
      - waitByCSS(body)
    # ...
```
It is possible to use only the `remote` property, and in this way declare the intention to use the `browser: Remote`, allowing a more compact yaml

Sample usage of `remote` without `browser: Remote` clausule declaration:
```yaml
scenarios:
  request_example:
    remote: http://user:key@remote_web_driver_host:port/wd/hub
    capabilities:
      browser: firefox  # Depends on the capabilities of the remote selenium server
      version: "54.0"
    requests:
    - url: http://demo.blazemeter.com  # url to open, only get method is supported
      actions:  # holds list of actions to perform
      - waitByCSS(body)
    # ...
```

## Mobile Browsers

It is also possible to perform tests on mobile browsers. Currently the browsers supported are `Chrome-Android` and `Safari-iOS`.
Mobile test services are provided by Appium, and it is possible to use Appium locally or some remote Appium service through the Remote WebDriver capability.

Note: Taurus provides the ability to provide Appium provisioning support, it is recommended to read the documentation related to [Selenium Executor / Appium](Selenium.md#appium)

Sample Mobile Browser scenario:
```yaml
scenarios:
  request_example:
    browser: Chrome-Android
    capabilities:
      device: id_device # set the id of the device here (adb devices)
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

