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
    - go(url) Redirect to another website
    - window handler (openWindow, switchWindow, closeWindow)
    - frame handler (switchFrame, switchFrameBy*<sup>1</sup>)
    - keysBy* Send keystrokes to element
    - typeBy* Assign the value to element, cleaning it previously
    - editContent Change text in editable field (checks contenteditable prop)
    - selectBy* Select value in drop down list
    - submitBy* Send data of form by any its element
    - scriptEval Execute JS command
    - echoString(text) Print text string on the Nose output execution
    - waitBy* 
    - clickBy* 
    - doubleClickBy* 
    - mouseDownBy* 
    - mouseUpBy*
    - dragBy*<sup>2</sup>
    - assertTextBy* Assert text on element
    - assertValueBy* Assert value attribute
    - assertTitle
    - storeTitle Store title in a variable
    - storeString Store a string or template in a variable
    - storeTextBy* Store text from element in a variable
    - storeValueBy* Store value from eleent in a variable
    - takeScreenshot(filename) Take a screenshot of a viewport and save it in a file

**Notes**:
  - \* selected by ID/Name/CSS/XPath.
  - \*<sup>1</sup> In addition you can use *ByIdx selector.
  - \*<sup>2</sup> To select the target drop element, the elementBy\* command must be used. Sample: dragByID(a): elementByID(b)
   
Action names are built as `<action>By<selector type>(<selector>)`. Sometimes actions can have value. Options are:
  - `waitByID`, `waitByName`, `waitByLinkText`, `waitByCSS` and `waitByXPath` - to wait until desired option becomes present on page.
  Timeout is taken from scenario-level `timeout` option. Optionally, you can specify parameter `visible` to wait
  for visibility rather than presence (like `waitByName(elmName): visible`)
  - `clickByID`, `clickByName`, `clickByLinkText`, `clickByCSS` and `clickByXPath` - no parameter, just click on object
  - `keysByID`, `keysByName`, `keysByLinkText`, `keysByCSS` and `keysByXPath` - to enter keys into input items, requires parameter.
  Like this: `keysByName(MyFormInputName): Value To Enter`. Special keys are allowed using the prefix `KEY\_` . Like this: `keysByName(MyFormInputName): KEY\_ENTER`.  See some of the possible values supported [here](http://selenium-python.readthedocs.io/api.html#module-selenium.webdriver.common.keys)

There is special action `pauseFor(<time>)` which makes script to sleep for specified amount of time. Also, calling action `clearCookies()` will force `delete\_all\_cookies` method to be called on WebDriver object.

#### Reporting
It is recommended to incorporate the use of the `final-stats` module with the option enabled `summary-labels` to have a better vision of results.
It will allow to visualize the status of the tests, the average time per transaction and to visualize the errors that may occur in webdriver and other components involved in the test.

Sample configuration:
```yaml
reporting:
- module: final-stats
  summary-labels: true
```
Sample output:
```
+-----------------+--------+---------+----------+-------+
| label           | status | success | avg time | error |
+-----------------+--------+---------+----------+-------+
| Find Flights    |   OK   | 100.00% |  10.581  |       |
| Reserve Flight  |   OK   | 100.00% |  1.276   |       |
| Purchase Flight |   OK   | 100.00% |  4.951   |       |
| Thanks          |   OK   | 100.00% |  0.062   |       |
+-----------------+--------+---------+----------+-------+
```

#### Window managment
To manage windows or tabs, the `switchWindow(<value>)` and `closeWindow(<value>)` commands will allow you to manage them.

These actions require a value parameter, the possible values are:
  - `number`: The index to the window in reference, 0 is the first, 1 is the second, and so with those who want to manage. 
  - `name`: The name of the window (reference to the name used in the target window attribute in a link).
  - `win_ser_name`: In the `name` part, assign a name to the focused opened window, the next time when reference to the same window name, returns with focus to the named window selected. 
  - `win_ser_local`: Go to the initial window.
  - `no value`: When no value is assigned, it means that the selection action is assigned over the last created window, and if the close action is used, it will also be over the last one created.

**Note**: When any action command opens a new window (like click over a link with target window assigned), the action of selecting the window must always be declared, otherwise the actions executed by the execution were performed on the default window or the last one used with selectWindow command.

#### Frame managmment
When you need to perform actions on elements that are inside a frame or iframe, you must use the `switchFrame` command to activate the frame before perform any action.

Sample usage
```yaml
scenario:
  sample_frame:
    requests:
    - url: http://a-frame-sample.com
      actions:
      - switchFrame(index=0) # Select First Frame
      - assertTextByCSS(body): "First Frame Body"
      - switchFrame(relative=parent) # Go to parent
      - switchFrame(index=1) # Select Second frame
      - switchFrame(index=0) # Nested Frame, Select the First frame inside the Top Second frame
      - assertTextByID(content): "First Frame Body inside Second Frame Body"
      - switchFrame(relative=top) # Go to top frame (main document)
```
**Note**: For first level frames, it is possible to use switchFrameBy* and using selector to match the frame to switch.

**Disclaimer**: Currently there are problems in the support of this functionality by geckodriver and chromedriver, depending on the case to test some of these methods can generate a failure, mainly in cases where you have nested frames or frames mixed between frame and iframes.

#### Sample request scenario:
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
      - openWindow(http://blazedemo.by) # new window is created (#1)
      - switchWindow(1)     # switch to the second window (#0)
      - closeWindow()      # close the second window (#1)
      - pauseFor(5s)
      - clearCookies()
      - keysByName(myInputName): keys_to_type
      - submitByName(myInputName)
      - waitByID(myObjectToAppear): visible
      - scriptEval("alert('This is Sparta');")
      assert: # assert executed after actions
      - contains:
        - blazemeter  # list of search patterns
        - Trusted
        subject: body # only body subject supported
        regexp: false  # treat string as regular expression
        not: false  # inverse assertion condition
```
All action names are case insensitive. Despite it misprint in action names or usage of unsupported actions break your scenario execution. 
To avoid it you can use `ignore-unknown-actions` Nose flag and taurus will show warning when unknown action occurs.
```yaml
scenario:
  sample:
    requests:
    - url: http://blazedemo.com
      actions:
      - definitelyUnknownAction(unknownSelector) 
modules:
  nose:
    ignore-unknown-action: True   # 

```

### Variables

It is possible to define variables to be used in the script, declaring them at the scenario level.

To use it, simply in any reference to a text in the script you must declare the insertion of the variable by using ```${name}```

The use of variables can be used in many reference locations, in selectors or in values. 
There are also commands that allow you to store and manipulate them.
Some of them are storeTitle, storeTextBy *, storeValueBy * and storeString

#### Sample:
```yaml
scenario:
  sample:
    variables:
        sample: The is a sample site you can test with BlazeMeter!
    requests:
    - url: http://blazedemo.com/
      actions:
      - assertTextByCSS(body > div.jumbotron > div > p:nth-child(2)): ${sample}
      - storeTitle(): my_title
      - storeTextByXPath(//*[@id="elemid"]/h2): my_text
      - storeValueByXPath(//*[@id="elemid"]/input): my_value
      - storeString(${my_title} love my ${my_text} with ${my_value}): final_text
```


### Remote WebDriver

It is possible to use the browser remotely using Remote WebDriver. It must be indicated as the browser name `Remote` and indicate in the `remote` property the URL in which the webdriver service is located to control the browser.

To specify the capabilities of the Remote WebDriver, it is necessary to be able to configure properties necessary for remote instantiation. You must use the `capabilities` structure where you can specify the main properties required by the remote webdriver.

**Note**: The capabilities are a way in which the remote service filters and selects the device or browser to be selected for the test, depending on its configuration according to the configured specifications. It is recommended to read the documentation of who provides the service

#### Capabilities commonly used 

  - browser
  - version
  - platform
  - device # Id of the device (Mobile browser)
  - os_version # commonly used only for mobile

**Note**: Currently it is possible to perform basic tests in mobile browsers using the available actions commands, in the future more commands related to mobile will be incorporated to allow a better interactivity.

#### Sample Remote Webdriver scenario:
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
It is possible to use only the `remote` option, and in this way declare the intention to use the `browser: Remote`, allowing a more compact YAML.

**Sample usage of `remote` without `browser: Remote` clause declaration:**
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

Note that `remote` keyword can be used on module settings, execution settings, or scenario level.

### Mobile Browsers

It is also possible to perform tests on mobile browsers. Currently the browsers supported are `Chrome-Android` and `Safari-iOS`.
Mobile test services are provided by Appium, and it is possible to use Appium locally or some remote Appium service through the Remote WebDriver capability.

**Note**: Taurus provides the ability to provide Appium provisioning support, it is recommended to read the documentation related to [Selenium Executor / Appium](Selenium.md#appium)

#### Sample Mobile Browser scenario:
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
