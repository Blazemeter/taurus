# Taurus 

[![Join the chat at https://gitter.im/Blazemeter/taurus](https://badges.gitter.im/Blazemeter/taurus.svg)](https://gitter.im/Blazemeter/taurus?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Quick links: [Taurus Documentation](http://gettaurus.org/docs/) | [Knowledge Base](http://gettaurus.org/kb/) | [Support Forum](https://groups.google.com/forum/#!forum/codename-taurus)

## Purpose
Hides the complexity of performance and functional tests with an automation-friendly convenience wrapper. Taurus relies on JMeter, Gatling, Locust.io, Grinder and Selenium WebDriver as its underlying tools. Free and open source under Apache 2.0 License.

## Fork Purpose (3dgiordano)
Improve the current support of Selenium Webdriver, providing support for more selenium commands, remote-webdriver support and other functionalities commonly used in selenium scripts.
The improvements made will be proposed to the Taurus team, as long as they are not proposed and accepted, they have my fork to take advantage of and use all the new features incorporated.

## Improvements to Taurus

- Icon in taurus UI :) (goodbye to the default tkinter icon)

- Command support to CLI (Done) - Run bzt -h and you will see a new section "Commands" in the style of tools like docker, aws-cli and other similar

- New Command bzt remote (WIP) - Allow remote services instances to be used by Taurus, currently focused on Selenium / Appium
     - Catalog (Done) - bzt remote catalog : Lists the set of available services to be used in the execution of the taurus script.
     - Attach (Done) - bzt remote attach : Attach an instance of the service you want to use
     - Detach (Done) - bzt remote detach : Release the attached instance
     - List (Done) - bzt remote list : Lists the status of attached instances
     - Inspect (TODO) - bzt remote inspect : Provides more detail about the attached instance

- Browser support at Runner level (Done) - Now it is possible to share the same scenario to different browsers and make the generation in a more dynamic way.

- Support for new Selenium commands in Taurus YML
     - Remote (Done) - New internal browser for remote webdriver
     - doubleClickBy* (Done)
     - mouseDownBy* (Done)
     - mouseMoveBy* (Done)
     - mouseUpBy* (Done)
     - selectBy* (Done)
     - assertTitle (Done)
     - assertTextBy* (Done)
     - Special keys support (Like KEY_ENTER, KEY_TAB and [others](http://selenium-python.readthedocs.io/api.html#module-selenium.webdriver.common.keys)
     
- Initial VNC Viewer (Done) - You can see what happens during the execution of your remote services execution * only for remote service instances.

- Appium support in Taurus YML and its main commands (WIP)
     - Appium locators 
     - click
     - clear
     - sendKeys
     - back
     - tap
     - swipe

- Collection of all the logs provided by the remote services (WIP) * only for remote service instances.

- Remote WebDriver support to allow connection with third party services (Done)
     - Tested with Taurus Remote Services, local browsers, Saucelabs, BrowserStack, Selenium Grid and others
     - New capabilities oriented connection
       - browser
       - version
       - plarform
       - selenium_version

- WebDriver connection to Script (Done) To be able to spread the information of remote web driver towards external script, allowing taurus and ml to simplify the use of new services or the use of other providers of remote web driver
     - Environment variables
       - BZT_REMOTE : Remote url
       - BZT_REMOTE_BROWSER : Browser name for the capability
       - others capabilities (TODO)

- Selenium test result (TODO) A report by console oriented to the functional results of the selenium script or the taurus selenium script generated.

## Fork Status (visibility, compatibility and quality matters)

[Appveyor](https://ci.appveyor.com/project/3dgiordano/taurus)
[Codecov](https://codecov.io/gh/3dgiordano/taurus)
[Travis CI](https://travis-ci.org/3dgiordano/taurus)
[Codacy](https://www.codacy.com/app/3dgiordano/taurus/dashboard)

## Local fork installation
Having the prerequisites installed, python + pip
Download the fork version

Run:
```bash
pip install -r requirements.txt
python setup.py install
bzt -h 
```
Verify the version
```bash
bzt -h
...
BlazeMeter Taurus Tool v1.10.4.3dgiordano, the configuration-driven test
running engine
...
```

## The master documentation starts here

## Installation or Upgrade

Just install it using PyPi:

```bash
sudo pip install bzt
```

More detailed instructions for Linux, Mac OS and Windows available [here](http://gettaurus.org/docs/Installation.md).

## Getting Started

Create a file named `test.yml` with following contents:

```yaml
---
execution:
- concurrency: 10
  ramp-up: 1m
  hold-for: 1m30s
  scenario: simple
  
scenarios:
  simple:
    think-time: 0.75
    requests:
    - http://blazedemo.com/
    - http://blazedemo.com/vacation.html
```

Then run `bzt test.yml`. After the tool finishes, observe resulting summary stats in console log (more reporting options [here](http://gettaurus.org/docs/Reporting.md)). All artifact files from the run will be placed in the directory mentioned in console log. Read more on command-line tool usage [here](http://gettaurus.org/docs/CommandLine.md).

![Analytics](https://ga-beacon.appspot.com/UA-63369152-1/taurus/readme)
