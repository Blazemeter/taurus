# Taurus 

[![Join the chat at https://gitter.im/Blazemeter/taurus](https://badges.gitter.im/Blazemeter/taurus.svg)](https://gitter.im/Blazemeter/taurus?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Quick links: [Taurus Documentation](http://gettaurus.org/docs/) | [Knowledge Base](http://gettaurus.org/kb/) | [Support Forum](https://groups.google.com/forum/#!forum/codename-taurus)

## Purpose
Hides the complexity of performance and functional tests with an automation-friendly convenience wrapper. Taurus relies on JMeter, Gatling, Locust.io, Grinder and Selenium WebDriver as its underlying tools. Free and open source under Apache 2.0 License.

## Fork Purpose (3dgiordano)
Improve the current support of Selenium Webdriver, providing support for more selenium commands, remote-webdriver support and other functionalities commonly used in selenium scripts.
The improvements made will be proposed to the Taurus team, as long as they are not proposed and accepted, they have my fork to take advantage of and use all the new features incorporated.

## Improvements to Taurus (Finished, in development or to be developed)
- Command support to CLI
- New Command bzt remote - Allow remote services instances to be used by Taurus, currently focused on Selenium / Appium
- Support for new Selenium commands in Taurus YML
- Appium support in Taurus YML and its main commands
- VNC Viewer - to be able to interactively visualize the execution of remote services if they allow it
- Collection of all the logs provided by the remote services
- Remote WebDriver support to allow connection with third party services
- To be able to spread the information of remote web driver towards external script, allowing taurus and ml to simplify the use of new services or the use of other providers of remote web driver
- A report by console oriented to the functional results of the selenium script or the taurus selenium script generated.

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
