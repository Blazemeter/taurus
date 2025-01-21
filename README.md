# Taurus 

Quick links: [Taurus Documentation](https://gettaurus.org/docs/) | [Knowledge Base](https://gettaurus.org/kb/) | [GitHub Issues](https://github.com/Blazemeter/taurus/issues)

## Purpose
Hides the complexity of performance and functional tests with an automation-friendly convenience wrapper. Taurus relies on JMeter, Gatling, Locust.io, and Selenium WebDriver as its underlying tools. Free and open source under Apache 2.0 License.


## Installation or Upgrade

Just install it using PyPi:

```bash
pip install bzt
```

For more detailed instructions for Linux, Mac OS and Windows, see [Installing and Upgrading](https://gettaurus.org/docs/Installation.md).

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

Then run `bzt test.yml`. After the tool finishes, observe resulting summary stats in console log (for more reporting options, see [Generating Test Reports](https://gettaurus.org/docs/Reporting.md)). All artifact files from the run will be placed in the directory mentioned in console log. Read more on command-line tool usage [Command-Line Tool](https://gettaurus.org/docs/CommandLine.md).

![Analytics](https://ga-beacon.appspot.com/UA-63369152-1/taurus/readme)

