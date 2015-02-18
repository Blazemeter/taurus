# Codename: Taurus

<i><b>T</b>est <b>au</b>tomation <b>ru</b>nning <b>s</b>moothly

## Purpose
Hide the complexity of load testing tools under automation-friendly convenience wrapper.

## Installation or Upgrade

Just install it using PyPi:
```
sudo pip install --upgrade bzt
```

## Getting Started

Create a file named `test.yml` with following contents:

```yaml
---
execution:
  scenario:
    requests:
      - http://blazedemo.com/
      - http://blazedemo.com/vacation.html

  hold: 1m
```

Then run `bzt test.yml`. After the tool finishes,
observe resulting summary stats in console log. All artifact files from run
will be placed in the directory mentioned in console log.

## Further Reading

[Taurus Documentation Wiki](https://github.com/Blazemeter/taurus/wiki)