# Codename: Taurus

<i><b>T</b>est <b>au</b>tomation <b>ru</b>nning <b>s</b>moothly

## Purpose
Hide the complexity of load testing tools under automation-friendly convenience wrapper.

## Installation or Upgrade

Just install it using PyPi (more detailed instructions for Linux, Mac OS and Windows are [here](https://github.com/Blazemeter/taurus/wiki/Installation)):

```bash
pip install bzt
```

To quickly upgrade existing installation, do:

```bash
pip install --upgrade --no-deps bzt
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

  ramp-up: 1m
  hold: 1m30s
```

Then run `bzt test.yml`. After the tool finishes,
observe resulting summary stats in console log. All artifact files from the run
will be placed in the directory mentioned in console log.

## Further Reading

[Taurus Documentation Wiki](https://github.com/Blazemeter/taurus/wiki)

[Support Forum](https://groups.google.com/forum/#!forum/codename-taurus)