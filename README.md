# Codename: Taurus

<i><b>T</b>est <b>au</b>tomation <b>ru</b>nning <b>s</b>moothly

## Purpose
Hide the complexity of load testing tools under automation-friendly convenience wrapper.

## Installation
You can avoid long compiling of dependencies by installing them:
```
sudo apt-get install python-yaml python-lxml python-psutil
```

Get the python egg from BlazeMeter's Jenkins and install it:
```
sudo pip install bzt-0.0.tar.gz
```

In the future we'll publish this at PyPi and it will be available as:
```
sudo pip install bzt
```

## Getting Started

Create a file named `test.json` with following contents:

```javascript
{
    "execution": {
        "scenario": {
            "requests": [
                "http://blazemeter.com/",
                "http://a.blazemeter.com/user",
                "http://a.blazemeter.com/app"
            ]
        }
    },
    "modules": {
        "jmeter": {
            "path": "path/to/apache-jmeter-2.12/bin/jmeter"
        }
    }
}
```

Then run `bzt test.json`. After the tool finishes,
observe resulting summary stats in console log. All artifact files from run
will be placed in the directory mentioned in console log.

If you want to run your existing JMX file, use following config:

```javascript
{
    "execution": {
        "scenario": {
            "script": "testplan.jmx"
        }
    },
    "modules": {
        "jmeter": {
            "path": "path/to/apache-jmeter-2.12/bin/jmeter"
        }
    }
}
```


## Documentation

[Taurus Documentation Wiki](https://github.com/Blazemeter/taurus/wiki)