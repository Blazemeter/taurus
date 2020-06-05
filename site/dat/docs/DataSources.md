# Data Sources
Taurus allows you to use external CSV files for testing purposes. It can only be used with
[JMeter](JMeter.md), [Apiritif](Apiritif.md), and [Gatling](Gatling.md) executors.

Please keep in mind that **not all options are supported by each executor**. 

Here is a full list of options:
```yaml
scenarios:
  sample:
    requests:
    - http://blazedemo.com/${id}-${name}     # request with data from a file

    data-sources: # list of external data sources
    - path/to/my.csv  # this is a shorthand form
    - path: path/to/another.csv  # this is a full form
      delimiter: ','
      quoted: false
      loop: true
      variable-names: id,name
      random-order: false
```

Explanation:
  - `path` is a path to a csv file. This option is required.
  - `delimiter` is a CSV delimiter. It is auto-detected by default, but you can use a symbol, i.e. `'.'` for dot, `','` for comma. Also, you can use `'tab'` for a tab symbol.
  - `quoted` allows quoted data. Can be `true` of `false`.
  - `loop` allows to loop over in case of end-of-file reached if `true`, stop thread if `false`.
  - `variable-names` delimiter-separated list of variable names, empty by default. When omitted, the first line of CSV file will be used as variable names.
  - `random-order` enables randomizing plugin; false by default. Available only for JMeter.

When `random-order` is `false`, data extraction will proceed in direct manner. Data lines, which contain delimeters, will be read from the top down to the bottom, just the way they were written. Otherwise, the data will be extracted in a random way.
