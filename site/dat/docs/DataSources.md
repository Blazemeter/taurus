# Data Sources
Taurus allows you to use external CSV files for testing purposes. It can only be used with
[JMeter](JMeter.md) and [Apiritif](Apiritif.md) executors.

Here is a full list of options for this:
```yaml
scenarios:
  sample:
    requests:
    - http://blazedemo.com/${id}-${name}     # request with data from a file

    data-sources: # list of external data sources
    - path/to/my.csv  # this is a shorthand form
    - path: path/to/another.csv
      delimiter: ','
      quoted: false
      loop: true
      variable-names: id,name
```

Explanation:
  - `path` is a path to a csv file. This option is required.
  - `delimiter` is a CSV delimiter. It is auto-detected by default, but you can use a symbol, 
  i.e. `'.'` for dot, `','` for comma. Also, you can use `'tab'` for a tab symbol.
  - `quoted` allows quoted data. Can be `true` of `false`.
  - `loop` allows to loop over in case of end-of-file reached if `true`, stop thread if `false`.
  - `variable-names` delimiter-separated list of variable names, empty by default.
  - `random-order` enables randomizing plugin; false by default.


When `random-order` is `false`, data extraction will proceed in direct manner. Data lines, which contain delimeters, will be read from the top down to the bottom, just the way they were written. Otherwise, the data will be extracted in a random way.

Also `variable-names` can be omitted. In such case the first line of CSV file will be used as variable names.
