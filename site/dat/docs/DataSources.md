# Data Sources
This allows to include external data sources files to a yaml config. Can only be used with
[JMeter](JMeter.md) and [Apiritif](Apiritif.md) executors.

Here is an example:
```yaml
scenarios:
  sample:
    requests:
    - http://blazedemo.com/{id}_{name}     # request with data from a file

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


