# RSpec Executor
Allows to run ruby tests based on RSpec.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete or `hold-for` time
will be exceeded.

Usage:
```yaml
execution:
- executor: rspec
  scenario:
    script: tests/  # folder with your tests or path to one test script
```

## Supported file types:

Test scenario may be presented as single ruby file or as a folder.

## Configuration options

You can specify the path to Ruby interpreter, if you don't have it in $PATH:
```yaml
modules:
  rspec:
    interpreter: /home/user/ruby-2.4/bin/ruby
```

The complete example of RSpec-based test suite and Taurus config can be found in
[examples/selenium/rspec-capybara](https://github.com/Blazemeter/taurus/tree/master/examples/selenium/rspec-capybara)
folder of Taurus's repo.
