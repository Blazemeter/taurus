# Running RSpec-based tests with Taurus

Here's a simple web application with a test suite based on Rails 4, RSpec and Capybara.

## Preparation

1. Ensure that you have Ruby 2.x installed. If your OS doesn't have Ruby 2.x — you might want to use rvm.
2. Invoke `bundle check` to generate Gemfile.lock file
3. Execute `bundle install` to install all dependencies (you may need to install development versions of Ruby and some libraries, like sqlite3).
4. Ensure that RSpec was installed correctly by running `rspec spec` from this directory. It should successfully execute all tests.

## Running tests with Taurus

Check out the `test.yml` file. It has minimal Taurus configuration to execute our test suite.
It also has `iterations` option set to 3, so Taurus will run the entire test suite 3 times.
You can also use `hold-for` option to specify time limit on test execution.
