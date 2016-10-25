require 'json'
require 'optparse'
require 'rspec'

$report_file = 'selenium_report.ldjson'

class TaurusFormatter
  RSpec::Core::Formatters.register self, :example_passed, :example_failed, :example_pending, :example_started, :start, :stop

  attr_reader :output

  def initialize output
    @output = output
    @started_at = nil
    @report = nil
    @total_tests = 0
    @tests_passed = 0
  end

  def start _notification
    @report = File.open($report_file, 'a')
  end

  def stop _notification
    @report.close
  end

  def report_stdout test
    test_case = test[:test_case]
    tests_failed = @total_tests - @tests_passed
    @output << "#{test_case},Total:#{@total_tests} Passed:#{@tests_passed} Failed:#{tests_failed}\n"
    @output.flush
  end

  def example_started _notification
    @total_tests += 1
    @started_at = Time.new.to_f
  end

  def example_passed notification # ExampleNotification
    finish_time = Time.new.to_f
    duration = finish_time - @started_at
    item = {:start_time => @started_at,
            :duration => duration,
            :test_case => notification.example.description,
            :test_suite => notification.example.full_description,
            :status => "PASSED",
            :error_msg => nil,
            :error_trace => nil,
            :extras => nil}
    # TODO: location
    @tests_passed += 1
    report_stdout item
    @report << item.to_json << "\n"
  end

  def example_failed notification # FailedExampleNotification
    finish_time = Time.new.to_f
    duration = finish_time - @started_at
    exception = notification.exception
    item = {:start_time => @started_at,
            :duration => duration,
            :test_case => notification.example.description,
            :test_suite => notification.example.full_description,
            :error_msg => exception.to_s.split(" ").join(" "),
            :error_trace => exception.backtrace.nil? ? nil : exception.backtrace.join("\n"),
            :status => "FAILED",
            :extras => nil}
    @report << item.to_json << "\n"
    report_stdout item
  end

  def example_pending notification # ExampleNotification
    finish_time = Time.new.to_f
    duration = finish_time - @started_at
    item = {:start_time => @started_at,
            :duration => duration,
            :test_case => notification.example.description,
            :test_suite => notification.example.full_description,
            :status => "SKIPPED",
            :error_msg => nil,
            :error_trace => nil,
            :extras => nil}
    @report << item.to_json << "\n"
    @tests_passed += 1
    report_stdout item
  end
end


def parse_options
  options = {}
  OptionParser.new do |opts|
    opts.banner = "Usage: rpsec_taurus_plugin.rb [options]"

    options[:iterations] = 0
    opts.on("-i", "--iterations N", Integer, "Number of iterations over test suite") do |v|
      options[:iterations] = v.to_i
    end

    options[:hold_for] = 0
    opts.on("-h", "--hold-for INTERVAL", Float, "Duration of test suite execution") do |v|
      options[:hold_for] = v.to_f
    end

    opts.on("-r", "--report-file FILE", "Duration of test suite execution") do |f|
      options[:report_file] = f
    end

    opts.on("-s", "--test-suite FILE", "Test suite") do |f|
      options[:test_suite] = f
    end
  end.parse!

  if options[:iterations] == 0
    if options[:hold_for] > 0
      options[:iterations] = 4611686018427387903  # trust me
    else
      options[:iterations] = 1
    end
  end

  options
end

def run_rspec
  options = parse_options
  $report_file = options[:report_file]
  hold_for = options[:hold_for]
  iterations = options[:iterations]
  suite = options[:test_suite]

  rspec_argv = ["--format", "TaurusFormatter", suite]

  start_time = Time.new.to_f
  iterations.times do
    RSpec::Core::Runner.run(rspec_argv)
    RSpec.clear_examples

    offset = Time.new.to_f - start_time
    if hold_for > 0 and offset > hold_for
      break
    end
  end

end

run_rspec
