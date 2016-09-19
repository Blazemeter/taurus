require 'json'

class TaurusFormatter
  RSpec::Core::Formatters.register self, :close, :example_passed, :example_failed, :example_pending, :example_started

  def initialize output
    @output = output
    @report_path = ENV.fetch("TAURUS_REPORT_FILE", "report.ldjson")
    @report = File.open(@report_path, 'w')
    @started_at = nil
    @total_tests = 0
    @tests_passed = 0
  end

  def report_stdout test
    test_case = test[:test_case]
    tests_failed = @tests_passed - @total_tests
    @output << "#{test_case},Total:#{@total_tests} Passed:#{@tests_passed} Failed:#{tests_failed}\n"
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
    @report << item.to_json << "\n"
    @tests_passed += 1
    report_stdout item
  end

  def example_failed notification # FailedExampleNotification
    finish_time = Time.new.to_f
    duration = finish_time - @started_at
    item = {:start_time => @started_at,
            :duration => duration,
            :test_case => notification.example.description,
            :test_suite => notification.example.full_description,
            :error_msg => notification.exception.to_s.split(" ").join(" "),
            :error_trace => notification.exception.backtrace.join("\n"),
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

  def close _notification # NullNotification
    @report.close
  end
end

