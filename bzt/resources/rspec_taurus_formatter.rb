require 'json'

class TaurusFormatter
  RSpec::Core::Formatters.register self, :dump_summary, :close, :example_passed, :example_failed, :example_pending, :example_started

  def initialize output
    @output = output
    @report = File.open('selenium_tests_report.ldjson', 'w')
    @started_at = nil
  end

  def example_started _notification
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
    # TODO: licalocation
    @report << item.to_json << "\n"

    @output << "."
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

    @output << "F"
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

    @output << "*"
  end

  def dump_summary notification # SummaryNotification
    @output << "\n\nFinished in #{RSpec::Core::Formatters::Helpers.format_duration(notification.duration)}."
  end

  def close _notification # NullNotification
    @output << "\n"
    @report.close
  end
end

