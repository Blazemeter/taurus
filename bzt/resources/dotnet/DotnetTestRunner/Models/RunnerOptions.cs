using System.CommandLine;

namespace DotnetTestRunner.Models
{
    public static class RunnerOptions
    {
        public static Option IterationOption { get; } = new Option("--iterations")
        {
            Description = "Number of iterations over test suite to make.",
            Argument = new Argument<int>(() => 0)
        };

        public static Option DurationOption { get; } = new Option("--duration")
        {
            Description = "Duration limit of test suite execution.",
            Argument = new Argument<int>(() => 0)
        };

        public static Option ConcurrencyOption { get; } = new Option("--concurrency")
        {
            Description = "Number of concurrent users.",
            Argument = new Argument<int>(() => 1)
        };

        public static Option RampUpOption { get; } = new Option("--ramp-up")
        {
            Description = "Time to ramp all concurrent users.",
            Argument = new Argument<int>("", () => 0)
        };

        public static Option ReportFileOption { get; } = new Option("--report-file")
        {
            Description = "Name of report file.",
            Argument = new Argument<string>(() => "report.ldjson")
        };

        public static Option TargetAssemblyOption { get; } = new Option("--target")
        {
            Description = "Assembly which will be used to load tests from.",
            Argument = new Argument<string>(() => string.Empty)
        };
    }
}