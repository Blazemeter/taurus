using System;
using Mono.Options;
using NUnitRunner.Models;

namespace NUnitRunner.Services
{
    public static class OptionsParser
    {
        public static RunnerOptions ParseOptions(string[] args)
        {
            RunnerOptions options = new RunnerOptions();

            var optionSet = new OptionSet {
                { "i|iterations=", "number of iterations over test suite to make.", (int n) => options.Iterations = n },
                { "d|duration=", "duration of test suite execution.", (int d) => options.DurationLimit = d },
                { "c|concurrency=", "number of concurrent users.", (int c) => options.Concurrency = c },
                { "l|ramp_up=", "time to ramp all concurrent users.", (int l) => options.RampUp = l},
                { "r|report-file=", "Name of report file", r => options.ReportFile = r },
                { "t|target=", "Test suite", t => options.TargetAssembly = t },
                { "h|help", "show this message and exit", h => options.ShouldShowHelp = h != null },
            };

            optionSet.Parse(args);

            if (options.ShouldShowHelp)
            {
                ShowHelp();
            }

            if (options.TargetAssembly == null)
            {
                throw new Exception("Target test suite wasn't provided. Is your file actually NUnit test DLL?");
            }

            if (options.Iterations <= 0)
            {
                options.Iterations = options.DurationLimit > 0 ? int.MaxValue : 1;
            }

            if (options.Concurrency <= 0)
            {
                options.Concurrency = 1;
            }

            if (options.RampUp <= 0)
            {
                options.RampUp = 1;
            }

            Console.WriteLine("Iterations: {0}", options.Iterations);
            Console.WriteLine("Hold for: {0}", options.DurationLimit);
            Console.WriteLine("Current users: {0}", options.Concurrency);
            Console.WriteLine("Ramp period: {0}", options.RampUp);
            Console.WriteLine("Report file: {0}", options.ReportFile);
            Console.WriteLine("Target: {0}", options.TargetAssembly);

            return options;
        }

        private static void ShowHelp()
        {
            Console.WriteLine("NUnit Runner for Taurus");
            Console.WriteLine("Usage:");
            Console.WriteLine("\t <executable> ARGS");
            Console.WriteLine("\t --iterations N - number of iterations over test suite to make");
            Console.WriteLine("\t --duration T - duration limit of test suite execution");
            Console.WriteLine("\t --concurrency N - number of concurrent users");
            Console.WriteLine("\t --ramp_up L - time to ramp all concurrent users");
            Console.WriteLine("\t --report-file REPORT_FILE - filename of report file");
            Console.WriteLine("\t --target TARGET_ASSEMBLY - assembly which will be used to load tests from");
            Console.WriteLine("\t --help - show this message and exit");
            Environment.Exit(0);
        }
    }
}
