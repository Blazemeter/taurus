using Mono.Options;
using NUnit.Engine;
using NUnitRunner.Models;
using NUnitRunner.Services;
using System;
using System.Collections.Concurrent;
using System.Threading;
using System.Threading.Tasks;


namespace NUnitRunner
{
    public class NUnitRunner
    {
        public static void Main(string[] args)
        {
            MainAsync(args).GetAwaiter().GetResult();
        }

        private static async Task MainAsync(string[] args)
        {
            var options = new RunnerOptions();

            try
            {
                options = OptionsParser.ParseOptions(args);
            }
            catch (OptionException e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine("Try running with '--help' for more information.");
                Environment.Exit(1);
            }

            var engine = TestEngineActivator.CreateInstance(true);
            var package = new TestPackage(options.TargetAssembly);

            var reportItems = new ConcurrentQueue<ReportItem>();
            var testEventListener = new TestEventListener(engine, package, reportItems);

            var testCount = testEventListener.Runner.CountTestCases(TestFilter.Empty);
            if (testCount == 0)
            {
                throw new ArgumentException("Nothing to run, no tests were loaded");
            }

            if (options.RampUp > 1 && options.DurationLimit > 0)
            {
                options.DurationLimit = options.DurationLimit + options.RampUp;
            }

            var userStepTime = options.RampUp / options.Concurrency;
            var reportWriter = new ReportWriter(reportItems);
            var reportWriterTask = Task.Run(() => reportWriter.StartWriting(options.ReportFile));
            var startTime = DateTime.UtcNow;
            var testTasks = new Task[options.Concurrency];

            for (int i = 0; i < options.Concurrency; i++)
            {
                testTasks[i] = Task.Run(() => Test.RunTest(startTime, options, new TestEventListener(engine, package, reportItems)));              
                Thread.Sleep(userStepTime * 1000);
            }

            await Task.WhenAll(testTasks);
            reportWriter.TestsCompleted = true;
            await reportWriterTask;
        }
    }
}
