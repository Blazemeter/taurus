using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.Threading;
using System.Threading.Tasks;
using DotnetTestRunner.Models;
using NUnit.Engine;

namespace DotnetTestRunner.Services.NUnit
{
    public static class NUnitTestRunner
    {
        public static Command GetNUnitCommand()
        {
            var command = new Command("nUnit", "Run tests with nUnit libraries")
            {
                RunnerOptions.IterationOption,
                RunnerOptions.DurationOption,
                RunnerOptions.ConcurrencyOption,
                RunnerOptions.RampUpOption,
                RunnerOptions.ReportFileOption,
                RunnerOptions.TargetAssemblyOption
            };
            command.Handler = CommandHandler.Create<int, int, int, int, string, string>(Handler);
            return command;
        }

        private static async Task Handler(int iterations, int duration, int concurrency, int rampUp, string reportFile, 
            string target)
        {
            if (string.IsNullOrEmpty(target))
            {
                throw new ArgumentException("Nothing to run, no tests library provided");
            }

            var testAssembly = System.Reflection.Assembly.LoadFrom(target);

            var engine = TestEngineActivator.CreateInstance();
            var package = new TestPackage(target);
            var runner = engine.GetRunner(package);
            var testCount = runner.CountTestCases(TestFilter.Empty);

            if (testCount == 0)
            {
                throw new ArgumentException("Nothing to run, no tests were loaded");
            }

            var reportWriter = new ReportWriter(reportFile);

            var userStepTime = rampUp / concurrency;
            var testTasks = new List<Task>();
            var startTime = DateTime.UtcNow;

            for (var i = 1; i <= concurrency; ++i)
            {
                var threadName = $"worker_{i}";

                var runnerForThread = engine.GetRunner(package);
                testTasks.Add(Task.Run(() =>
                        StartWorker(
                            runnerForThread,
                            new NUnitTestEventListener(reportWriter, threadName),
                            startTime,
                            iterations,
                            duration)
                    )
                );
                Thread.Sleep(userStepTime * 1000);
            }

            await Task.WhenAll(testTasks);
            await reportWriter.StopWritingAsync();
        }

        private static void StartWorker(ITestRunner runner, ITestEventListener testEventListener, DateTime startTime,
            int iterations, int duration)
        {
            try
            {
                var iteration = 0;
                while (true)
                {
                    runner.Run(testEventListener, TestFilter.Empty);
                    var offset = DateTime.UtcNow - startTime;
                    var durationStop = ((duration > 0) && (offset.TotalSeconds > duration));
                    var iterationsStop = ((iterations > 0) && (++iteration >= iterations));
                    if (durationStop || iterationsStop)
                    {
                        break;
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("EXCEPTION: {0}", e);
            }
        }
    }
}