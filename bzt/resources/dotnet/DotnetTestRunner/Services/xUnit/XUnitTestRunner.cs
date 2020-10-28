using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.Threading;
using System.Threading.Tasks;
using DotnetTestRunner.Models;
using Xunit.Runners;

namespace DotnetTestRunner.Services.xUnit
{
    public static class XUnitTestRunner
    {
        public static Command GetNUnitCommand()
        {
            var command = new Command("xUnit", "Run tests with xUnit libraries")
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
            var reportWriter = new ReportWriter(reportFile);

            var userStepTime = rampUp / concurrency;
            var testTasks = new List<Task>();
            var startTime = DateTime.UtcNow;

            var testAssembly = System.Reflection.Assembly.LoadFrom(target);

            for (var i = 1; i <= concurrency; ++i)
            {
                var threadName = $"worker_{i}";
                var eventListener = new XUnitTestEventListener(reportWriter, threadName);

                var runner = AssemblyRunner.WithoutAppDomain(target);
                runner.OnDiscoveryComplete = XUnitTestEventListener.OnDiscoveryComplete;

                runner.OnTestFailed = eventListener.OnTestFailed;
                runner.OnTestSkipped = eventListener.OnTestSkipped;
                runner.OnTestFinished = eventListener.OnTestFinished;

                testTasks.Add(Task.Run(() => StartWorker(runner, startTime, iterations, duration)));
                Thread.Sleep(userStepTime * 1000);
            }

            await Task.WhenAll(testTasks);
            await reportWriter.StopWritingAsync();
        }

        private static void StartWorker(AssemblyRunner runner, DateTime startTime, int iterations, int duration)
        {
            try
            {
                var iteration = 0;
                while (true)
                {
                    RunAllTests(runner);
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
                ;
            }
        }

        private static void RunAllTests(AssemblyRunner runner)
        {
            using var executionCompleteEvent = new ManualResetEvent(false);
            var onExecutionComplete = runner.OnExecutionComplete;

            runner.OnExecutionComplete += info => executionCompleteEvent.Set();
            runner.Start();
            executionCompleteEvent.WaitOne();

            runner.OnExecutionComplete = onExecutionComplete;
        }
    }
}