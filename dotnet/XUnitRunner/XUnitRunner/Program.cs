using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Loader;
using System.Threading;
using Microsoft.Extensions.Configuration;
using Xunit.Runners;

namespace XUnitRunner
{
    public class Program
    {
        // We use consoleLock because messages can arrive in parallel, so we want to make sure we get
        // consistent console output.
        private static readonly object ConsoleLock = new object();

        // Use an event to know when we're done
        private static readonly ManualResetEvent Finished = new ManualResetEvent(false);

        private static RecordingListener _recordingListener;
        // Start out assuming success; we'll set this to 1 if we get a failed test


        public static IConfiguration Configuration { get; set; }

        public static void ShowHelp()
        {
            Console.WriteLine("XUnit runner for Taurus");
            Console.WriteLine("Usage:");
            Console.WriteLine("\t <executable> ARGS");
            Console.WriteLine("\t --iterations N - number of iterations over test suite to make");
            Console.WriteLine("\t --duration T - duration limit of test suite execution");
            Console.WriteLine("\t --report-file REPORT_FILE - filename of report file");
            Console.WriteLine("\t --target TARGET_ASSEMBLY - assembly which will be used to load tests from");
            Console.WriteLine("\t --help - show this message and exit");
            Environment.Exit(0);
        }

        public static Dictionary<string, string> GetSwitchMappings(string[] args)
        {
            return args.Select(item =>
                    new KeyValuePair<string, string>(item.Replace("/", "").Replace("-", "").Split('=')[0], item.Split('=')?[1]))
                .ToDictionary(item => item.Key, item => item.Value);
        }

        private static RunnerOptions GetRunnerOptions(IReadOnlyDictionary<string, string> dict)
        {
            var opts = new RunnerOptions();

            if (dict.Count == 0 || dict.ContainsKey("h") || dict.ContainsKey("help"))
            {
                opts.shouldShowHelp = true;
                ShowHelp();
            }

            foreach (KeyValuePair<string, string> pair in dict)
            {
                Console.WriteLine("{0} - {1}", pair.Key, pair.Value);
            }

            if (!dict.TryGetValue("target", out opts.targetAssembly))
            {
                throw new Exception("Target test suite wasn't provided.");
            }

            if (!File.Exists(opts.targetAssembly))
            {
                throw new Exception($"Target test suite cannot be found {opts.targetAssembly}");
            }

            if (dict.ContainsKey("reportfile"))
            {
                opts.reportFile = dict["reportfile"];
            }

            if (dict.TryGetValue("iterations", out var sValue))
            {
                int.TryParse(sValue, out opts.iterations);
            }

            if (dict.TryGetValue("duration", out sValue))
            {
                int.TryParse(sValue, out opts.durationLimit);
            }

            if (opts.iterations == 0) opts.iterations = opts.durationLimit > 0 ? int.MaxValue : 1;

            Console.WriteLine("Iterations: {0}", opts.iterations);
            Console.WriteLine("Hold for: {0}", opts.durationLimit);
            Console.WriteLine("Report file: {0}", opts.reportFile);
            Console.WriteLine("Target: {0}", opts.targetAssembly);

            return opts;
        }


        public static void Main(string[] args)
        {
            var runnerOptions = new RunnerOptions();

            try
            {
                runnerOptions = GetRunnerOptions(GetSwitchMappings(args));
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine("Try running with '--help' for more information.");
                var stList = e.StackTrace.ToString().Split('\\');
                Console.WriteLine("Exception occurred at " + stList[stList.Count() - 1]);
                Environment.Exit(1);
            }

            try
            {
                using (_recordingListener = new RecordingListener(runnerOptions.reportFile))
                {
                    var startTime = DateTime.Now;

                    var assembly = AssemblyLoadContext.Default.LoadFromAssemblyPath(runnerOptions.targetAssembly);

                    using (var runner = AssemblyRunner.WithoutAppDomain(assembly.Location))
                    {
                        runner.OnDiscoveryComplete = OnDiscoveryComplete;
                        runner.OnExecutionComplete = OnExecutionComplete;
                        runner.OnTestPassed = OnTestPassed;
                        runner.OnTestSkipped = OnTestSkipped;
                        runner.OnTestFailed = OnTestFailed;
                        for (var i = 0; i < runnerOptions.iterations; i++)
                        {
                            Console.WriteLine($"Iteration {i + 1} of {runnerOptions.iterations}");
                            WaitForIdle(runner, i, "Start");
                            runner.Start(); //typeName: null, diagnosticMessages: true, parallel: false

                            var offset = DateTime.Now - startTime;
                            if (runnerOptions.durationLimit > 0 && offset.TotalSeconds > runnerOptions.durationLimit)
                            {
                                break;
                            }

                            WaitForIdle(runner, i, "Dispose");
                        }
                    }
                }
            }
            finally
            {
                Finished.WaitOne();
                Finished.Dispose();
            }

            Environment.Exit(0);
        }

        /// <summary>
        ///     Waits for Idle status. XUnit will not Start or Dispose if the state is not in an Idle status
        /// </summary>
        /// <param name="runner">The runner.</param>
        /// <param name="currentIteration">The current iteration.</param>
        /// <param name="nextStep">The next step.</param>
        private static void WaitForIdle(AssemblyRunner runner, int currentIteration, string nextStep)
        {
            var waitCount = 1;
            while (runner.Status != AssemblyRunnerStatus.Idle)
            {
                if (waitCount > 1)
                {
                    var oldColor = Console.ForegroundColor;
                    Console.ForegroundColor = ConsoleColor.Yellow;
                    Console.WriteLine($"Waiting on {runner.Status} for {nextStep}\titeration:{currentIteration + 1}\twait:{waitCount}");
                    Console.ForegroundColor = oldColor;
                }

                Thread.Sleep(300);
                if (waitCount++ <= 200) continue;

                break;
            }
        }

        #region Events

        private static void OnDiscoveryComplete(DiscoveryCompleteInfo info)
        {
            lock (ConsoleLock)
            {
                var oldColor = Console.ForegroundColor;
                if (info.TestCasesToRun == 0)
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                }

                Console.WriteLine($"Running {info.TestCasesToRun} of {info.TestCasesDiscovered} tests...");
                Console.ForegroundColor = oldColor;
            }
        }

        private static void OnExecutionComplete(ExecutionCompleteInfo info)
        {
            Finished.Set();
        }

        private static void OnTestPassed(TestPassedInfo info)
        {
            var start = DateTime.Now.AddSeconds((double) info.ExecutionTime * -1);
            var item = new ReportItem
            {
                StartTime = (start.ToUniversalTime().Ticks - 621355968000000000) / 10000000,
                Duration = info.ExecutionTime,
                TestCase = info.MethodName,
                TestSuite = info.TypeName,
                ErrorMessage = "",
                ErrorTrace = "",
                Status = "PASSED"
            };
            _recordingListener.WriteReport(item);
        }

        private static void OnTestSkipped(TestSkippedInfo info)
        {
            var item = new ReportItem
            {
                Duration = 0,
                TestCase = info.MethodName,
                TestSuite = info.TypeName,
                ErrorMessage = info.SkipReason.Trim(),
                Status = "SKIPPED"
            };
            _recordingListener.WriteReport(item);
        }


        private static void OnTestFailed(TestFailedInfo info)
        {
            var start = DateTime.Now.AddSeconds((double) info.ExecutionTime * -1);
            var item = new ReportItem
            {
                StartTime = (start.ToUniversalTime().Ticks - 621355968000000000) / 10000000,
                Duration = info.ExecutionTime,
                TestCase = info.MethodName,
                TestSuite = info.TypeName,
                ErrorMessage = info.ExceptionMessage.Trim(),
                ErrorTrace = info.ExceptionStackTrace.Trim(),
                Status = "FAILED"
            };
            _recordingListener.WriteReport(item);
        }

        #endregion
    }
}