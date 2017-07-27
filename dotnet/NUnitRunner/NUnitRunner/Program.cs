using System;
using System.Xml;

using Mono.Options;

using NUnit.Engine;
using NUnit.Engine.Extensibility;


namespace NUnitRunner
{
    public class NUnitRunner : ITestEventListener
    {
		public void OnTestEvent(string report)
        {
            Console.WriteLine(report);
		}

		public static void Main(string[] args)
        {

			var shouldShowHelp = false;
            int iterations = 0;
            float holdFor = 0;
            string reportFile = "report.ldjson";
            string testAssembly = null;

            // thses are the available options, not that they set the variables
            var options = new OptionSet {
                { "i|iterations=", "number of iterations over test suite to make.", (int n) => iterations = n },
                { "d|duration=", "duration of test suite execution.", (float h) => holdFor = h },
                { "r|report-file=", "Name of report file", r => reportFile = r },
                { "t|target=", "Test suite", t => testAssembly = t },
                { "h|help", "show this message and exit", h => shouldShowHelp = h != null },
            };

			try
			{
				options.Parse(args);
			}
			catch (OptionException e)
			{
				Console.WriteLine(e.Message);
				Console.WriteLine("Try running with '--help' for more information.");
				return;
			}

            Console.WriteLine("Iterations: {0}", iterations);
            Console.WriteLine("Hold for: {0}", holdFor);
            Console.WriteLine("Report file: {0}", reportFile);
            Console.WriteLine("Target: {0}", testAssembly);

            // TODO: print help if `shouldShowHelp`

            if (testAssembly == null)
                throw new Exception("Target test suite wasn't provided");

            if (iterations == 0)
                if (holdFor > 0)
                    iterations = int.MaxValue;
                else
                    iterations = 1;

			NUnitRunner listener = new NUnitRunner();

			ITestEngine engine = TestEngineActivator.CreateInstance();
			TestPackage package = new TestPackage(testAssembly);
			ITestRunner runner = engine.GetRunner(package);

            DateTime startTime = DateTime.Now;
            for (int i = 0; i < iterations; i++)
            {
                runner.Run(listener, TestFilter.Empty);
                TimeSpan offset = DateTime.Now - startTime;
                if (holdFor > 0 && offset.TotalSeconds > holdFor)
                    break;
            }

			Console.WriteLine("Hello World!");
        }
    }
}
