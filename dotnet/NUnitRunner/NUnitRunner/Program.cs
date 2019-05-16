using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Xml;

using Mono.Options;
using Newtonsoft.Json;
using NUnit.Engine;


namespace NUnitRunner
{
    public class NUnitRunner
    {

        //List of Report Items
        public static List<List<ReportItem>> reportItemsList = new List<List<ReportItem>>();

        public static ITestEngine engine { get; set; }
        public static TestPackage package { get; set; }
        public static RunnerOptions opts { get; set; }
        public static RecordingListener listener { get; set; }
       
        public class RunnerOptions
        {
            public string reportFile = "report.ldjson";
            public int iterations = 0;
            public int durationLimit = 0;
            public int concurrency = 0;
            public string targetAssembly = null;
            public bool shouldShowHelp = false;
        }

        public class ReportItem
        {
            public long StartTime;
            public double Duration;
            public string TestCase;
            public string TestSuite;
            public string Status;
            public string ErrorMessage;
            public string ErrorTrace;
            public Dictionary<object, object> Extras;
        }

        public class RecordingListener : ITestEventListener
        {
			public string reportFile { get; set; }
            public StreamWriter writer;
            public List<ReportItem> reportItems;

            public ITestRunner runner { get; set; }

            public RecordingListener()
            {
                reportItems = new List<ReportItem>();
                runner = engine.GetRunner(package);
            }

            public void UpdateGlobalList()
            {
                lock (reportItemsList) { reportItemsList.Add(reportItems); }
            }

            public void OpenFile(string reportFile)
            {
                this.reportFile = reportFile;
                this.writer = new StreamWriter(reportFile)
                {
                    AutoFlush = true
                };
            }

            public void CloseFile()
            {
				writer.Close();
			}

            public void WriteReport(ReportItem item)
            {
				var sample = new Dictionary<object, object>
                {
                    { "start_time", item.StartTime },
                    { "duration", item.Duration },
                    { "test_case", item.TestCase },
					{ "test_suite", item.TestSuite },
					{ "status", item.Status },
                    { "error_msg", item.ErrorMessage },
                    { "error_trace", item.ErrorTrace },
                    { "extras", item.Extras }
				};
                string json = JsonConvert.SerializeObject(sample);
                writer.WriteLine(json);
			}

			public void OnTestEvent(string report)
            {
                try
                {
                    XmlDocument xmlDoc = new XmlDocument();
                    xmlDoc.LoadXml(report);
                    XmlNode node = xmlDoc.FirstChild;
                    if (node.Name == "test-case")
                    {
                        Console.WriteLine(report);
                        /*
                         test-case
                            id=0-1001
                            name=TestCase
                            fullname=SeleniumSuite.Test.TestCase
                            methodname=TestCase
                            classname=SeleniumSuite.Test
                            runstate=Runnable
                            seed=2077806795
                            result=Passed
                            start-time=2017-07-28 14:02:47Z
                            end-time=2017-07-28 14:02:50Z
                            duration=2.483757
                            asserts=1
                            parentId=0-1000
                        */
                        ReportItem item = new ReportItem();
                        DateTime start = DateTime.Parse(node.Attributes["start-time"].Value);
                        item.StartTime = (start.ToUniversalTime().Ticks - 621355968000000000) / 10000000;
                        item.Duration = Double.Parse(node.Attributes["duration"].Value,
                                                     NumberStyles.AllowDecimalPoint,
                                                     NumberFormatInfo.InvariantInfo);
                        item.TestCase = node.Attributes["methodname"].Value;
                        item.TestSuite = node.Attributes["classname"].Value;
                        item.ErrorMessage = "";
                        item.ErrorTrace = "";

                        //Get Properties
                        XmlNodeList propertiesNodeList = node.SelectNodes("properties");

                        Dictionary<object, object> dictionary = new Dictionary<object, object>();

                        if (propertiesNodeList.Count == 1)
                        {
                        
                            Dictionary<string, string> transactionDictionary = new Dictionary<string, string>();

                            foreach (XmlNode childNode in propertiesNodeList[0])
                            {
                                transactionDictionary.Add(childNode.Attributes["name"].Value, childNode.Attributes["value"].Value);
                            }

                            dictionary.Add("test_context", transactionDictionary);
                        }

                        item.Extras = dictionary;

                        if (node.Attributes["result"].Value == "Passed")
                        {
                            item.Status = "PASSED";
                        }
                        else if (node.Attributes["result"].Value == "Failed")
                        {
							item.Status = "FAILED";
							XmlNode failureNode = node.SelectSingleNode("failure");
                            if (failureNode != null)
                            {
                                string message = failureNode.SelectSingleNode("message").InnerText;
                                string trace = failureNode.SelectSingleNode("stack-trace").InnerText;
                                item.ErrorMessage = message.Trim();
                                item.ErrorTrace = trace.Trim();
                            }
                        }
                        else if (node.Attributes["result"].Value == "Skipped")
                        {
							item.Status = "SKIPPED";
							XmlNode reasonNode = node.SelectSingleNode("reason");
							if (reasonNode != null)
                                item.ErrorMessage = reasonNode.SelectSingleNode("message").InnerText.Trim();
						}
                        else
                            Console.WriteLine(report);

                        lock (reportItems) { reportItems.Add(item); }
                    }
                }
				catch (Exception e)
                {
                    Console.WriteLine("EXCEPTION: {0}", e.ToString());
                }
			}
        }

        public static void ShowHelp()
        {
            Console.WriteLine("NUnit runner for Taurus");
			Console.WriteLine("Usage:");
            Console.WriteLine("\t <executable> ARGS");
            Console.WriteLine("\t --iterations N - number of iterations over test suite to make");
			Console.WriteLine("\t --duration T - duration limit of test suite execution");
            Console.WriteLine("\t --concurrency N - number of concurrent users");
            Console.WriteLine("\t --report-file REPORT_FILE - filename of report file");
            Console.WriteLine("\t --target TARGET_ASSEMBLY - assembly which will be used to load tests from");
            Console.WriteLine("\t --help - show this message and exit");
            Environment.Exit(0);
		}

        public static RunnerOptions ParseOptions(string[] args)
        {
            RunnerOptions opts = new RunnerOptions();
            
			var optionSet = new OptionSet {
				{ "i|iterations=", "number of iterations over test suite to make.", (int n) => opts.iterations = n },
                { "d|duration=", "duration of test suite execution.", (int d) => opts.durationLimit = d },
                { "c|concurrency=", "number of concurrent users.", (int c) => opts.concurrency = c },
				{ "r|report-file=", "Name of report file", r => opts.reportFile = r },
                { "t|target=", "Test suite", t => opts.targetAssembly = t },
				{ "h|help", "show this message and exit", h => opts.shouldShowHelp = h != null },
			};

            optionSet.Parse(args);

			if (opts.shouldShowHelp)
				ShowHelp();

			if (opts.targetAssembly == null)
				throw new Exception("Target test suite wasn't provided. Is your file actually NUnit test DLL?");

            if (opts.iterations == 0)
				if (opts.durationLimit > 0)
                    opts.iterations = int.MaxValue;
				else
                    opts.iterations = 1;

            if (opts.concurrency == 0)
                opts.concurrency = 1;

			Console.WriteLine("Iterations: {0}", opts.iterations);
            Console.WriteLine("Hold for: {0}", opts.durationLimit);
            Console.WriteLine("Current users: {0}", opts.concurrency);
            Console.WriteLine("Report file: {0}", opts.reportFile);
            Console.WriteLine("Target: {0}", opts.targetAssembly);

			return opts;
		}

        public static void Main(string[] args)
        {
            opts = null;
            try
            {
                opts = ParseOptions(args);
            }
            catch (OptionException e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine("Try running with '--help' for more information.");
                Environment.Exit(1);
            }

            engine = TestEngineActivator.CreateInstance(true);
            package = new TestPackage(opts.targetAssembly);

            listener = new RecordingListener();

            int testCount = listener.runner.CountTestCases(TestFilter.Empty);
            if (testCount < 1)
                throw new ArgumentException("Nothing to run, no tests were loaded");

            WaitHandle[] waitHandles = new WaitHandle[opts.concurrency];

            for (int i = 0; i < opts.concurrency; i++)
            {
                var handle = new EventWaitHandle(false, EventResetMode.ManualReset);

                var thread = new Thread(()=>
                                        {
                                            RunTest();
                                            handle.Set();
                                        });
                thread.Start();

                waitHandles[i] = handle;

                Thread.Sleep(500);
            }

            WaitHandle.WaitAll(waitHandles);

            Thread.Sleep(2000);
            
            listener.OpenFile(opts.reportFile);

            foreach (var list in reportItemsList)
            {
                foreach (var item in list)
                {
                    listener.WriteReport(item);
                }
            }

            listener.CloseFile();

            Environment.Exit(0);
		}

        static void RunTest()
        {
            try
            {
                DateTime startTime = DateTime.Now;
                var threadListener = new RecordingListener();

                for (int t = 0; t < opts.iterations; t++)
                {
                    threadListener.runner.Run(threadListener, TestFilter.Empty);
                    TimeSpan offset = DateTime.Now - startTime;
                    if (opts.durationLimit > 0 && offset.TotalSeconds > opts.durationLimit)
                        break;
                }

                threadListener.UpdateGlobalList();
            }
            catch (Exception e)
            {
                Console.WriteLine("EXCEPTION: {0}", e.ToString()); ;
            }
        }
    }
}
