using System;
using DotnetTestRunner.Models;
using Xunit.Runners;

namespace DotnetTestRunner.Services.xUnit
{
    public class XUnitTestEventListener
    {
        private readonly ReportWriter _reportWriter;
        private readonly string _threadName;

        public XUnitTestEventListener(ReportWriter reportWriter, string threadName)
        {
            _reportWriter = reportWriter;
            _threadName = threadName;
        }

        public static void OnDiscoveryComplete(DiscoveryCompleteInfo info)
        {
            if (info.TestCasesToRun == 0)
            {
                throw new ArgumentException("Nothing to run, no tests were loaded");
            }
        }

        public void OnTestFinished(TestFinishedInfo info)
        {
            var item = new ReportItem
            {
                ThreadName = _threadName,
                Duration = (double) info.ExecutionTime,
                TestCase = info.MethodName,
                TestSuite = info.TypeName,
                ErrorMessage = "",
                ErrorTrace = "",
                Status = "PASSED"
            };

            _reportWriter.AddItemToReport(item);
        }

        public void OnTestSkipped(TestSkippedInfo info)
        {
            var item = new ReportItem
            {
                ThreadName = _threadName,
                Duration = 0,
                TestCase = info.MethodName,
                TestSuite = info.TypeName,
                ErrorMessage = info.SkipReason,
                ErrorTrace = "",
                Status = "SKIPPED"
            };

            _reportWriter.AddItemToReport(item);
        }

        public void OnTestFailed(TestFailedInfo info)
        {
            var item = new ReportItem
            {
                ThreadName = _threadName,
                Duration = (double) info.ExecutionTime,
                TestCase = info.MethodName,
                TestSuite = info.TypeName,
                ErrorMessage = info.ExceptionMessage,
                ErrorTrace = info.ExceptionStackTrace,
                Status = "FAILED"
            };

            _reportWriter.AddItemToReport(item);
        }
    }
}
