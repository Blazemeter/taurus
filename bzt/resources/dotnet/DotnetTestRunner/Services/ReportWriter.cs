using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using DotnetTestRunner.Models;
using Newtonsoft.Json;

namespace DotnetTestRunner.Services
{
    public class ReportWriter
    {
        private readonly string _reportFile;
        private readonly ConcurrentQueue<ReportItem> _reportItems;
        private readonly Task _writingTask;
        private bool _stopWriting;

        public ReportWriter(string reportFile)
        {
            _reportFile = reportFile;
            _reportItems = new ConcurrentQueue<ReportItem>();
            _stopWriting = false;
            _writingTask = Task.Run(WriteReport);
        }

        public void AddItemToReport(ReportItem item)
        {
            if (!_stopWriting)
            {
                _reportItems.Enqueue(item);
            }
        }

        public void StopWriting()
        {
            _stopWriting = true;
            _writingTask.GetAwaiter().GetResult();
        }

        public async Task StopWritingAsync()
        {
            _stopWriting = true;
            await _writingTask;
        }

        private async Task WriteReport()
        {
            await using var streamWriter = new StreamWriter(_reportFile) {AutoFlush = true};

            while (!_stopWriting || _reportItems.Count > 0)
            {
                if (_reportItems.TryDequeue(out var item))
                {
                    var reportLine = GetReportLine(item);
                    await streamWriter.WriteLineAsync(reportLine);
                }
            }
        }

        private static string GetReportLine(ReportItem item)
        {
            var sample = new Dictionary<string, object>
            {
                {"start_time", item.StartTime},
                {"workerID", item.ThreadName},
                {"duration", item.Duration},
                {"test_case", item.TestCase},
                {"test_suite", item.TestSuite},
                {"status", item.Status},
                {"error_msg", item.ErrorMessage},
                {"error_trace", item.ErrorTrace},
                {"extras", item.Extras}
            };

            return JsonConvert.SerializeObject(sample);
        }
    }
}
