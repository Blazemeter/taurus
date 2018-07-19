using System;
using System.Collections.Generic;
using System.IO;
using Newtonsoft.Json;

namespace XUnitRunner
{
    public class RecordingListener : IDisposable
    {
        public StreamWriter Writer;

        public RecordingListener(string reportFile)
        {
            ReportFile = reportFile;
            Writer = new StreamWriter(reportFile)
            {
                AutoFlush = true
            };
        }

        public string ReportFile { get; set; }

        public void Dispose()
        {
            CloseFile();
        }

        public void CloseFile()
        {
            Writer?.Close();
        }

        public void WriteReport(ReportItem item)
        {
            var sample = new Dictionary<object, object>
            {
                {"start_time", item.StartTime},
                {"duration", item.Duration},
                {"test_case", item.TestCase},
                {"test_suite", item.TestSuite},
                {"status", item.Status},
                {"error_msg", item.ErrorMessage},
                {"error_trace", item.ErrorTrace},
                {"extras", item.Extras}
            };
            var json = JsonConvert.SerializeObject(sample);
            Writer.WriteLine(json);
        }
    }
}