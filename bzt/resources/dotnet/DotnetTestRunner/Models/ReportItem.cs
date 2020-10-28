using System.Collections.Generic;

namespace DotnetTestRunner.Models
{
    public class ReportItem
    {
        public long StartTime { get; set; }
        public double Duration { get; set; }
        public string ThreadName { get; set; }
        public string TestCase { get; set; }
        public string TestSuite { get; set; }
        public string Status { get; set; }
        public string ErrorMessage { get; set; }
        public string ErrorTrace { get; set; }
        public Dictionary<object, object> Extras { get; set; }
    }
}
