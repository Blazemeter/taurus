using System.Collections.Generic;

namespace XUnitRunner
{
    public class ReportItem
    {
        public decimal Duration;
        public string ErrorMessage;
        public string ErrorTrace;
        public Dictionary<object, object> Extras;
        public long StartTime;
        public string Status;
        public string TestCase;
        public string TestSuite;
    }
}