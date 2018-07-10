using System;
using System.Collections.Generic;

namespace XUnitRunner
{
    public class ReportItem
    {
        public long StartTime;
        public decimal Duration;
        public string TestCase;
        public string TestSuite;
        public string Status;
        public string ErrorMessage;
        public string ErrorTrace;
        public Dictionary<object, object> Extras;
    }

}
