using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using DotnetTestRunner.Models;
using NUnit.Engine;

namespace DotnetTestRunner.Services.NUnit
{
    public class NUnitTestEventListener : ITestEventListener
    {
        private const long EpochTicks = 621355968000000000; // ticks from 1.1.0000 to 1.1.1970
        private const long NanosecondsInSecond = 10000000;

        private readonly ReportWriter _reportWriter;
        private readonly string _threadName;

        public NUnitTestEventListener(ReportWriter reportWriter, string threadName)
        {
            _reportWriter = reportWriter;
            _threadName = threadName;
        }

        public void OnTestEvent(string report)
        {
            try
            {
                var xmlDoc = new XmlDocument();
                xmlDoc.LoadXml(report);
                var node = xmlDoc.FirstChild;
                if (node.Name != "test-case")
                {
                    return;
                }

                var start = DateTime.Parse(node.Attributes["start-time"].Value);
                var item = new ReportItem
                {
                    ThreadName = _threadName,
                    StartTime = (start.ToUniversalTime().Ticks - EpochTicks) / NanosecondsInSecond,
                    Duration = Double.Parse(node.Attributes["duration"].Value,
                        NumberStyles.AllowDecimalPoint,
                        NumberFormatInfo.InvariantInfo),
                    TestCase = node.Attributes["methodname"].Value,
                    TestSuite = node.Attributes["classname"].Value,
                    ErrorMessage = "",
                    ErrorTrace = "",
                    Extras = GetXmlNodeProperties(node)
                };

                switch (node.Attributes["result"].Value)
                {
                    case "Passed":
                        item.Status = "PASSED";
                        break;

                    case "Failed":
                        item.Status = "FAILED";
                        var failureNode = node.SelectSingleNode("failure");
                        if (failureNode != null)
                        {
                            item.ErrorMessage = failureNode.SelectSingleNode("message").InnerText.Trim();
                            item.ErrorTrace = failureNode.SelectSingleNode("stack-trace").InnerText.Trim();
                        }

                        break;

                    case "Skipped":
                        item.Status = "SKIPPED";
                        var reasonNode = node.SelectSingleNode("reason");
                        if (reasonNode != null)
                        {
                            item.ErrorMessage = reasonNode.SelectSingleNode("message").InnerText.Trim();
                        }

                        break;
                }

                _reportWriter.AddItemToReport(item);
            }
            catch (Exception e)
            {
                Console.WriteLine("EXCEPTION: {0}", e);
            }
        }

        private Dictionary<object, object> GetXmlNodeProperties(XmlNode node)
        {
            var propertiesNodeList = node.SelectNodes("properties");
            var properties = new Dictionary<object, object>();

            if (propertiesNodeList.Count == 1)
            {
                var testContext = new Dictionary<string, string>();
                foreach (XmlNode childNode in propertiesNodeList[0])
                {
                    testContext.Add(childNode.Attributes["name"].Value, childNode.Attributes["value"].Value);
                }

                properties.Add("test_context", testContext);
            }

            return properties;
        }
    }
}
