using NUnit.Engine;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Globalization;
using System.Threading;
using System.Xml;

namespace NUnitRunner.Models
{
    public class TestEventListener : ITestEventListener
    {
        public ITestRunner Runner { get; }
        private readonly ConcurrentQueue<ReportItem> _reportItems;
        private readonly string _threadName;

        public TestEventListener(
            ITestEngine engine,
            TestPackage package,
            ConcurrentQueue<ReportItem> reportItems,
            string threadName
        )
        {
            _reportItems = reportItems;
            Runner = engine.GetRunner(package);
            _threadName = threadName;
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

                    switch (node.Attributes["result"].Value)
                    {
                        case "Passed":
                            item.Status = "PASSED";
                            break;

                        case "Failed":
                            item.Status = "FAILED";
                            XmlNode failureNode = node.SelectSingleNode("failure");
                            if (failureNode != null)
                            {
                                string message = failureNode.SelectSingleNode("message").InnerText;
                                string trace = failureNode.SelectSingleNode("stack-trace").InnerText;
                                item.ErrorMessage = message.Trim();
                                item.ErrorTrace = trace.Trim();
                            }

                            break;

                        case "Skipped":
                            item.Status = "SKIPPED";
                            XmlNode reasonNode = node.SelectSingleNode("reason");
                            if (reasonNode != null)
                                item.ErrorMessage = reasonNode.SelectSingleNode("message").InnerText.Trim();
                            break;

                        default:
                            Console.WriteLine(report);
                            break;
                    }

                    item.ThreadName = _threadName;

                    _reportItems.Enqueue(item);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("EXCEPTION: {0}", e.ToString());
            }
        }
    }
}
