using NUnit.Framework;
using System;


namespace NUnitRunner
{
    [TestFixture()]
    public class NUnitTestClass
    {
        [Test()]
        public void TestCase()
        {
            NUnitRunner.Main(new [] { "--iterations", "1", "--target", "../../NUnitSelenium/SeleniumSuite/bin/Release/SeleniumSuite.dll"});
        }
    }
}