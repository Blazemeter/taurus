using System;
using NUnit.Framework;
using OpenQA.Selenium;
using OpenQA.Selenium.Chrome;
using OpenQA.Selenium.Firefox;


namespace SeleniumSuite
{
    [TestFixture()]
    public class Test
    {
		IWebDriver driver = null;

		[SetUp]
		public void Initialize()
		{
			driver = new ChromeDriver();
		}

		[Test()]
		public void PassingTest()
		{
			driver.Navigate().GoToUrl("http://blazedemo.com");
			Assert.AreEqual(driver.Title, "BlazeDemo");
		}

		[Test()]
		public void FailingTest()
		{
			driver.Navigate().GoToUrl("http://blazedemo.com/reserve.php");
			Assert.AreEqual(driver.Title, "BlazeDemo");
		}

	    [TearDown]
		public void AfterTest()
		{
			if (this.driver != null)
				driver.Close();
		}
	}
}
