using NUnit.Framework;
using OpenQA.Selenium;
using OpenQA.Selenium.Chrome;


namespace SeleniumSuite
{
    [TestFixture()]
    public class Test
    {
		IWebDriver driver = null;

		[SetUp]
		public void Initialize()
		{
			ChromeDriverService service = ChromeDriverService.CreateDefaultService(@"/home/dmand/.bzt/selenium-taurus/tools/chromedriver/2.30");
			ChromeOptions options = new ChromeOptions();
			options.BinaryLocation = @"/usr/bin/google-chrome";
			driver = new ChromeDriver(service, options);
		}

		[Test()]
		public void TestCase()
		{
			driver.Navigate().GoToUrl("http://blazedemo.com");
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
