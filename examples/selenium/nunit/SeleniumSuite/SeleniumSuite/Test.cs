using NUnit.Framework;
using OpenQA.Selenium;
using OpenQA.Selenium.Chrome;


namespace SeleniumSuite
{
    [TestFixture]
    public class Test
    {
        private IWebDriver _driver;

        [SetUp]
        public void Initialize()
        {
            _driver = new ChromeDriver();
        }

        [Test]
        public void IndexPage()
        {
            _driver.Navigate().GoToUrl("http://blazedemo.com");
            Assert.AreEqual(_driver.Title, "BlazeDemo");
        }

        [Test]
        public void ReservePage()
        {
            _driver.Navigate().GoToUrl("http://blazedemo.com/reserve.php");
            Assert.AreEqual(_driver.Title, "BlazeDemo - reserve");
        }

        [TearDown]
        public void AfterTest()
        {
            _driver?.Close();
        }
    }
}