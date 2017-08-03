package tests;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Parameters;
import org.testng.annotations.Test;

public class TestNGExample {
    private WebDriver driver;

    @BeforeMethod
    public void setUp() {
        this.driver = new FirefoxDriver();
        this.driver.manage().window().maximize();
    }

    @AfterMethod
    public void tearDown() {
        this.driver.quit();
    }

    @Test
    public void blazedemoTitleIsRight() {
        this.driver.get("http://blazedemo.com/");
        Assert.assertEquals(this.driver.getTitle(), "BlazeDemo");
    }

}
