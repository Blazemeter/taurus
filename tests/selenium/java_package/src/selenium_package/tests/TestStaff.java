package selenium_package.tests;

import java.util.regex.Pattern;
import java.util.concurrent.TimeUnit;

import org.junit.*;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import org.openqa.selenium.*;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.support.ui.Select;

public class TestStaff {
	
	private WebDriver driver;
	private String baseUrl;
	private boolean acceptNextAlert = true;
	private StringBuffer verificationErrors = new StringBuffer();

	@Before
	public void setUp() throws Exception {
	    driver = new FirefoxDriver();
	    baseUrl = "http://demo.blazemeter.com/";
	    driver.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
	  }

	  @Test
	  public void testPricingDisplayed() throws Exception {
	    driver.get(baseUrl + "index.html");
	    assertTrue(driver.findElement(By.linkText("Pricing")).isDisplayed());
	  }
  
	  @Test
	  public void testAboutOpened() throws Exception {
		driver.get(baseUrl + "index.html");
		driver.findElement(By.linkText("About")).click();
		assertTrue(isElementPresent(By.id("page-title")));
	  }
  
	  @Test
	  public void testDivPresent() throws Exception{
		  driver.get(baseUrl + "index.html");
		  assertTrue(isElementPresent(By.cssSelector("div.main-holder")));
	  }
		  
	  @Test
	  public void testAlertPresent() throws Exception{
		  driver.get(baseUrl + "index.html");
		  assertFalse(isAlertPresent());
	  }
	  
	  @Test
	  public void testNotFoundNoAlert() throws Exception{
		  driver.get(baseUrl + "not_found");
		  assertFalse(isAlertPresent());
	  }
		  
	  @Test
	  public void testFailedElementPresent() throws Exception{
		  driver.get(baseUrl + "not_found");
		  assertTrue(isElementPresent(By.id("page-title")));
	  }  
  
	  @After
	  public void tearDown() throws Exception {
	    driver.quit();
	    String verificationErrorString = verificationErrors.toString();
	    if (!"".equals(verificationErrorString)) {
	      fail(verificationErrorString);
	    }
	  }

  private boolean isElementPresent(By by) {
    try {
      driver.findElement(by);
      return true;
    } catch (NoSuchElementException e) {
      return false;
    }
  }

  private boolean isAlertPresent() {
    try {
      driver.switchTo().alert();
      return true;
    } catch (NoAlertPresentException e) {
      return false;
    }
  }

  private String closeAlertAndGetItsText() {
    try {
      Alert alert = driver.switchTo().alert();
      String alertText = alert.getText();
      if (acceptNextAlert) {
        alert.accept();
      } else {
        alert.dismiss();
      }
      return alertText;
    } finally {
      acceptNextAlert = true;
    }
  }
	
}
