package selenium_package.tests.main_page;


import java.util.regex.Pattern;
import java.util.concurrent.TimeUnit;

import org.junit.*;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import org.openqa.selenium.*;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.support.ui.Select;


public class TestMainPage {
	private String baseUrl;
	private boolean acceptNextAlert = true;
	private StringBuffer verificationErrors = new StringBuffer();

	@Before
	public void setUp() throws Exception {
	    baseUrl = "http://demo.blazemeter.com/";
	  }

	@Test
	public void testMainPagePricingDisplayed() throws Exception {
	  }
	@Test
	public void testMainPageElementFail() throws Exception {
	  }
	  
	@After
	public void tearDown() throws Exception {
	    String verificationErrorString = verificationErrors.toString();
	    if (!"".equals(verificationErrorString)) {
	      fail(verificationErrorString);
	    }
	  }

  private boolean isElementPresent(By by) {
    try {
      return true;
    } catch (NoSuchElementException e) {
      return false;
    }
  }

  private boolean isAlertPresent() {
    try {
      return true;
    } catch (NoAlertPresentException e) {
      return false;
    }
  }

  private String closeAlertAndGetItsText() {
    try {
      if (acceptNextAlert) {
      } else {
      }
      return "";
    } finally {
      acceptNextAlert = true;
    }
  }
	
}
