import junit.framework.TestCase;
import org.openqa.selenium.*;
import org.openqa.selenium.remote.*;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.junit.Assert;

public class SimpleTest extends TestCase {
        private WebDriver driver;
        WebElement element;

        public void setUp() throws Exception {
            DesiredCapabilities capabilities = DesiredCapabilities.firefox();
            capabilities.setCapability("version", "11");
            capabilities.setCapability("platform", Platform.WINDOWS);
            capabilities.setCapability("name", "Testing Selenium 2");

            this.driver = new FirefoxDriver();
            driver.manage().timeouts().implicitlyWait(30, TimeUnit.SECONDS);
        }

        public void testSimple() throws Exception {
            this.driver.get("http://www.google.com");
            assertEquals("Google", this.driver.getTitle());
        }

        public void testInValid_UserCredential()
     {
		 System.out.println("Starting test " + new Object(){}.getClass().getEnclosingMethod().getName());
	     driver.get("http://www.store.demoqa.com");
	     driver.findElement(By.xpath(".//*[@id='account']/a")).click();
	     driver.findElement(By.id("log")).sendKeys("testuser");
	     driver.findElement(By.id("pwd")).sendKeys("Test@123");
	     driver.findElement(By.id("login")).click();
	     try{
			element = driver.findElement (By.xpath(".//*[@id='account_logout']/a"));
	     }catch (Exception e){
			}
	     Assert.assertNotNull(element);
	     System.out.println("Ending test " + new Object(){}.getClass().getEnclosingMethod().getName());
     }

        public void tearDown() throws Exception {
            this.driver.quit();
        }
    }