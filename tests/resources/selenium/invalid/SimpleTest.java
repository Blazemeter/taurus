import junit.framework.TestCase;
import org.openqa.selenium.*;
import org.openqa.selenium.remote.*;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.junit.Assert;

public class SimpleTest extends TestCase {
        WebElement element;

        public void setUp() throws Exception {
            DesiredCapabilities capabilities = DesiredCapabilities.firefox();
            capabilities.setCapability("version", "11");
            capabilities.setCapability("platform", Platform.WINDOWS);
            capabilities.setCapability("name", "Testing Selenium 2");

        }

        public void testSimple() throws Exception {
        }

        public void testInValid_UserCredential()
     {
     }

        public void tearDown() throws Exception {
        }
    }