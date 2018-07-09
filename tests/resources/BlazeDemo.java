import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openqa.selenium.firefox.FirefoxDriver;

import java.util.concurrent.TimeUnit;
import org.openqa.selenium.By;

import static org.openqa.selenium.OutputType.*;

public class BlazeDemo {
    FirefoxDriver wd;

    @Before
    public void setUp() throws Exception {
        wd = new FirefoxDriver();
        wd.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
    }

    @Test
    public void BlazeDemo() {
        wd.get("http://localhost:8000/BlazeDemo.html");
        wd.findElement(By.cssSelector("input.btn.btn-primary")).click();
    }

    @Test
    public void ErrorWithProps() {
        throw new RuntimeException(System.getProperty("settprop") + System.getProperty("execprop") + System.getProperty("scenprop"));
    }

    @After
    public void tearDown() {
        wd.quit();
    }
}
