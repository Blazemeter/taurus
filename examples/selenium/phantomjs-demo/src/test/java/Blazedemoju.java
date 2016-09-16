import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.phantomjs.PhantomJSDriver;
import org.openqa.selenium.phantomjs.PhantomJSDriverService;
import org.openqa.selenium.remote.DesiredCapabilities;

import java.util.concurrent.TimeUnit;

public class Blazedemoju {
    private PhantomJSDriver wd;

    @Before
    public void setUp() throws Exception {
        DesiredCapabilities caps = new DesiredCapabilities();
        //caps.setCapability(PhantomJSDriverService.PHANTOMJS_EXECUTABLE_PATH_PROPERTY, "phantomjs");
        wd = new PhantomJSDriver(caps);
        wd.manage().timeouts().implicitlyWait(60, TimeUnit.SECONDS);
    }

    @Test
    public void test() throws Exception {
        wd.get("http://blazedemo.com/");
        if (!wd.findElement(By.xpath("//div[3]/form/select[1]//option[3]")).isSelected()) {
            wd.findElement(By.xpath("//div[3]/form/select[1]//option[3]")).click();
        }
        if (!wd.findElement(By.xpath("//div[3]/form/select[2]//option[6]")).isSelected()) {
            wd.findElement(By.xpath("//div[3]/form/select[2]//option[6]")).click();
        }
        wd.findElement(By.cssSelector("input.btn.btn-primary")).click();

        Thread.sleep(100);
        wd.findElement(By.xpath("//table[@class='table']/tbody/tr[4]/td[1]/input")).click();

        Thread.sleep(200);
        wd.findElement(By.cssSelector("input.btn.btn-primary")).click();
        
        Thread.sleep(500);
    }

    @After
    public void tearDown() {
        wd.quit();
    }
}
