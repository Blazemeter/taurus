package dir;

import org.junit.After;
import org.junit.Before;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

import java.util.concurrent.TimeUnit;
import java.util.Date;
import java.io.File;

import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.*;

import static org.openqa.selenium.OutputType.*;

public class Blazedemoju extends Base {
    FirefoxDriver wd;

    @Before
    public void setUp() throws Exception {
        wd = new FirefoxDriver();
        wd.manage().timeouts().implicitlyWait(60, TimeUnit.SECONDS);
    }

    public void testMethod() {
        wd.get("http://blazedemo.com/");
        if (!wd.findElement(By.xpath("//div[3]/form/select[1]//option[3]")).isSelected()) {
            wd.findElement(By.xpath("//div[3]/form/select[1]//option[3]")).click();
        }
        if (!wd.findElement(By.xpath("//div[3]/form/select[2]//option[6]")).isSelected()) {
            wd.findElement(By.xpath("//div[3]/form/select[2]//option[6]")).click();
        }
        wd.findElement(By.cssSelector("input.btn.btn-primary")).click();
        wd.findElement(By.xpath("//table[@class='table']/tbody/tr[4]/td[1]/input")).click();
        wd.findElement(By.cssSelector("input.btn.btn-primary")).click();
    }

    @After
    public void tearDown() {
        if (wd!=null) {
            wd.quit();
        }
    }

    public static boolean isAlertPresent(FirefoxDriver wd) {
        try {
            wd.switchTo().alert();
            return true;
        } catch (NoAlertPresentException e) {
            return false;
        }
    }
}
