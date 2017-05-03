
import java.util.concurrent.TimeUnit;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;

public class selenium1 {

     @BeforeClass
     public static void openBrowser(){
     }

     @Test
     public void valid_UserCredential(){
        Assert.assertEquals(1, 1);
	 }

}