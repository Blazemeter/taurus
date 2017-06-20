import org.junit.Test;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.*;

public class WDJavaExample {
    @Test
    public void startTest() {
        
        // Set WebDriver Path (Optional)
        //System.setProperty("webdriver.chrome.driver", "/Users/.../chromedriver");

        WebDriver driver = new ChromeDriver();
        
        try {
            driver.get("http://www.google.com");
            Thread.sleep(1500);  // Pause in order to see test in action (Optional)

            driver.findElement(By.name("q")).sendKeys("BlazeMeter");
            driver.findElement(By.name("q")).submit();
            Thread.sleep(1500);  // Pause in order to see test in action (Optional)
        
        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {
            driver.quit();
        }
    }
}