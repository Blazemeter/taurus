package example;

import java.io.File;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeDriverService;
import org.openqa.selenium.chrome.ChromeOptions;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class TestNG {

    private WebDriver driver;

    @BeforeClass
    public void setUp() {
        System.setProperty("webdriver.chrome.driver", "/root/.bzt/selenium-taurus/tools/drivers/chromedriver/138.0.7204.168/chromedriver");
        ChromeOptions options = new ChromeOptions();
        options.setBinary("/usr/bin/google-chrome");
        options.addArguments("--headless=chrome");
        options.addArguments("--no-sandbox");
        options.addArguments("--disable-dev-shm-usage");

        ChromeDriverService service = new ChromeDriverService.Builder()
            .usingDriverExecutable(new File(System.getProperty("webdriver.chrome.driver")))
            .withLogFile(new File("/tmp/artifacts/chromedriver.log"))
            .build();

        driver = new ChromeDriver(service, options);
    }

    @Test
    public void testOpenUrl() {
        driver.get("https://www.blazedemo.com");
    }

    @AfterClass
    public void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }
}
