package com.company;

import org.junit.Test;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.*;

public class JUnitTaurusDemo {
    @Test
    public void startDemo() {
        
        // Set WebDriver Path (Optional)
        //System.setProperty("webdriver.chrome.driver", "/Users/.../chromedriver");

        ChromeDriver driver = new ChromeDriver();
        
        try {
            driver.get("http://www.blazedemo.com");

            driver.findElement(By.xpath("//select[@name='fromPort']/option[text()='Boston']")).click();
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)

            driver.findElement(By.xpath("//select[@name='toPort']/option[text()='New York']")).click();
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)

            driver.findElement(By.className("btn-primary")).click();
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)

            driver.findElement(By.className("btn-small")).click();
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)

            driver.findElement(By.id("inputName")).sendKeys("Blaze Meter");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("address")).sendKeys("3965 Freedom Cir");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("city")).sendKeys("Santa Clara");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("state")).sendKeys("CA");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("zipCode")).sendKeys("95054");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.xpath("//select[@name='cardType']/option[text()='American Express']")).click();
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("creditCardNumber")).sendKeys("00001111222233334444");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("creditCardMonth")).clear();
            driver.findElement(By.id("creditCardMonth")).sendKeys("01");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("creditCardYear")).clear();
            driver.findElement(By.id("creditCardYear")).sendKeys("2020");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)
            driver.findElement(By.id("nameOnCard")).sendKeys("Blaze Meter");
            Thread.sleep(1000);  // Pause in order to see test in action (Optional)

            driver.findElement(By.className("btn-primary")).click();


            Thread.sleep(5000);
        
        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {
            driver.quit();
        }
    }
}