# Creating JMeter and Selenium Scripts with Taurus

_Author: Iurii Bushnev_

The wide range of [performance testing tools](https://www.blazemeter.com/blog/open-source-load-testing-tools-which-one-should-you-use?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) available enables companies to find a solution that can meet any expectation and budget. However, many testing tools are limited when it comes to automating and integrating them into the [Continuous Integration](http://info.blazemeter.com/automated-performance-tests-in-jenkins-ci-environments?utm_source=BM&utm_medium=resources&utm_campaign=webinar-automated-performance-tests-jenkins-ci-environments) cycle, and using them to manage the functionality workflow might also be complicated. For example, when invoking workflow management APIs that don’t alert when the workflow is finished.

Many of you are probably using, [Selenium](https://www.blazemeter.com/blog/how-automate-testing-using-selenium-webdriver-jenkins-and-allure?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) for this purpose. This might be an even more attractive idea if you already cover functional testing for your application with Selenium scripts. But getting reasonable performance metrics from Selenium might be tricky.

[Taurus](/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) is an open-source framework that helps manage the above-mentioned issues, and more. Briefly, Taurus is a tool that provides a simple way to create and run performance tests, as well as an easy integration with additional open-source functional and performance testing software, like Selenium, Gatling or JMeter. Taurus uses YAML files, which are very easy to understand.

In this article we will go over the steps to create basic functional and performance tests for a simple web application, by using Selenium, [JMeter](https://www.blazemeter.com/jmeter-load-testing?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) and Taurus. In following articles we will also go over how to analyze test results and how to configure [Jenkins](https://www.blazemeter.com/jenkins?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) to establish a basic Continuous Integration environment for tests orchestrated by Taurus.

## Selenium Scripts

Selenium is one of the best solutions for functional testing web applications. Since the creation of Selenium scripts is not the main topic of this article, we will not go into a detailed [Selenium tutorial](http://toolsqa.com/selenium-webdriver/selenium-introduction/), but rather to the final functional scripts. 

The typical Selenium project uses the Page Object pattern and its structure looks like this:

![Java project layout](java-project-idea.png)

The best practice for the Page Object pattern is when a function that logically leads to another page returns this page as an object:

```java
// HomePage.java
package pages;

import framework.BasePage;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.ui.Select;

public class HomePage extends BasePage {
    @FindBy(xpath = DESTINATION_OF_THE_WEEK_LINK_XPATH)
    private WebElement destinationOfTheWeekLink;

    @FindBy(xpath = FROM_SELECT_ELEMENT_XPATH)
    private WebElement fromSelectElement;

    @FindBy(xpath = TO_SELECT_ELEMENT_XPATH)
    private WebElement toSelectElement;

    @FindBy(xpath = FIND_FLIGHTS_BUTTON_XPATH)
    private WebElement findFlightsButton;

    private final static String FIND_FLIGHTS_BUTTON_XPATH = "//input[@value='Find Flights']";
    private final static String FROM_SELECT_ELEMENT_XPATH = "//select[@name='fromPort']";
    private final static String TO_SELECT_ELEMENT_XPATH = "//select[@name='toPort']";
    private final static String DESTINATION_OF_THE_WEEK_LINK_XPATH = "//a[@href='vacation.html']";

    public DestinationOfWeekPage checkoutDestinationOfTheWeek() {
        destinationOfTheWeekLink.click();
        return initPage(DestinationOfWeekPage.class);
    }

    public FlightsPage findFLights(String from, String to){
        Select selectFromElement = new Select(fromSelectElement);
        Select selectToElement = new Select(toSelectElement);

        selectFromElement.selectByValue(from);
        selectToElement.selectByValue(to);

        findFlightsButton.click();
        return initPage(FlightsPage.class);
    }
}
```

In this case, Selenium tests are very readable and look complete without additional and redundant instantiations of pages:

```java
//FlightsSearch.java
import framework.BasePage;
import framework.BaseTest;
import org.junit.Assert;
import org.junit.Test;
import pages.FlightsPage;
import pages.HomePage;

public class FlightsSearchTests extends BaseTest {
    @Test
    public void verifyThatWeCanFindFlightsFromParisToRome() {
        HomePage homePage = BasePage.initPage(HomePage.class);
        FlightsPage flightsPage = homePage.findFLights("Paris", "Rome");

        Assert.assertTrue(flightsPage.getFlightsNumbers().size() > 0);
    }
}
```

## JMeter Scripts

[JMeter](http://jmeter.apache.org/), designed to load test functional behavior and measure website performance, is one of the most popular open-source performance testing software tools. The simplest and most convenient way to create JMeter scripts from scratch is to use the [BlazeMeter Proxy Recorder](https://guide.blazemeter.com/hc/en-us/articles/207420545-BlazeMeter-Proxy-Recorder-Mobile-and-web-?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium). The recorder enables easy recording and creation of JMeter test scenarios for your performance tests. 

After your script is successfully recorded with the Proxy Recorder, save the *.jmx result file and put it into your project.

![JMeter files view](jmeter-files.png)


## Taurus

Taurus is a free and open-source framework under the Apache 2.0 License. Taurus extends the capabilities of popular functional and performance testing frameworks like JMeter, [Gatling](http://gatling.io/) or Selenium. It is also user-friendly for configuration, running and analyzing [test results](/docs/Reporting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium).

The simplest script looks like this (TaurusScriptExample.yml):

```yaml
execution:
- concurrency: 100
  ramp-up: 1m
  hold-for: 1m30s
  scenario: simple

scenarios:
  simple:
    think-time: 0.75
    requests:
    - http://blazedemo.com/
```

As you can see, the Taurus [YAML](/docs/YAMLTutorial/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) test file is very easy to use. By using just a few human readable lines, we can start our first test.

We just need to [install](/install/Installation/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) Taurus, and after that we can run this test with simple console command:

![console3.png](console3.png)

But back to our existing JMeter and Selenium tests. Taurus lets you run existing *.jmx JMeter files and Selenium *.java tests files using the JUnit executor. You can find detailed instructions for supported functionality for the [JMeter executor](/docs/JMeter/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) and the [Selenium executor](/docs/Selenium/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium). 

Based on these instructions and our implemented scripts, we can run the tests using the YAML file by specifying the sections (assuming that we are going to run our Taurus script from the root of the project):


```yaml
- concurrency: 20
  ramp-up: 60m
  hold-for: 60m
  scenario: purchase-flight-jmeter-test

- concurrency: 30
  ramp-up: 30m
  hold-for: 90m
  scenario: find-lots-of-flights-jmeter-test

- concurrency: 25
  ramp-up: 40m
  hold-for: 80m
  scenario: find-simple-flight-jmeter-test

- concurrency: 15
  ramp-up: 120m
  hold-for: 10m
  scenario: verify-week-destination-jmeter-test

scenarios:
  purchase-flight-jmeter-test:
      script: ../jmeter/FlightPurchaseTest.jmx
  find-lots-of-flights-jmeter-test:
      script: ../jmeter/FindLotsOfFlightsTest.jmx
  find-simple-flight-jmeter-test:
      script: ../jmeter/FindSimpleFlightTest.jmx
  verify-week-destination-jmeter-test:
      script: ../jmeter/VerifyWeekDestinationTest.jmx
```

We can do the same to run Selenium scripts on Taurus with the Selenium executor. Note that if the Selenium test script has a dependency on a class outside of the test section, we need to specify the path to the compiled project jar file:

```yaml
execution:
- executor: selenium
  ramp-up: 40m
  hold-for: 60m
  scenario: week-destination-selenium-test

- executor: selenium
  ramp-up: 30m
  hold-for: 70m
  scenario: flights-search-selenium-test

- executor: selenium
  ramp-up: 20m
  hold-for: 80m
  scenario: flight-information-selenium-test

- executor: selenium
  ramp-up:  30m
  hold-for: 70m
  scenario: flights-information-selenium-test

- executor: selenium
  ramp-up: 20m
  hold-for: 80m
  scenario: purchase-flight-selenium-test

scenarios:
  week-destination-selenium-test:
      script: ../selenium/DestinationOfTheWeekTests.java
      additional-classpath:
      - out/artifacts/TaurusTestProject/TaurusTestProject.jar
  flight-information-selenium-test:
      script: ../selenium/FlightInformationTests.java
      additional-classpath:
      - out/artifacts/TaurusTestProject/TaurusTestProject.jar
  flights-information-selenium-test:
      script: ../selenium/FlightsInformationTests.java
      additional-classpath:
      - out/artifacts/TaurusTestProject/TaurusTestProject.jar
  flights-search-selenium-test:
      script: ../selenium/FlightsSearchTests.java
      additional-classpath:
      - out/artifacts/TaurusTestProject/TaurusTestProject.jar
  purchase-flight-selenium-test:
      script: ../selenium/PurchaseFlightTests.java
      additional-classpath:
      - out/artifacts/TaurusTestProject/TaurusTestProject.jar
```

This configuration is enough to run your scripts and to get some initial results:

![console4.png](console4.png)

No additional configuration is necessary. Even a simple specification of test scripts, as mentioned above, provides us with very detailed functional and performance tests results in real-time with the main metrics:

![console5.png](console5.png)

In addition to running existing test scenarios, the Taurus framework also allows us to create scenarios from scratch. Let’s use this feature and rewrite one Selenium and one JMeter test to get an idea of how it can be done:


### JMeter Config 

```yaml
execution:
- concurrency: 5
  ramp-up: 1m
  hold-for: 30s
  scenario: reach-home-page-and-make-reserv-jmeter-test

scenarios:
  reach-home-page-and-make-reserv-jmeter-test:
    store-cache: true
    store-cookie: true
    think-time: 1s500ms
    timeout: 500ms
    keepalive: true
    retrieve-resources: true
    concurrent-pool-size: 4
    use-dns-cache-mgr: true
    requests:
      - url: http://blazedemo.com/
        method: GET
        label: homepage
      - url: http://blazedemo.com/reserve.php
        method: POST
        label: reserve
        body:
          fromPort: Paris
          toPort: Buenos Aires
        assert:
          - contains:
            - .+Virgin America.+
            subject: body
            regexp: true
```

### Selenium Config

```yaml
execution:
- concurrency: 5
  ramp-up: 1m
  hold-for: 30s
  scenario: reach-home-page-selenium-test

scenarios:
  reach-home-page-selenium-test:
    browser: Firefox
    timeout: 10
    think-time: 1s500ms
    default-address: http://demo.blazemeter.com
    requests:
    - url: /
      assert:
      - contains:
        - blazemeter
        - Trusted
        subject: body
        regexp: false
```

Congratulations! You now know how to create scripts in Taurus from scratch, without direct use of Selenium and JMeter. This comes in handy when we need to create complicated tests, because it helps avoid redundant actions and adding unnecessary files.

Learn more about using Taurus from this free [webinar](http://info.blazemeter.com/automated-performance-tests-in-jenkins-ci-environments?utm_source=Blog&utm_medium=BM_Blog&utm_campaign=automated-performance-tests-jenkins-ci-environments?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium), and follow us for the upcoming articles, which will cover [analyzing test results](Reporting.md) and [integration with Jenkins](Jenkins.md).

The Taurus team is open for feedbacks and suggestions and you can be sure that you will get help for any issues via the [support forum](https://groups.google.com/forum/#!forum/codename-taurus).





