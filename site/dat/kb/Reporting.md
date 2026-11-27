# Reporting and Results Analysis on Taurus

_Author: Iurii Bushnev_

Welcome to part 2 of our “Continuous Functional and Performance Test Automation with [Selenium](https://www.blazemeter.com/blog/how-automate-testing-using-selenium-webdriver-jenkins-and-allure?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium), [JMeter](https://www.blazemeter.com/jmeter-load-testing?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) and [Taurus](/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium)” series. In the [previous article](/kb/Scripting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium), we covered how to create Taurus scripts from scratch, without direct use of Selenium and JMeter. In this article, we will go over how to view and analyze reports.

Taurus is a tool that provides a simple way to create and run performance tests, as well as an easy integration with additional open-source functional and performance testing software, like Selenium, JUnit, [Gatling](https://gatling.io/) or JMeter.

Taurus also provides great ways for results reporting and analysis. The default way is [Console Reporting](/docs/Reporting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium#Console-Reporter). With console reporting, you can just run your test. The results are detailed and even more importantly - they are also in real-time. The metrics are self-explanatory and provide lots of information.

![](console6.png)

Another option is [final statistics reporting](/docs/Reporting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium#Final-Stats-Reporter). This reporting provides the basic metrics in the console after test execution. This report can also be tuned according to your needs. For example, you can specify a ‘dump-csv’ file for creating graphs by using Jenkins. In that case, the reporting section will look like this:

```yaml
reporting:
- module: console
- module: final-stats
  summary: true  # overall samples count and percent of failures
  percentiles: true  # display average times and percentiles
  failed-labels: false  # provides list of sample labels with failures
  test-duration: true  # provides test duration
  dump-csv: TaurusResult/perf_result_csv.csv
```

The results produced by the test execution will look like this:

![](console7.png)

Since Taurus also covers functional tests with Selenium, we recommend you provide pass-fail statistics. Taurus currently supports the ability to generate the [JUnit xml result file](/docs/Reporting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium#JUnit-XML-Reporter). Later, this result xml file can be used in Jenkins to provide functional tests statistics. In this case, we need an additional module into the reporting sections:

```yaml
reporting:
- module: console
- module: final-stats
  summary: true
  percentiles: true
  failed-labels: false   
  test-duration: true
  dump-csv: TaurusResult/perf_result_csv.csv
- module: junit-xml
  data-source: pass-fail
  filename: TaurusResult/junit-result.xml
```

To view results in a web dashboard, use the [BlazeMeter reporter](/docs/Reporting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium#BlazeMeter-Reporter). In your BlazeMeter account, [create an API key](https://help.blazemeter.com/docs/guide/api-blazemeter-api-keys.html) under **Settings → Personal → API Keys** and copy the key ID and secret.

Join the key ID and secret with a colon and set them in your per-user configuration file, `~/.bzt-rc`, to keep the token out of the test YAML. Taurus can copy that file into its artifacts directory. The example below disables artifact uploads while still reporting test results. Remove files containing credentials before sharing local artifacts.

```yaml
modules:
  blazemeter:
    token: "<key-id>:<key-secret>"
```

After that, add a module under the reporting section with appropriate information. Your reporting section will look like this:

```yaml
reporting:
- module: console
- module: final-stats
  summary: true
  percentiles: true 
  failed-labels: false
  test-duration: true
  dump-csv: TaurusResult/perf_result_csv.csv
- module: junit-xml
  data-source: pass-fail
  filename: TaurusResult/junit-result.xml
- module: blazemeter
  report-name: Taurus test report
  test: Taurus test
  project: Taurus test project
  upload-artifacts: false
```

Taurus prints a link to the report in the console. Use it to view your results in BlazeMeter.

![](blazemeter2.png)

In BlazeMeter, you can create and analyze graphs of performance metrics for each test script:

![](blazemeter3.png)

We also recommend you specify the locations of artifacts into your YAML file:

```yaml
settings:
  artifacts-dir: TaurusResult
```

We will need to go through artifacts if we have issues, like failures in our Selenium tests. These issues will be immediately shown in our console reporting (red entries):

![](console8.png)

Then we can go into the artifacts folder and find junit logs in ‘junit.err’ and ‘junit.out’ files, as well as Selenium logs in the ‘selenium_tests_report.ldjson’ file.

![](selenium-ldjson.png)

Congratulations! You now know how to create and read Taurus analytics reports. Stay with us for the next article, about [integrating Taurus with Jenkins](Jenkins.md).

The Taurus team is open for feedback and suggestions, ask for help with any issues via [GitHub Issues](https://github.com/Blazemeter/taurus/issues).