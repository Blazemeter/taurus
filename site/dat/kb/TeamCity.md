# Running Taurus with TeamCity
Taurus is an open source framework for performance tests automation. It can be used to run scripts from scratch as well as allows to run tests from other open source tools including JMeter, Gatling, Selenium or Locust. It hides complexity of existing automation tools and provides user-friendly and convenient wrapper. One of the main strength of Taurus is that it can be integrated with any of continuous integration (CI) servers in few steps. Usage of CI servers helps us to automate tests execution and avoid routine actions to run our scripts again and again. In this article we are going to find out how we can easily run our Taurus performance script in the TeamCity continuous integration server while you also can find tutorials for other CI solutions: [Jenkins](Jenkins) and [Bamboo](Bamboo).
First of all, let’s create our first Taurus performance script:

```yaml
execution:
- concurrency: 100
  ramp-up: 40s
  hold-for: 1m
  scenario: Open-BlazeDemo

scenarios:
  Open-BlazeDemo:
    requests:
      - url: http://blazedemo.com/
```

Taurus scripts can be written using JSON or YAML formats. As you can see, it is human readable and you don’t need to be a genius to find out that current script performs url requests to [http://blazedemo.com/](http://blazedemo.com/) web page using with 100 users during 1 minute while users coming onboard during first 40 seconds. If you have already installed Taurus framework (installation steps can be found [here](../docs/Installation)) then existing script can be easily run by such command:

```
bzt [path to the script]
```

Let’s save our created script with ‘BlazeDemoTest.yml’ and run the command:

```
bzt /Users/BlazeMeter/tests/BlazeDemoTest.yml
```

As soon as script has been started, we will real time performance metrics.

![](teamcity1.png)

Now we are ready to move created script into TeamCity integration server.
TeamCity is one of the most popular continuous integration servers developed by JetBrains team. It is a commercial software but open source projects can request a free license. The main advantage of this continuous integration server is that it provides very great usability out of the box. It has user friendly interface which makes it easy to use even for someone new to continuous integration solutions.
Let’s assume that you already have TeamCity integration server. If no, you can find straight forward installation steps on JetBrains [official website](https://confluence.jetbrains.com/display/TCD10/Installation).
First of all, we should create a new project.

![](teamcity2.png)

During project creation you should see few integration options. You can choose one of them depending on your needs: if you want to integrate source code from some custom repository, GitHub, Bitbucket or Visual Studio Team service. We are not going to cover these integrations and for our needs it is enough to use “Manually” option. In this case we just need to define some name for our project, unique project id and description (which is not mandatory).

![](teamcity3.png)

After project has been created you will be redirected to the main project page. Basically, project is a container for different build configurations while build configuration (build plan) is a sequence of execution steps to perform some specific job: deploy specific service, run some tests and so on. Therefore, if you want to run your tests in CI you need to create a separate build configuration:

![](teamcity4.png)

‘Create Build configuration’ page looks exactly the same as ‘Create Project’ and we need to define name, build configuration id and optional description:

![](teamcity5.png)

On the second page you can specify version control settings in case if you want to checkout performance script from source code repository. We are going to use performance script from local machine, that’s why you can just skip that step and click on “Create” button without specifying any additional settings.

![](teamcity6.png)

Each build configuration consists of build steps which provide logical separation.For example, we can have one build configuration to run smoke, integration and performance tests. In this case we will have one build configuration and 3 steps inside to run each type of tests accordingly.

![](teamcity7.png)

To execute our performance tests we need to use ‘Command Line’ runner with ‘Custom script’ run option. In custom script input we should specify the same command which we used before in command line to run our performance script.

![](teamcity8.png)

Click on ‘Save’ and we will see that the first build step was created. Now we are able to start our performance script in TeamCity.

![](teamcity9.png)

To run build configuration you need to use the button ‘Run’ in top right corner.

![](teamcity10.png)

On ‘Build Log’ tab you can see real-time logs coming from performance script execution.

![](teamcity11.png)

But when we will go through log file you can see that details are not human readable. Taurus use ASCII-art graphs to show real time dashboards while you run script in terminal. But the same ASCII-art graphs looks pretty ugly when you are trying just to use that in simple log files:

![](teamcity12.png)

Taurus provides number of [options how you can tune your reports](../docs/Reporting). First of all, it might be useful to turn off console graph report. To do that you can go back to your created build step and add command line argument *‘-o modules.console.disable=true’*:

![](teamcity13.png)

After that you will only reasonable information in a log file:

![](teamcity14.png)

Another great option provided by Taurus is integration with BlazeMeter reporting out of the box. We can use blazemeter reporter to upload our test results to BlazeMeter.com which provides great reporting and performance analytical functionality. For that we can add one more command line option. But this time we can use another way of reporting configuration. Instead of specifying command like parameters, we can open our script and add ‘reporting’ section with ‘blazemeter’ module parameters:

```yaml
execution:
- concurrency: 100
  ramp-up: 40s
  hold-for: 1m
  scenario: Open-BlazeDemo
scenarios:
  Open-BlazeDemo:
    requests:
      - url: http://blazedemo.com/
reporting:
- module: blazemeter
  report-name: TeamCity test report
  test: Taurus with TeamCity Demo
  project: Taurus with TeamCity
```

After we run our build plan again, our script will automatically initialize separate browser tab pointed to Blazemter application which is collecting statistics from build plan execution and show all useful metrics in real time:

![](teamcity15.png)

If for some reason browser with reporting was not initialized automatically, you can find the link in build execution logs:

![](teamcity16.png)

As a result of these straightforward steps with help of Taurus and TeamCity continuous integration server you can easily automate performance verification of your app and get detailed test reports in real-time.
