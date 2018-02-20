# How to Easily Run Taurus with Bamboo

_By: Yuri Bushnev, May 2017_

## Why Use Taurus with CI tools

[Continuous Integration (CI\)](https://www.blazemeter.com/blog/how-include-load-testing-your-continuous-integration-environment-0?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo) is a way to automate our tests and make the test process itself easier and more productive. With the help of CI it takes much less time to release, build, deploy and run tests automatically on a daily basis. At the same time, most performance testing tools are not well designed for a smooth CI pipeline integration. By using [Taurus](/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo), an open source automation testing framework, combined with some continuous integration tools like [Bamboo](https://www.atlassian.com/software/bamboo), we can achieve outstanding results in automation of our performance gates. 

## An Introduction to Taurus

Taurus is an open source automation framework that provides the ability to create test performance scripts from scratch or use existing scripts, and to configure run options. These options include many features that help execute and analyze script results.

The main idea behind the Taurus framework is that the script creation/configuration language is more user-friendly than options provided by other testing tools. Taurus, with its YAML-based syntax or JSON languages, helps to generate scripts in [Apache JMeter™](http://jmeter.apache.org/), [The Grinder](http://grinder.sourceforge.net/), [Gatling](http://gatling.io/) and additional tools.

There are also many additional advantages: 
- Very clear human readable scenarios 
- As a Taurus script is basically just a text code file, it is much easier to use under control versioning systems like git or svn
- Provides real-time reporting during test execution, which does not impact execution performance

The installation of Taurus is very straightforward and should take just a few minutes, by using [this guide](/install/Installation/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo). 

## An Introduction to Bamboo

Bamboo is a continuous integration server developed by Atlassian. It provides automated deployment, building and testing capabilities and built-in integration with other Atlassian products like Jira or Confluence. Because Jira is the most popular bug tracking tools, Bamboo is one of the most popular CI systems that is used in many companies over the world. Bamboo installation is very well covered in its [official documentation](https://confluence.atlassian.com/bamboo/installing-bamboo-on-linux-289276792.html).

## Running Taurus with Bamboo

Our workspace:
- Ubuntu 16.04
- Taurus v.1.9.1 
- Bamboo v.6.0.0

First of all, let’s create a simple [YAML](/docs/YAMLTutorial/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo) script named “script1.yml”

```yaml
execution:
- concurrency: 20
  hold-for: 1m
  ramp-up: 10s
  scenario: Thread Group
scenarios:
  Thread Group:
    requests:
    - label: HTTP Request BlazeDemo
      method: GET
      url: http://www.blazedemo.com/
```

As you can see, Taurus script files are human readable. In the provided example we simulate simple get requests to _**blazedemo.com**_ from 20 different users for 1 minute. Ramp-up means the onboarding time for all users, and the hold-for parameter means that the target load is held for 1 minute. Taurus uses [JMeter](http://www.blazemeter.com/jmeter-load-testing?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo) as a default test executor, but you can choose different [executors](/docs/ExecutionSettings/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo): [Selenium](/docs/Selenium/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo), [Gatling](/docs/Gatling/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo), [Locust](/docs/Locust/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo), etc.

To run that script using Taurus execute the following command:

```
bzt script1.yml
```

During test execution, you can see console real-time dashboard with lots of useful metrics:

![](bamboo1.png)

If there are any errors, we can see them in the right upper corner. In the middle of dashboard we can see such test parameters as latency, the requests’ response codes, errors, etc., which are necessary to know during and after the test run.

Pretty simple, isn’t it?

Now, what about Bamboo -

Bamboo should be installed on your computer and available on a URL ([http://localhost:8085/](http://localhost:8085/) in our case). If you open Bamboo for the first time, you will see the Start window.

![](bamboo2.png)

If you’re running Bamboo for the first time, click “Create your first build plan” button. Otherwise, click on the Bamboo main menu panel  “Create -> Create a new plan” to create a new test plan and follow steps below:

![](bamboo3.png)

Here we should specify our project parameters.

- Project - select project name [BlazeDemo]
- Plan name - type the name of your test plan [BlazeRequest]
- Plan description - a few words about what the purpose of our test plan
- Repository host - choose the repository you need for your test scripts. It might be repo on GitHub, BitBucket or other similar systems of version control. As this project is very simple, we don’t need any repo here [none]. 

After configuring these parameters, click the “Configure plan” button.

Next you’ll see a “Create a new plan” window:

![](bamboo4.png)

Here you should check the “Yes, please” CheckBox to make your test plan enabled, so you can configure and run it.

The next step - press “Add task” button to add new task to your test plan. You’ll see a window like this:

![](bamboo5.png)

Here the user is asked of what type of task should be used. In “Task types” window choose “Script” type.

![](bamboo6.png)

Mention the Task description and choose a command interpreter depending on our OS architecture (“Shell” for Linux, “Windows PowerShell” for Windows, “bin/sh or cmd.exe” as universal option).

In “Script body” field type:

```
bzt /{path_to_yml_script}/script1.yml
```
  
After this click “Add task” button and click on “Create” button to finish your test plan creation. 

![](bamboo7.png)

In the newly opened window click “Run” -> “Run plan” to execute our test plan. That’s it! Our performance test is up and running.

![](bamboo8.png)

## Reporting 

As soon as your test is finished you’ll see result performance metrics at the end of the log file in your finished build run.

![](bamboo9.png)

If you take a look at the mentioned log’s lines, you can find there are parameters as test duration, latency, samples count and response time. But, of course, reading metrics from logs is not the best option. That’s why Taurus provides better test analysis. Through Taurus, you can easily view, analyze and collect all performance metrics, through the Blazemeter app.

To do so and to see your tests results, change the command in the “Script body” field to:
```
bzt /{path_to_yml_script}/script1.yml -report
```

The “-report” command runs a [BlazeMeter](http://a.blazemeter.com/?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo) reporter. It uploads your test to the BlazeMeter app, which contains everything you need to get as much as possible from your performance metrics. It’s a sufficient reporting functionality that feeds results to the server, so you don’t need to provide any additional configuration. The report link is received in the console text. This link is automatically opened in the browser and you will see the web application with your test execution:

![](bamboo10.png)

[BlazeMeter reporting](https://www.blazemeter.com/blog/understanding-your-reports-part-4-how-read-your-load-testing-reports-blazemeter?utm_source=taurus&&utm_medium=KB&utm_campaign=taurus-bamboo) provides the ability to see various parameters of test running. You can find all necessary information about time (duration time, response time, latency, etc), loading and request stats, information about errors, response codes and many other helpful parameters that allow us to make our test analyze deeper and fuller.

That’s it! Now you are able to run Taurus YAML test scripts via the Bamboo CI tool.
