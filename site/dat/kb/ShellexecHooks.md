# Handling Pre/Post Test Actions of a Load Test
 
_By: Guy Salton, BlazeMeter Inc._
 
## How to Automate the Pre/Post Test Actions of Your Load Test

These days everybody in the performance testing world is talking about automation. Continuous Delivery is becoming the ‘new normal’, especially for SaaS companies continually creating new features and improving their websites.

Engineers are using Continuous Integration (CI) tools like Jenkins, Bamboo and TeamCity to integrate their tests to the software development lifecycle and trigger a load test on their web or mobile app with every code commit.

Despite all this automation,there are still manual actions being done before and after each test. In this post, we’ll  show you how to automate them!

A load test can be divided into 5 different steps:

1. Start Service
2. Data Preparation
3. Load Generation
4. Report Generation
5. Shutdown Service


## Separating the Pre and Post Test Actions from Load Generating

Now, let's say that in order to run your load test you need to download a CSV file (with the usernames and passwords to login to the app) from some location on the web, and maybe also randomize it. Maybe you want to email the test’s log to your manager once the test is completed? These actions shouldn’t be part of the load generation step!

These are classic pre-test/post-test actions and not part of the load, so by including them in your script, they will be counted as samples and will affect the average response time, latency and other KPIs. You don’t want this to happen. 

![jmeter screenshot](shellexec1.png)
 
 
## What About Test Automation?
 
We now understand why adding pre/post test actions to your load test script isn’t the right way, so…where should we put them??

It is always possible to run these actions manually before/after the load test execution but this is not a good practice as we would like to avoid any manual steps.

A pretty good way is to use the “Execute shell” section in the project configuration of the different CI tools that we mentioned above to execute commands before/after the test script

![jenkins screenshot](shellexec2.png)

But is this really the best way? If the test fails on of the  steps, you will have to debug and maintain your test in two places: the load generation part in your local computer and the pre/post test in Jenkins. What will happen when you want to run your test locally without Jenkins?
Let me show you an easier way to handle your pre/post test actions from a single configuration file.
 
Taurus is an open source tool that lets you run many open-source testing tools (such as JMeter, Gatling, The Grinder, Apache Benchmark, Locust.io and more) using YAML/JSON format. Taurus has a module called [“Shell Executor Service Module”](/docs/ShellExec/) that enables you to divide your test script into different logical steps we talked about and properly set your desired pre/post test actions in a single configuration file:
 
```yaml
services:
- module: shellexec
  prepare:  
  - mkdir tmp_guy
  - wget http://www.openss7.org/repos/csv_files/hosts.csv
  - sort -R hosts.csv > random_hosts.csv
  startup:
  - echo 1 > tmp_guy/bla.txt
  - echo 2 >> tmp_guy/bla.txt
  shutdown:
  - echo "shutdown time is $(date)" >> tmp/bla.txt
  post-process:
  - echo "this is a post process" >> tmp/bla.txt
  - mail -a tmp_guy/bzt.log -s “my_taurus_log_file“ user@example.com < tmp/bla.txt
  
execution:
- concurrency: 20
  hold-for: 60s
  iterations: 5
  ramp-up: 10s
  scenario: Thread Group
scenarios:
  Thread Group:
    data-sources:
    - delimiter: ','
      path: random_hosts.csv
      quoted: false
      recycle: true
    requests:
    - label: HTTP Request
      method: GET
      url: http://www.blazedemo.com/
    store-cache: false
    store-cookie: false
    use-dns-cache-mgr: false
```

This file contains the pre test actions in the “prepare” and “startup” steps, the post test actions in the “shutdown” and “post-process” steps and even the load generation step under “execution”. If you want, you can also separate the execution part to a different YAML file and keep it as a configuration file only. This way you maintain your pre and post test action in one simple YAML file, which you can use both locally and with a CI tool!
 
This YAML code will run JMeter (as this is the default executor) but you can specify the other executors under the “execution” section and use the same configuration file for different executors.

As always if you have any questions or comments, please message us on our [project forum](/support/).
 
 
