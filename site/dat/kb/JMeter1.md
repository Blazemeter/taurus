# Solving JMeter Test Execution Problem


If you’re using JMeter to set up and execute your load tests on a regular basis, you’re no doubt aware of the dual importance and inconvenience of reporting with this tool.

Reporting is vital - you need immediate visual information on how your test is going. Without this prompt feedback, you could be waiting several hours before you realize that there’s a crucial problem (i.e. your script was hitting a misconfigured server the entire time). 

But JMeter best practices state that you should avoid using Listeners, which enable you to view sampler results, in your test plan for production-level runs due to excessive memory consumption. In fact, best practices state that you shouldn’t run a load test from the GUI mode at all. 

It leaves us with a deadlock:
 
<b>To understand the results of your load test, you need the User Interface (UI).</b>
 
BUT 
 
<b>To run your load test without exhausting your machine’s memory, you shouldn’t use the UI at all.</b>

This conflict between resources and functionality gives you a number of problems, especially when it comes to real-time test visualization and advanced analysis. In this article, I’m going to cover these issues and show you how to overcome them. 

##Gaps in Real-Time Test Visualization & Analysis in JMeter

Let’s say you run a JMeter script with a number of requests to a page, you execute the test, and run it for a few hours because you want to see how it performs over an extended period of time. JMeter won’t give you any automatic visual feedback, making it hard for you to understand how the page is performing. If you’re in the non UI mode (which is recommended when running a load test in JMeter) you can’t view anything. You’ll just get one single line showing you the test status every 30 seconds. 


### Why is ‘real-time’ visual feedback so important?

Without real-time feedback, it’s very hard to tell if you should stop the test. You could be running a test for three hours with invalid requests and you won’t know it - which is obviously a huge waste of time and resources. 

Of course, you can add some graphs and tables into your JMeter test plan so you have some information to view. By using [JMeter plugins](http://jmeter-plugins.org/), you can add KPIs like transactions per second, active threads over time, percentiles etc. However, there are a number of drawbacks to this approach:

1. <b>It takes a lot of time.</b> As there’s no ‘default’ reporting UI in JMeter, you’ll need to set it up from scratch _every single time_ you create a test plan. 
1. <b>You don’t have one simple place to look.</b> Even if you’ve set up all the individual reporting components like tables and graphs, you’ll need to jump from plugin to plugin to see if there are any errors, where the errors are etc. AND you can’t do any of this when you’re in non-UI mode (which is where you should be when running a load test in JMeter). The more KPIs you want to see, the more confusing this all becomes.
1. <b>It uses up too many resources.</b> The more reporting components you add, the more resources you use. 

## How to Get Test Visualization and Analysis While Using JMeter

The open source tool [Taurus](http://gettaurus.org) solves the conflict raised at the beginning of this post. Taurus allows you to execute a JMeter script in non-UI mode through the bzt command, and automatically gives you information on how your test is doing through its UI. 


Taurus’ default dashboard shows you key information like:

 - What’s happening right now
 - How much time has passed since the start of the test and how much time is left until the end of the test
 - Whether there are any errors in the test
 - The most important top level KPIs like percentiles, average response times, response codes etc. 
 - The health of load generator resources - so you can check that the generator won’t overload 

You can view all of these results in real time while running your test in JMeter. If you’re running JMeter with BlazeMeter, use the “-report” command line option to send all the reports to BlazeMeter in real-time.

![Console Screen](console.png)

## Running Post Test Analysis

Taurus also automatically saves reports in JMeter’s results files for further analysis. Once you’ve completed your test, you can then: 


1. Upload the files in the JMeter UI for advanced analysis after the test
1. Upload to [BlazeMeter Sense](http://sense.blazemeter.com/) test results’ storage to view interactive reports, and calculate statistics.
1. There are also several more [reporting](/docs/Reporting.md) options to integrate with Jenkins

----

So there is a simple solution to the seemingly impossible quandary posed by JMeter when it comes to reporting. With JMeter alone, you are stuck between your need to view real-time reports in the UI and the tool’s incompatibility with this requirement.  The open source tool Taurus resolves this problem by allowing you to execute your JMeter script in non-UI mode and presenting you with real-time reports on its UI. 
More Information?

If you have any questions or comments about any of the points raised in this article, please share them on our [support forums](/support/). 
