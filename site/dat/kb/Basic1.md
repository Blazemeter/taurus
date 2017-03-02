# How to Run JMeter Scripts in Taurus

_By: Guy Salton, BlazeMeter Inc._

## Why You Need Taurus for JMeter Scripts
JMeter is a very popular open source tool for testing your website or webapp. Although it’s quite powerful, it’s not very easy to use and its reporting abilities are very limited. Taurus is a free and open source framework for Continuous Testing which helps you by hiding the complexities of running performance tests. Think of it as an automation-friendly wrapper - it cloaks nicely around JMeter, neatly covering all of its complexities and imperfections. 

When running JMeter through Taurus you can:

 - Run an existing JMeter script
 - Create a new JMeter script very easily using a YAML text file
 - Automate your script with Jenkins

## Installation
To get started, first follow these [instructions to install Taurus](/docs/Installation/) on your computer. 
If there is no JMeter installed at the configured path, Taurus will attempt to install the latest JMeter and associated plugins into this location (by default this is: `~/.bzt/jmeter-taurus/bin/jmeter`). You can change this setting to your preferred JMeter location (consider putting it into the `~/.bzt-rc` file). 


## How to Run an Existing JMeter Script

Taurus provides different executors for many open-source testing tools like JMeter, Selenium, Gatling, Grinder, and more. The default executor is JMeter so if you’ve already created a JMX file with JMeter, it's very easy to run it with Taurus! Just use the bzt command followed by the JMX path. For example:

```bash
bzt example.jmx
```

In this case, `example.jmx` is the name of your JMeter test file.

This command will initiate JMeter and run the JMX file. After a few seconds, you’ll be able to see a full screen dashboard with all the Taurus reports. This dashboard features ASCII-art graphs displaying key statistics and KPIs about your test - all while your tests are still running. 


__Real-Time Load Test Results on the Taurus Dashboard__
![Console Screen](console2.png)


## Creating a JMeter Script Using YAML

If you aren’t familiar with JMeter, you can also use Taurus’ simple configuration syntax to create a test scenario as a YAML file. You can do this without even knowing or launching JMeter at all!  

Take a look at the following example:

```yaml
execution:
- concurrency: 5
  ramp-up: 20s
  hold-for: 2m
  scenario: TwoCheckout


scenarios:
  TwoCheckout:
    default-address: https://sandbox.2checkout.com/
    requests: 
      - /
      - /gimme-404
```

[See the full syntax dictionary here](/docs/JMeter/#Building-Test-Plan-from-Config) 


## Adding JMeter Properties 

Want to add some JMeter properties to your JMeter script? No problem!

There are two places to specify JMeter properties: 

### Global at module-level 

Global properties are set like this:

```yaml
modules:
  jmeter:
    properties:
      my-hostname: www.blazedemo.com
      log_level.JMeter: WARN
      log_level.JMeter.threads: DEBUG
    system-properties:
      org.apache.commons.logging.simplelog.log.org.apache.http: DEBUG
```
### Local at scenario-level

Scenario-level properties are set like this:
```yaml
execution:
- scenario: 
    properties:
        my-hostname: www.blazedemo.com
        log_level.JMeter.junit: DEBUG
```

You may also specify system properties for JMeter in the system-properties section. They come in the system.properties file in artifacts.


## Using BlazeMeter’s Reports

Another great advantage of running a JMeter script with Taurus is that you get access to BlazeMeter’s reporting service. While many of the testing tools focus on execution and less on reporting, this allows you to access test results in a convenient and interactive way, compare different executions, monitor trends over time, and collaborate with your colleagues. Plus you can use BlazeMeter’s reporting service even if you’re just running the ‘light’ free of charge version. 

The easiest way to do this is to simply use the `-report` command line switch. This will send your results to BlazeMeter’s reporting service - and you won’t need to set anything else up. You’ll receive the link for your report in the console text, and this will be automatically be opened in your default browser.

![bzm](blazemeter1.png)

## Scaling With Cloud Provisioning

One of the big disadvantages of running JMeter scripts on your local computer is that it’s not really scalable – you’re limited to the resources of your local computer. Taurus gives you the option of cloud provisioning – meaning you can run your JMX scripts on the cloud with your BlazeMeter account. Again, you don’t have to be a paying customer as free accounts can also execute cloud tests. To use cloud provisioning, you have to set the following:

```yaml
provisioning: cloud
```

To access the BlazeMeter cloud, Taurus needs to have API key set inside the cloud module settings. Like this:

```yaml
modules:
  cloud:
   token: ******* # API Key
   timeout: 7s   # BlazeMeter API client timeout
   browser-open: start    # auto-open browser on test start/end/both/none
```

Regarding the cloud locations, you can choose from all the locations provided by BlazeMeter (to get the list of available locations, run the `bzt -locations -o modules.cloud.token=<API Key>`).

If you specify multiple cloud locations for the same execution, Taurus will distribute the concurrency and/or throughput amongst the locations. In order to use cloud locations you have to specify a location and its relative weight - the relative weight determines the amount of concurrent users and throughput that will run in this location.

```yaml
execution:
- locations:
    us-west-1: 1
    us-central1-a: 2
```

## Automate Your Script With Jenkins

Continuous Delivery is becoming the ‘new normal’, especially for SaaS companies continually creating new features and improving their websites. While the software development world has been progressing very well in terms of process automation, the testing side of things was lagging behind.  This is where Taurus comes in.

Within Jenkins you can use the “Execute shell” section in the project configuration to run your Taurus test and make this a part of your continuous delivery cycle.


As always if you have any questions or comments just leave a message on the [project forums](/support/). 
