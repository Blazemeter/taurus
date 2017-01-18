# Integrating Taurus with Jenkins

_Author: Iurii Bushnev_

Welcome to part 3 of our “Continuous Functional and Performance Test Automation with Selenium, JMeter and Taurus” series. In the previous articles, we covered [how to create Taurus scripts](/kb/Scripting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) from scratch without direct use of Selenium and JMeter, and [how to create and read reports](/kb/Reporting/?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) on Taurus. In this article, we will go over how to integrate Taurus with Jenkins. In this article, we will explain how to put our Selenium and JMeter tests that are run by Taurus, in the Continuous Integration cycle.

[Jenkins](https://www.blazemeter.com/jenkins?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) is the must-have solution for those who want to have free open source continuous integration server and control process of project build and tests execution on daily bases. You probably have Jenkins on your computer, but if you don’t, install it [from here](https://jenkins.io/).

Jenkins usually runs under a default user called ‘Jenkins’. Since Taurus creates temporary data and also different reports, you might run into issues if Taurus doesn’t have writing permissions for its working directory.

Therefore, as a second step change the Jenkins default user. Here’s how:
- Linux: [http://blog.manula.org/2013/03/running-jenkins-under-different-user-in.html](http://blog.manula.org/2013/03/running-jenkins-under-different-user-in.html)
- Mac: [http://stackoverflow.com/questions/24092295/change-the-jenkins-default-user-user-name-on-mac](http://stackoverflow.com/questions/24092295/change-the-jenkins-default-user-user-name-on-mac)
- Windows: [http://antagonisticpleiotropy.blogspot.fi/2012/08/running-jenkins-in-windows-with-regular.html](http://antagonisticpleiotropy.blogspot.fi/2012/08/running-jenkins-in-windows-with-regular.html)

Third, install the [Jenkins Plot Plugin](https://wiki.jenkins-ci.org/display/JENKINS/Plot+Plugin). It will be useful for comparing your basic performance metrics for the last builds. To install the plugin go to the ‘Manage Jenkins’ section and open the ‘Manage Plugins’ menu entry. Then, go to the ‘Available’ tab and find ‘Plot Plugin’ in the list below. 

Now let’s move to building a plan creation. To configure Jenkins do the following:


In the build section run your YAML script:

![](jenkins1.png)

You can also add post build action for plots creation. It is better to configure it this way, since the ‘timeStamp’ produces very large values and that makes its general graph user-unfriendly.

![](jenkins2.png)

Add post build action for JUnit tests result statistic:

![](jenkins3.png)

After the build execution, you will be able to see the tests results trend graph that is related to the JUnit tests report:

![](jenkins4.png)

There is also an additional ‘Plots’ section on the left menu that provides you with the final Taurus statistics for a configurable number of builds.

At the same time, you can analyze all your metrics on [CA BlazeMeter](/docs/Reporting/#BlazeMeter-Reporter?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium), for a detailed investigation of the product health, trends and [KPI](https://www.blazemeter.com/blog/understanding-your-reports-part-4-how-read-your-load-testing-reports-blazemeter?utm_source=taurus&utm_medium=KB&utm_campaign=JMeterSelenium) performance.


Congratulations! You now know how to run Taurus tests in the Continuous Integration cycle with Jenkins.

The Taurus team is open for feedbacks and suggestions and you can be sure that you will get help for any issues via the [support forum](https://groups.google.com/forum/#!forum/codename-taurus).
 

