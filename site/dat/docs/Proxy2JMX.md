# Proxy2JMX Converter

It's possible to convert existing Selenium scripts into JMeter JMX file. Keep in mind: only requests will be converted, no assertions or other logic. 
For this purpose Taurus uses [BlazeMeter Recorder](https://guide.blazemeter.com/hc/en-us/articles/207420545-BlazeMeter-Recorder-Mobile-Recorder-) so you need valid token. This service starts proxy for logging requests and build jmx file based on the requests when test is finished. You will need [BlazeMeter API key configured](BlazemeterReporter/#Personalized-Usage) for this approach to work. Lets see example config:

```yaml
execution:
- executor: selenium
  iterations: 1
  scenario: sel

scenarios:
  sel:
    script: example.java

services:
- module: proxy2jmx
```

As soon as taurus completes its work you'll find JMX in artifacts dir with the name `generated.jmx`.

## Proxy Server Auto Setup
Taurus can help you with settings of proxy server for recording purposes. This ability depends on your operation system.

### Linux 
Full support of Chrome and Firefox.

### Microsoft Windows
We provide support of Chrome browser at the moment. For correct work of proxy you have to prepare the right place
for chromedriver (don't place your chromedriver inside Windows directory). We strongly recommend next way:
1. create directory (e.g. "c:\chromedriver")
2. put chromedriver.exe into created directory and remove all another copies of chromedriver
3. add directory to path:
3.1 go to `Control Panel` -> `System and Security` -> `System`
3.2 `Advanced system settings` -> `Environment Variables`
3.3 in the `System Variables` area locate the `Path` variable, highlight it and click `Edit`
3.4 add your path ("c:\chromedriver") to previous value (and don't forget about path separator `;`)
3.5 save changes.

### MacOS
Auto setup in MacOS is currently not implemented.

<iframe width="700" height="394" src="https://www.youtube.com/embed/zuZkCHW259U" frameborder="0" allowfullscreen></iframe>
