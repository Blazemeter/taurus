# Proxy2JMX Converter

It's possible to convert existing Selenium scripts into JMeter JMX file. Keep in mind: only requests will be converted, no assertions or other logic. 
For this purpose Taurus uses [BlazeMeter Recorder](https://guide.blazemeter.com/hc/en-us/articles/207420545-BlazeMeter-Recorder-Mobile-Recorder-) so you need valid token. This service starts proxy for logging requests and build jmx file based on the requests when test is finished. Lets see example config:
```yaml
---
execution:
- executor: selenium
  iterations: 1
  scenario: sel

scenarios:
  sel:
    script: example.java

services:
- module: proxy2jmx

modules:
  proxy2jmx:
    token: <your_token>

```

After execution it you'll find JMX in artifacts dir with the name `generated.jmx`.
As only Linux supports setting proxy through environment variables, you have set up another systems for work with proxy2jmx manually. Info about proxy parameters (port and address) you can find in your BlazeMeter account or in the Taurus log. 

## Proxy Server Auto Setup
Taurus helps you with setting of proxy server for recording purposes. This ability depends on your operation system.

### Linux 
Full support of Chrome and Firefox.

### Microsoft Windows
We provide support of Chrome browser at the moment. For correct work don't place your chromedriver inside of Windows directory.

### MacOS
Auto setup in MacOS is impossible.

<iframe width="700" height="394" src="https://www.youtube.com/embed/zuZkCHW259U" frameborder="0" allowfullscreen></iframe>
