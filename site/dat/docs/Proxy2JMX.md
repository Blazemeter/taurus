# Proxy2JMX Converter

It's possible to convert Selenium test into JMeter JMX file. For this purpose Taurus uses [BlazeMeter Recorder](https://guide.blazemeter.com/hc/en-us/articles/207420545-BlazeMeter-Recorder-Mobile-Recorder-) so you need valid token. This service starts proxy for logging requests and build jmx file based on the requests when test is finished. Lets see example config:
```yaml
---
execution:
- executor: selenium
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
After execution it you'll find generated.jmx in artifacts dir.
