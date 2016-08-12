# Proxy2JMX Converter

It's possible to convert existing Selenium scripts into JMeter JMX file. For this purpose Taurus uses [BlazeMeter Recorder](https://guide.blazemeter.com/hc/en-us/articles/207420545-BlazeMeter-Recorder-Mobile-Recorder-) so you need valid token. This service starts proxy for logging requests and build jmx file based on the requests when test is finished. Lets see example config:
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

After execution it you'll find JMX in artifacts dir with the name of your scenario, for example above it will be `sel.jmx`.

<iframe width="700" height="394" src="https://www.youtube.com/embed/zuZkCHW259U" frameborder="0" allowfullscreen></iframe>
