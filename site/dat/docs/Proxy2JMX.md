# Proxy2JMX Converter

It's possible to convert Selenium test into JMeter JMX file. For this purpose Taurus uses BlazeMeter recorder so you need valid token. Lets see example config:
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
