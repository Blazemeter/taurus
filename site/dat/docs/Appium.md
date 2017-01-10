# Appium Executor

## About
[Appium](http://appium.io) is a tool for testing naitive mobile applications.
Taurus supports only python scripts for appium. Additionally, you can use taurus services to run [Appium server](Services.md#Appium-Loader) and [Android emulator](Services.md#Android-Emulator-Loader).
There is typical example of usage:
 
```yaml
---
execution:
- executor: appium
  scenario: ap_scen

scenarios:
  ap_scen:
    script: test_appium_script.py

services:
- appium-loader
- android-emulator-loader

modules:
  android-emulator-loader:
    avd: android10_arm128
```  

