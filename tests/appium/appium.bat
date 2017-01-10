echo off

set APPIUM_PATH=%~dp0
python %APPIUM_PATH%/appium.py %*
