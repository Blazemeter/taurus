echo off

set JMETER_PATH=%~dp0
python %JMETER_PATH%/jmeter-fake.py %*
