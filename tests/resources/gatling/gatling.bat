echo off

set COMPILATION_CLASSPATH=""

if DEFINED GATLING_HOME (
  set GATLING_DIR=%GATLING_HOME%/gatling
else (
  set GATLING_PATH=%~dp0
)

python -u %GATLING_PATH%/galing-fake.py %*
