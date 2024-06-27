@echo off

set COMPILER_CLASSPATH=""
set GATLING_CLASSPATH=""

if DEFINED GATLING_HOME (
  set GATLING_PATH=%GATLING_HOME%/gatling
) else (
  set GATLING_PATH=%~dp0
)

python -u %GATLING_PATH%/gatling-fake.py %*
