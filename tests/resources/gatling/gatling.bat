echo off
echo "Fake gatling output"
set QT=0
for /f %%i in ('dir /b %2\*.scala') do set /a QT+=1
if %QT% gtr 1 set /p temp="Choose a simulation number:"
echo "started..."

setlocal

set "DEFAULT_GATLING_HOME=FALSE"
if not defined GATLING_HOME set GATLING_HOME=%DEFAULT_GATLING_HOME%

set USER_ARGS=%*

rem set GATLING_HOME automatically if possible

set COMPILATION_CLASSPATH=""

echo %GATLING_HOME%
echo %COMPILATION_CLASSPATH%
echo %NO_PAUSE%
echo %JAVA_OPTS%
