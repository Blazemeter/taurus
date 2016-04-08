echo off

if "%3"=="--version" (
  echo "Fake JMeter is installed"
  goto :eof
)
echo %JVM_ARGS%

if NOT "%TEST_SERVER_PATH%"=="" python %TEST_SERVER_PATH%/udp-server.py

:eof