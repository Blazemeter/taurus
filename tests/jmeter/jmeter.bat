echo off

if "%3"=="--version" (
  echo "Fake JMeter is installed"
  goto :eof
)
echo %JVM_ARGS%
python %TEST_SERVER_PATH%/udp-server.py

:eof