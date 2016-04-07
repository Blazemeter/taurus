#!/bin/sh
if [ "$3" = "--version" ]; then
  echo "Fake JMeter is installed"
else
  echo "$JVM_ARGS"
  python ${TEST_SERVER_PATH}/udp-server.py
fi