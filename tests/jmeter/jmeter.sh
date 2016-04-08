#!/bin/sh
if [ "$3" = "--version" ]; then
  echo "Fake JMeter is installed"
else
  echo "$JVM_ARGS"
  if [ -n "${TEST_SERVER_PATH}" ]; then
    python ${TEST_SERVER_PATH}/udp-server.py
  fi
fi