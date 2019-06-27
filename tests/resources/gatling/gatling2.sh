#!/bin/sh

COMPILATION_CLASSPATH=

if [ -n "$GATLING_HOME" ]; then
  GATLING_DIR=${GATLING_HOME}/gatling
else
  GATLING_DIR=`dirname "$0"`
fi

JAVA="echo me"
bash -c "$JAVA"

GATLING_HOME="$GATLING_HOME" COMPILATION_CLASSPATH="$COMPILATION_CLASSPATH" python -u ${GATLING_DIR}/gatling-fake.py $@
