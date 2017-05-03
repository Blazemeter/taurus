#!/bin/sh
JMETER_DIR=`dirname "$0"`
python ${JMETER_DIR}/jmeter-fake.py $@
