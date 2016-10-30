#!/bin/bash
set -x

curdir=$(dirname $0)

mvn package -Dmaven.test.skip.exec
cp "$curdir/target/taurus-testng-1.0.jar" "$curdir/../../bzt/resources/taurus-testng-1.0.jar"
