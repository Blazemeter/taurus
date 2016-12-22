#!/bin/sh
echo "Fake gatling output"
echo "dir:"
pwd

QT="`ls $2/*scala | wc -l`"

if [ "$QT" -gt 1 ]; then
  echo "Choose a simulation number:"
  read tmp_var
fi
echo "started..."

DEFAULT_GATLING_HOME="False"

GATLING_HOME="${GATLING_HOME:=${DEFAULT_GATLING_HOME}}"

JAVA_OPTS="-server -XX:+UseThreadPriorities -XX:ThreadPriorityPolicy=42 -Xms512M -Xmx512M -Xmn100M -XX:+HeapDumpOnOutOfMemoryError -XX:+AggressiveOpts -XX:+OptimizeStringConcat -XX:+UseFastAccessorMethods -XX:+UseParNewGC -XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled -Djava.net.preferIPv4Stack=true -Djava.net.preferIPv6Addresses=false ${JAVA_OPTS}"

# Build compilation classpath
COMPILATION_CLASSPATH=`find $GATLING_HOME/lib -maxdepth 1 -name "*.jar" -type f -exec printf :{} ';'`

echo ${GATLING_HOME}
echo ${COMPILATION_CLASSPATH}
echo ${NO_PAUSE}
echo ${JAVA_OPTS}