#! /bin/sh -xe
for B in vu hits; do
  for E in gatling jmeter pbench; do
    bzt scenario.yml exec-$B.yml service.yml -o execution.0.executor=$E -o modules.blazemeter.report-name="$B keep-alive $E" -o scenarios.dummy.keepalive=true
    echo Cooldown for 1m...
    sleep 60

    bzt scenario.yml exec-$B.yml service.yml -o execution.0.executor=$E -o modules.blazemeter.report-name="$B connect $E" -o scenarios.dummy.keepalive=false
    echo Cooldown for 1m...
    sleep 60
  done
done