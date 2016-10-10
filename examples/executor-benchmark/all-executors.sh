#! /bin/sh -xe
for B in vu hits; do
  for E in locust gatling jmeter pbench grinder; do
    bzt scenario.yml exec-$B.yml service.yml -o execution.0.executor=$E -o modules.blazemeter.report-name="$B connect $E" -o scenarios.dummy.headers.Connection=close
    sleep 60
    bzt scenario.yml exec-$B.yml service.yml -o execution.0.executor=$E -o modules.blazemeter.report-name="$B keep-alive $E" -o scenarios.dummy.headers.Connection=keep-alive
    sleep 60
  done
done