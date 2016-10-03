#! /bin/sh -xe
for E in gatling jmeter pbench; do
    echo Running $E

    bzt scenario.yml exec-hits.yml service.yml -o execution.0.executor=$E -o modules.blazemeter.report-name="hits keep-alive $E" -o scenarios.dummy.keepalive=true
    echo Cooldown for 1m...
    sleep 60

    bzt scenario.yml exec-hits.yml service.yml -o execution.0.executor=$E -o modules.blazemeter.report-name="hits connect $E" -o scenarios.dummy.keepalive=false
    echo Cooldown for 1m...
    sleep 60
done