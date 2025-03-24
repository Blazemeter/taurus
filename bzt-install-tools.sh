#!/bin/bash

retry_count=5
image_version=$1

if [ "$image_version" == "full" ]
then
    tools=( "jmeter:5.6.3"
            "jmeter:5.5"
            "jmeter:5.4.3"
            "jmeter:5.4.2"
            "jmeter:5.3"
            "jmeter:5.2.1"
            "jmeter:5.1.1"
            "jmeter:5.1"
            "jmeter:5.0"
            "jmeter:4.0"
            "jmeter:3.1"
            "gatling:2.3.0"
            "gatling:2.2.3"
            "gatling:2.1.7" )
else
    tools=( "jmeter:5.6.3"
            "jmeter:5.5" )
fi

for tool in "${tools[@]}"
do
  name=${tool%%:*}
  version=${tool#*:}
  for i in {1..retry_count}
  do
    echo "bzt -install-tools -o modules.$name.version=$version"
    bzt -install-tools -o "modules.$name.version=$version" && break
  done
done
