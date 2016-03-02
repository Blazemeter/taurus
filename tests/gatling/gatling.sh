#!/bin/sh -xe
echo -e "Fake gatling output"
echo "dir:"
pwd

QT="`ls *scala | wc -l`"

if [ "$QT" -gt 1 ]; then
  echo "Choose a simulation number:"
  read
fi
echo -e "Done"
