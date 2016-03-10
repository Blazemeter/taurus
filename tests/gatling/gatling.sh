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
