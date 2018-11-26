#!/bin/sh -xe
echo Emulated!

if [ "$#" -ne 1 ]; then
  echo "phantom is installed"
  exit 0
fi

KPI=`cat $2 | grep kpi.txt | cut -d\" -f2`
echo "0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\n" > $KPI

cat $KPI