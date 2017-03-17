#!/bin/sh -xe
echo Emulated!

KPI=`cat $2 | grep kpi.txt | cut -d\" -f2`
echo ${KPI}
echo "0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\n" > ${KPI}

cat $[KPI}