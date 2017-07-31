#!/bin/bash
set -x

xbuild /p:Configuration=Release NUnitRunner.sln
cp NUnitRunner/bin/Release/* ../../bzt/resources/NUnitRunner/
