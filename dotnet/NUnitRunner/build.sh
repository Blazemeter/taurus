#!/bin/bash -xe

xbuild /p:Configuration=Release NUnitRunner.sln

cp NUnitRunner/bin/Release/* ../../bzt/resources/NUnitRunner/
