#!/bin/bash -xe
HERE=$(dirname $0)

nuget restore "$HERE/NUnitRunner/packages.config" -SolutionDirectory "$HERE" -Verbosity detailed
xbuild /target:clean "$HERE/NUnitRunner.sln"
xbuild /p:Configuration=Release "$HERE/NUnitRunner.sln"
cp $HERE/NUnitRunner/bin/Release/* $HERE/../../bzt/resources/NUnitRunner/
