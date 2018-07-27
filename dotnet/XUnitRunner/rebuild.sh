#!/bin/bash -xe
HERE=$(dirname $0)

pushd "$HERE"
dotnet clean
dotnet restore
dotnet build --output=bin
popd

cp $HERE/XUnitRunner/bin/* $HERE/../../bzt/resources/XUnitRunner/
