#!/bin/bash -xe

echo "Cleaning environment"
python $(dirname $0)/setup.py clean

echo "Building NUnit plugin"
pushd $(dirname $0)/dotnet/NUnitRunner
$(dirname $0)/rebuild.sh
popd

echo "Building chrome-loader.exe"
rm -f $(dirname $0)/bzt/resources/chrome-loader.exe
x86_64-w64-mingw32-gcc -std=c99 -o $(dirname $0)/bzt/resources/chrome-loader.exe $(dirname $0)/bzt/resources/chrome-loader.c

echo "Creating distribution packages"
python $(dirname $0)/setup.py sdist bdist_wheel
