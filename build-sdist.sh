#!/bin/bash -xe

echo "Cleaning environment"
python $(dirname $0)/setup.py clean

echo "Building NUnit plugin"
pushd $(dirname $0)/dotnet/NUnitRunner
./rebuild.sh
popd

echo "Creating distribution packages"
python $(dirname $0)/setup.py sdist bdist_wheel
