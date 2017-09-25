#!/bin/bash -xe

echo "Cleaning environment"
python $(dirname $0)/setup.py clean

echo "Building Grinder plugin"
pushd $(dirname $0)/java/grinder_logger
mvn clean package
cp target/*.jar ../../bzt/resources/
popd

echo "Building JUnit plugin"
pushd $(dirname $0)/java/JUnit_plugin
mvn clean package
cp target/*.jar ../../bzt/resources/
popd

echo "Building TestNG plugin"
pushd $(dirname $0)/java/TestNG_plugin
mvn clean package
cp target/*.jar ../../bzt/resources/
popd

echo "Building NUnit plugin"
pushd $(dirname $0)/dotnet/NUnitRunner
./rebuild.sh
popd

echo "Creating source distribution"
python $(dirname $0)/setup.py sdist
