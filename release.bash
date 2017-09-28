#! /bin/bash -xe

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

./build-sdist.sh

python $(dirname $0)/setup.py sdist bdist_wheel upload
