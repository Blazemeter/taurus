#! /bin/bash -xe

echo "Building JUnit plugin"
pushd $(dirname $0)/java/JUnit_plugin
mvn clean package
cp target/*.jar $(dirname $0)/../../bzt/resources/
popd

echo "Building TestNG plugin"
pushd $(dirname $0)/java/TestNG_plugin
mvn clean package
cp target/*.jar $(dirname $0)/../../bzt/resources/
popd

$(dirname $0)/build-sdist.sh

python $(dirname $0)/setup.py sdist bdist_wheel upload
