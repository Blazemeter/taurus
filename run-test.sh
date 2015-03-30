#!/bin/sh -xe

mkdir -p build

python -m nose tests.modules.test_JUnitXMLReporter --nocapture -v -x

# install it under virtualenv and try
#python setup.py sdist

