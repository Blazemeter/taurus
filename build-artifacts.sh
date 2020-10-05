#!/bin/bash -xe
BUILD_NUMBER=$1

# # build source distribution
# ./build-sdist.sh

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl
