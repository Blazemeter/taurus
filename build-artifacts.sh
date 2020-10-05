#!/bin/bash -xe

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl
