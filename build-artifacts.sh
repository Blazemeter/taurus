#!/bin/bash -xe

PREV_DIR=`pwd`
cd "$(dirname $0)"

echo "Cleaning environment"
python3 setup.py clean

echo "Building NUnit plugin"
pushd ./dotnet/NUnitRunner
./rebuild.sh
popd

echo "Building chrome-loader.exe"
rm -f bzt/resources/chrome-loader.exe
x86_64-w64-mingw32-gcc -std=c99 -o bzt/resources/chrome-loader.exe bzt/resources/chrome-loader.c

echo "Creating distribution packages"
python3 ./setup.py sdist bdist_wheel

cd "${PREV_DIR}"

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl
