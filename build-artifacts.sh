#!/bin/bash -xe

PREV_DIR=$(pwd)
cd "$(dirname $0)"

echo "Cleaning environment"
rm -rf ./build

echo "Building chrome-loader.exe"
rm -f bzt/resources/chrome-loader.exe
x86_64-w64-mingw32-gcc -std=c99 -o bzt/resources/chrome-loader.exe bzt/resources/chrome-loader.c

echo "Creating distribution packages"
rm -rf ./dist
python3 -m build

cd "${PREV_DIR}"
