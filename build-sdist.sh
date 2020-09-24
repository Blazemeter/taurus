#!/bin/bash -xe

PREV_DIR=`pwd`
cd "$(dirname $0)"

echo "Getting build info"
VERSION=$(git describe --tags $(git rev-list --tags --max-count=1))
GIT_INFO="$(git rev-parse --abbrev-ref HEAD) $(git show --oneline -s)"
echo "VERSION=\"$VERSION\"" > bzt/resources/version/version.py
echo "GIT_INFO=\"$GIT_INFO\"" > bzt/resources/version/gitinfo.py

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