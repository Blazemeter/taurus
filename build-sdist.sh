#!/bin/bash -xe

TAURUS=$(dirname $0)
echo "Cleaning environment"
python3 ${TAURUS}/setup.py clean

echo "Building NUnit plugin"
pushd ${TAURUS}/dotnet/NUnitRunner
${TAURUS}/rebuild.sh
popd

echo "Building chrome-loader.exe"
rm -f ${TAURUS}/bzt/resources/chrome-loader.exe
x86_64-w64-mingw32-gcc -std=c99 -o ${TAURUS}/bzt/resources/chrome-loader.exe ${TAURUS}/bzt/resources/chrome-loader.c

echo "Getting build info"
VERSION=$(git describe --tags $(git rev-list --tags --max-count=1))
GIT_INFO="$(git rev-parse --abbrev-ref HEAD) $(git show --oneline -s)"
echo "VERSION=\"$VERSION\"" > ${TAURUS}/bzt/resources/version/version.py
echo "GIT_INFO=\"$GIT_INFO\"" > ${TAURUS}/bzt/resources/version/git_info.py

echo "Creating distribution packages"
python3 ${TAURUS}/setup.py sdist bdist_wheel
