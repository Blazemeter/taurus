#!/bin/bash -xe

echo "Getting build info"
VERSION=$(git describe --tags $(git rev-list --tags --max-count=1))
GIT_INFO="$(git rev-parse --abbrev-ref HEAD) $(git show --oneline -s)"
if [ "$1" = "true" ]; then
    VERSION="${VERSION}.${BUILD_NUMBER}"
fi
echo "BUILD_NUM=\"$BUILD_NUMBER\"" > bzt/resources/version/build.py
echo "VERSION=\"$VERSION\"" > bzt/resources/version/version.py
echo "GIT_INFO=\"$GIT_INFO\"" > bzt/resources/version/gitinfo.py