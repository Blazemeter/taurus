#!/bin/bash -xe

echo "Getting build info"
VERSION=$(git describe --tags $(git rev-list --tags --max-count=1))
GIT_INFO="$(git branch --show-current) $(git show --oneline -s)"
echo "BUILD=\"${BUILD_NUMBER} $(date)\"" > bzt/resources/version/build.py
echo "VERSION=\"${VERSION}\"" > bzt/resources/version/version.py
echo "GIT_INFO=\"${GIT_INFO}\"" > bzt/resources/version/gitinfo.py
