#!/bin/bash -xe
BUILD_NUMBER=$2

CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"
echo "deb http://packages.cloud.google.com/apt ${CLOUD_SDK_REPO} main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -

apt-get update -y
apt-get install -y --force-yes gcc-mingw-w64-x86-64 nsis composer zip google-cloud-sdk 
apt-get install -y libssl-dev libncurses5-dev libsqlite3-dev libreadline-dev libtk8.5 libgdm-dev libdb4o-cil-dev libpcap-dev

gcloud auth activate-service-account --key-file ${KEY_FILE}
gcloud config set project blazemeter-taurus-website-prod

# build source distribution
./build-sdist.sh

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl

#if [ "$1" = "true" ]; then
./build-base-site.sh
./build-snapshot.sh
#else
#fi
