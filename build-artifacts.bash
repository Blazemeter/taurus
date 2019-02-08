#!/bin/bash -xe
BUILD_NUMBER=$1

apt-get update -y
apt-get install -y --force-yes gcc-mingw-w64-x86-64 nsis composer zip
apt-get install -y libssl-dev libncurses5-dev libsqlite3-dev libreadline-dev libtk8.5 libgdm-dev libdb4o-cil-dev libpcap-dev

#gcloud auth activate-service-account --key-file ${KEY_FILE}
#gcloud config set project ${PROJECT_ID}
#gcloud config set compute/zone us-central1-a

# build source distribution
./build-sdist.sh

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl


#PROJECT_ID="blazemeter-taurus-website-prod"
#if [ "$1" = "true" ]; then
#./build-base-site.sh
#fi

#./build-snapshot-site.sh
#./deploy-site.sh