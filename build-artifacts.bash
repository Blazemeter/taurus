#!/bin/bash -xe
BUILD_NUMBER="$2"
CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"
echo "deb http://packages.cloud.google.com/apt ${CLOUD_SDK_REPO} main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -

apt-get update -y
apt-get install -y --force-yes gcc-mingw-w64-x86-64 nsis composer zip google-cloud-sdk
apt-get install -y --force-yes libssl-dev libncurses5-dev libsqlite3-dev libreadline-dev libtk8.5 libgdm-dev libdb4o-cil-dev libpcap-dev

# build source distribution
./build-sdist.sh

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl


if [ "$1" = "false" ]; then
    cp -r site/dat/kb ./
    rm -r site

    mkdir -p site/snapshots
    cp dist/*.tar.gz site/snapshots
    wget -P site/snapshots https://s3.amazonaws.com/deployment.blazemeter.com/jobs/taurus-pbench/10/blazemeter-pbench-extras_0.1.10.1_amd64.deb
    cp build/nsis/*${BUILD_NUMBER}*.exe site/snapshots

    mkdir -p site/dat
    mv ./kb site/dat/
else
    cd site
    composer update --prefer-stable --no-dev
    cp vendor/undera/pwe/.htaccess ./
    cd ..

    TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')
    sed -ri "s/_TAURUS_VERSION_/_${TAURUS_VERSION}_/" site/dat/docs/Installation.md
    mkdir -p site/msi
    cp build/nsis/*.exe site/msi/

    python site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md
    cp site/dat/docs/img/*.png site/img/
fi

cd site
zip -r site.zip *
cd ..
