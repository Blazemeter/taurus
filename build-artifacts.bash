#!/bin/bash -xe
BUILD_NUMBER=$2

apt-get install -y --force-yes gcc-mingw-w64-x86-64 nsis composer zip
pip install "pynsist<2"

# build source distribution
./build-sdist.sh

# build a windows installer
./build-windows-installer.sh ./dist/bzt-*.tar.gz


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
