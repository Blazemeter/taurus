#!/bin/bash -xe
apt-get install -y --force-yes gcc-mingw-w64-x86-64 nsis composer
pip install "pynsist<2"

# build source distribution
./build-sdist.sh

# build a windows installer
./build-windows-installer.sh ./dist/bzt-*.tar.gz

cd site
composer update --prefer-stable --no-dev
cp vendor/undera/pwe/.htaccess ./
cd ..

python site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md
cp site/dat/docs/img/*.png site/img/
