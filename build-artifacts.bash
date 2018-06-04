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

TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')
sed -ri "s/_TAURUS_VERSION_/_${TAURUS_VERSION}_/" site/dat/docs/Installation.md
mkdir -p site/msi
cp build/nsis/*.exe site/msi/

echo Flag is: $1